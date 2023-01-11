#include "URL.h"
#include "Compression/Compression.h"

#define FREEARC_USER_AGENT "FreeArc/0.67"

// ****************************************************************************
// ** URL reading library
// ****************************************************************************

#ifdef FREEARC_NOURL

void url_setup_proxy(char *_proxy) {}
void url_setup_bypass_list(char *_bypass_list) {}
URL *url_open(char *_url) { return NULL; }
int url_readp(URL *url, int64 offset, char *buf, int size) { return -1; }
void url_close(URL *url) {}

#else

char *proxy = NULL;
void  url_setup_proxy (char *_proxy)
{
    proxy = strequ(_proxy,"--") ? NULL : strdup_msg (_proxy);
}

void  url_setup_bypass_list (char *_bypass_list)
{
}

static size_t NoWriteCallback(void *ptr, size_t size, size_t nmemb, void *data)
{
    return -1;
}

static size_t WriteMemoryCallback(void *ptr, size_t size, size_t nmemb, void *data)
{
    char **buf = (char **) data;
    memcpy (*buf, ptr, size*nmemb);
    *buf += size*nmemb;
    return size*nmemb;
}

URL* url_open (char *_url)
{
    static bool curl_inited = FALSE;
    if (!curl_inited)  curl_global_init(CURL_GLOBAL_ALL), curl_inited=TRUE;

    URL *url = (URL*) malloc(sizeof(URL));
    if (!url)  return NULL;
    url->url      = strdup_msg (_url);
    url->curpos   = 0;

    /* init the curl session */
    url->curl_handle = curl_easy_init();

    /* set proxy if it was specified by user */
    if (proxy)  curl_easy_setopt (url->curl_handle, CURLOPT_PROXY, proxy);

    /* some servers don't like requests that are made without a user-agent
       field, so we provide one */
    curl_easy_setopt (url->curl_handle, CURLOPT_USERAGENT, FREEARC_USER_AGENT);

    /* specify URL to get */
    curl_easy_setopt (url->curl_handle, CURLOPT_URL, url->url);

    // Get file size
    curl_easy_setopt (url->curl_handle, CURLOPT_WRITEFUNCTION, NoWriteCallback);
    CURLcode res = curl_easy_perform (url->curl_handle);
    if (CURLE_OK!=res && CURLE_WRITE_ERROR!=res)   {url_close(url); return NULL;}

    long response;
    curl_easy_getinfo (url->curl_handle, CURLINFO_RESPONSE_CODE, &response);
    if (response!=200 && response!=301 && response!=302)   {url_close(url); return NULL;}

    double size;
    res = curl_easy_getinfo (url->curl_handle, CURLINFO_CONTENT_LENGTH_DOWNLOAD, &size);
    if (CURLE_OK != res)   {url_close(url); return NULL;}
    url->size = (int64)size;

    /* send all data read to this function  */
    curl_easy_setopt (url->curl_handle, CURLOPT_WRITEFUNCTION, WriteMemoryCallback);

    return url;
}

// Check existence of given url
int url_exists (char *_url)
{
    URL *url = url_open(_url);
    url_close(url);
    return url!=NULL;
}

int url_readp (URL *url, int64 offset, char *buf, int size)
{
    if (size==0)  return 0;
    if (!url)     return -1;

    /* specify byte range to get */
    char Range[100];
    sprintf (Range, "%.0lf-%.0lf", double(offset), double(offset+size-1));
    curl_easy_setopt (url->curl_handle, CURLOPT_RANGE, Range);

    /* we pass address of 'current write pointer' variable to the callback function */
    char *ptr = buf;
    curl_easy_setopt (url->curl_handle, CURLOPT_WRITEDATA, (void *)&ptr);

    /* get it! */
    curl_easy_perform (url->curl_handle);

    return ptr-buf;
}

void url_reset (URL *url)
{
}

void url_close (URL *url)
{
    if (!url) return;
    /* cleanup curl stuff */
    curl_easy_cleanup (url->curl_handle);

    free (url->url);
    free (url);
}

#endif


int64 url_size (URL *url)                      {return url? url->size   : 0;}
int64 url_pos  (URL *url)                      {return url? url->curpos : 0;}
void  url_seek (URL *url, int64 newpos)        {if (url)  url->curpos = newpos, url_reset(url);}
int   url_read (URL *url, char *buf, int size) {return url? url_readp (url, url->curpos, buf, size) : -1;}
