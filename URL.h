#include "Compression/Common.h"

#ifdef __cplusplus
extern "C" {
#endif

#include <curl/curl.h>
typedef struct {
  char *url;
  int64 size;
  int64 curpos;
  CURL *curl_handle;
} URL;

void url_setup_proxy(char *_proxy);
void url_setup_bypass_list(char *_bypass_list);
URL *url_open(char *_url);
int url_exists(char *_url);
int64 url_size(URL *url);
int64 url_pos(URL *url);
void url_seek(URL *url, int64 newpos);
int url_read(URL *url, char *buf, int size);
void url_reset(URL *url);
void url_close(URL *url);

#ifdef __cplusplus
}
#endif
