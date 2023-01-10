#define _WIN32_WINNT 0x0501
#include <stdio.h>
#include <sys/stat.h>
#include <utime.h>
#include <limits.h>
#include <memory.h>
#include "Environment.h"
#include "Compression/Compression.h"

// Изменим настройки RTS, включив compacting GC начиная с 40 mb:
char *ghc_rts_opts = "-c1 -M4000m -K80m                       ";


/* ********************************************************************************************************
*  FreeArc.dll
***********************************************************************************************************/

#ifdef FREEARC_DLL
#include "Compression/_TABI/tabi.h"

extern "C" {
#include "HsFFI.h"
#include "Arc_stub.h"
void __stginit_Main();

BOOL APIENTRY DllMain (HANDLE hModule, DWORD  ul_reason_for_call, LPVOID lpReserved)
{
   return TRUE;
}

enum { MESSAGE_TOTAL_FILES=1
     , MESSAGE_TOTAL_ORIGINAL
     , MESSAGE_PROGRESS_ORIGINAL
     , MESSAGE_PROGRESS_COMPRESSED
     , MESSAGE_PROGRESS_MESSAGE
     , MESSAGE_PROGRESS_FILENAME
     , MESSAGE_WARNING_MESSAGE
     , MESSAGE_VOLUME_FILENAME
     , MESSAGE_CAN_BE_EXTRACTED

     , MESSAGE_PASSWORD_BUF
     , MESSAGE_PASSWORD_SIZE
     , MESSAGE_ARCINFO_TYPE
     , MESSAGE_ARCINFO_FILES
     , MESSAGE_ITEMINFO
     , MESSAGE_ARCINFO_NAME
     };

struct ItemInfo
{
    wchar_t *diskname;    // Filename on disk (only for "can_be_extracted?" request)
    wchar_t *filename;    // Filename of this item
    int64 original;       // Bytes, uncompressed
    int64 compressed;     // Bytes, compressed
    uint  time;           // Datetime stamp
    uint  attr;           // DOS attributes
    uint  is_folder;
    uint  crc;            // CRC32
    uint  is_encrypted;
};


typedef int CallBackFunc (WPARAM wParam, LPARAM lParam);
CallBackFunc *FreeArcCallback;

#define Send(msg,param) FreeArcCallback(msg, (LPARAM) (param))


// Function called by dotnet_FreeArcExecute() to display progress indicator and interact with user
int Callback_for_FreeArcExecute (TABI_ELEMENT* params)
{
    TABI_MAP p(params);
    char *request = p._str("request");   // Operation requested from callback
    if (strequ(request, "total"))
    {
        // Общий объём файлов, которые предстоит упаковать/распаковать
        int64 files      = p._longlong("files");          // Number of files
        int64 original   = p._longlong("original");       // Total size of files
        Send (MESSAGE_TOTAL_FILES,    &files);
        Send (MESSAGE_TOTAL_ORIGINAL, &original);
    }
    else if (strequ(request, "progress"))
    {
        // Информируем пользователя о ходе упаковки/распаковки
        int64 original   = p._longlong("original");       // Bytes, uncompressed
        int64 compressed = p._longlong("compressed");     // Bytes, compressed
        Send (MESSAGE_PROGRESS_ORIGINAL,   &original);
        Send (MESSAGE_PROGRESS_COMPRESSED, &compressed);
    }
    else if (strequ(request, "file"))
    {
        // Начало упаковки/распаковки нового файла
        wchar_t *message   = p._wstr("message");
        wchar_t *filename  = p._wstr("filename");
        Send (MESSAGE_PROGRESS_MESSAGE,  message);
        Send (MESSAGE_PROGRESS_FILENAME, filename);
    }
    else if (strequ(request, "warning") || strequ(request, "error"))
    {
        // Сообщшение о [не]критической ошибке
        wchar_t *message   = p._wstr("message");
        Send (MESSAGE_WARNING_MESSAGE, message);
    }
    else if (strequ(request, "volume"))
    {
        // Начало нового тома
        wchar_t *filename  = p._wstr("filename");       // Filename of new archive volume
        Send (MESSAGE_VOLUME_FILENAME, filename);
    }
    else if (strequ(request, "can_be_extracted?"))
    {
        // Можно ли извлечь этот файл?
        ItemInfo i;
        i.diskname      = p._wstr    ("diskname");       // Filename on disk
        i.filename      = p._wstr    ("filename");       // Filename of this item
        i.original      = p._longlong("original");       // Bytes, uncompressed
        i.compressed    = p._longlong("compressed");     // Bytes, compressed
        i.time          = p._unsigned("time");           // Datetime stamp
        i.attr          = p._unsigned("attr");           // DOS attributes
        i.is_folder     = p._bool    ("is_folder?");
        i.crc           = p._unsigned("crc");            // CRC32
        i.is_encrypted  = p._bool    ("is_encrypted?");
        return Send (MESSAGE_CAN_BE_EXTRACTED, &i);
    }
    else if (strequ(request, "ask_password"))
    {
        // Запрос пароля расшифровки
        wchar_t *password_buf  = (wchar_t *) p._ptr("password_buf");    // Buffer for password
        int      password_size =             p._int("password_size");   // Buffer size
        Send (MESSAGE_PASSWORD_BUF,  password_buf);
        Send (MESSAGE_PASSWORD_SIZE, password_size);
    }
    else if (strequ(request, "archive"))
    {
        // Общая информация об архиве
        wchar_t *arcname  = p._wstr("arcname");
        char    *arctype  = p._str ("arctype");
        int64    files    = p._longlong("files");
        Send (MESSAGE_ARCINFO_NAME,  arcname);
        Send (MESSAGE_ARCINFO_TYPE,  arctype);
        Send (MESSAGE_ARCINFO_FILES, &files);
    }
    else if (strequ(request, "item"))
    {
        // Информация об очередном файле внутри архива
        ItemInfo i;
        i.diskname      = p._wstr    ("filename");       // Filename of this item
        i.filename      = p._wstr    ("filename");       // Filename of this item
        i.original      = p._longlong("original");       // Bytes, uncompressed
        i.compressed    = p._longlong("compressed");     // Bytes, compressed
        i.time          = p._unsigned("time");           // Datetime stamp
        i.attr          = p._unsigned("attr");           // DOS attributes
        i.is_folder     = p._bool    ("is_folder?");
        i.crc           = p._unsigned("crc");            // CRC32
        i.is_encrypted  = p._bool    ("is_encrypted?");
        Send (MESSAGE_ITEMINFO, &i);
    }
    return 0;
}

// Initialize Haskell runtime
void haskell_init (void)
{
  static bool initialized = FALSE;
  if (!initialized)
  {
    initialized = TRUE;
    int argc = 1;
    char* argv[] = {"ghcDll", NULL}; // argv must end with NULL
    char** args = argv;
    hs_init(&argc, &args);
    hs_add_root(__stginit_Main);
  }
}

int FreeArcExecute (TABI_DYNAMAP arg)
{
  haskell_init();
  return haskell_FreeArcExecute(arg);
}

int FreeArcOpenArchive (TABI_DYNAMAP arg)
{
  haskell_init();
  return haskell_FreeArcOpenArchive(arg);
}

int dotnet_FreeArcExecute (wchar_t** arg, CallBackFunc *f)
{
  FreeArcCallback = f;
  return FreeArcExecute(TABI_DYNAMAP ("command", (void*)arg) ("callback", Callback_for_FreeArcExecute));
}

int dotnet_FreeArcOpenArchive (wchar_t* arcname, CallBackFunc *f)
{
  FreeArcCallback = f;
  return FreeArcOpenArchive(TABI_DYNAMAP ("arcname", arcname) ("callback", Callback_for_FreeArcExecute));
}

} // extern "C"
#endif // FREEARC_DLL


/* ********************************************************************************************************
*  Find largest contiguous memory block available and dump information about all available memory blocks
***********************************************************************************************************/

void memstat(void);

struct LargestMemoryBlock
{
  void   *p;
  size_t size;
  LargestMemoryBlock();
  ~LargestMemoryBlock()         {free();}
  size_t total();
  void alloc(size_t n);
  void free();
  void test();
};

LargestMemoryBlock::LargestMemoryBlock() : p(NULL)
{
  size_t a=0, b=UINT_MAX;
  while (b-a>1) {
    free();
    size_t c=(a+b)/2;
    alloc(c);
    if(p) a=c;  else b=c;
  }
}

size_t LargestMemoryBlock::total()
{
  if (size >= 10*mb) {               // Don't count too small memory blocks
    LargestMemoryBlock next;
    return size + next.total();
  } else {
    return 0;
  }
}

void LargestMemoryBlock::test()
{
  if ((size>>20)>0) {
    printf("Allocated %4d mb, addr=%p\n", size>>20, p);
    LargestMemoryBlock next;
    next.test();
  } else {
    memstat();
  }
}

void TestMalloc (void)
{
  memstat();
  printf("\n");
  LargestMemoryBlock m;
  m.test();
}


// Provide malloc operations for testing
void LargestMemoryBlock::alloc(size_t n) {p=malloc(size=n);};
void LargestMemoryBlock::free ()         {::free(p); p=NULL;};

void memstat (void)
{
}


#include <unistd.h>

CFILENAME GetExeName (CFILENAME buf, int bufsize)
{
  int len = readlink("/proc/self/exe", buf, bufsize-1);
  if (len<0)  len=0;
  buf[len] = '\0';
  return buf;
}

unsigned GetMaxBlockToAlloc (void)
{
  //struct sysinfo si;
  //  sysinfo(&si);
  return INT_MAX;
}

unsigned GetTotalMemoryToAlloc (void)
{
  return INT_MAX;
}

// Инициировать выключение компьютера
int PowerOffComputer()
{
  system ("shutdown now");
  return TRUE;
}


void FormatDateTime (char *buf, int bufsize, time_t t)
{
  if (t<0)  t=INT_MAX;  // Иначе получаем вылет :(
  struct tm *p;
  p = localtime(&t);
  strftime (buf, bufsize, "%Y-%m-%d %H:%M:%S", p);
}

// Максимальная длина имени файла
int long_path_size (void)
{
  return MY_FILENAME_MAX;
}


// Вернуть имя файла без имени каталога
FILENAME basename (FILENAME fullname)
{
  char *basename = fullname;
  for (char* p=fullname; *p; p++)
    if (in_set (*p, ALL_PATH_DELIMITERS))
      basename = p+1;
  return basename;
}

// От-xor-ить два блока данных
void memxor (char *dest, char *src, uint size)
{
  if (size) do
      *dest++ ^= *src++;
  while (--size);
}


/* ***************************************************************************
*                                                                            *
* Random system values collection routine from CryptLib by Peter Gutmann     *
* [ftp://ftp.franken.de/pub/crypt/cryptlib/cl331.zip]                        *
*                                                                            *
*****************************************************************************/

/* The size of the intermediate buffer used to accumulate polled data */
#define RANDOM_BUFSIZE	4096

// Handling random data buffer
#define initRandomData(rand_buf, rand_size)  \
                                 char *rand_ptr=(rand_buf), *rand_end=(rand_buf)+(rand_size)
#define addRandomData(ptr,size)  (memcpy (rand_ptr, (ptr), mymin((size),rand_end-rand_ptr)), rand_ptr+=mymin((size),rand_end-rand_ptr))
#define addRandomLong(value)     {long n=(value); addRandomData(&n, sizeof(long));}
#define addRandomValue(value)    addRandomLong((long) value)


/* Map a value that may be 32 or 64 bits depending on the platform to a long */
#if defined( _MSC_VER ) && ( _MSC_VER >= 1400 )
  #define addRandomHandle( handle ) \
		  addRandomLong( PtrToUlong( handle ) )
#else
  #define addRandomHandle	addRandomValue
#endif /* 32- vs. 64-bit VC++ */


// This routine fills buffer with system-generated pseudo-random data
// and returns number of bytes filled
int systemRandomData (char *rand_buf, int rand_size)
{
	FILE *f = fopen ("/dev/urandom", "rb");

	if (f == NULL)
	{
		perror ("Cannot open /dev/urandom");
		return 0;
	}

	if (file_read (f, rand_buf, rand_size) != rand_size)
	{
		perror ("Read from /dev/urandom failed");
		fclose (f);
		return 0;
	}

	fclose (f);
	return rand_size;
}

/****************************************************************************
*
*                                           Random system values collection *
*
****************************************************************************/
