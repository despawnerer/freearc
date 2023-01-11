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
