// Common definitions, used by various parts of FreeArc project
#ifndef FREEARC_COMMON_H
#define FREEARC_COMMON_H

#include <ctype.h>
#include <limits.h>
#include <math.h>
#include <setjmp.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include <fcntl.h>
#include <sys/stat.h>
#include <sys/types.h>

#if defined(_M_X64) || defined(_M_AMD64) || defined(__x86_64__)
#define FREEARC_64BIT
#endif

#ifdef __cplusplus
extern "C" {
#endif

/******************************************************************************
** Базовые определения FREEARC ************************************************
******************************************************************************/

#if defined(FREEARC_INTEL_BYTE_ORDER)
#if _BIG_ENDIAN
#error                                                                         \
    "You're compiling for Motorola byte order, but FREEARC_INTEL_BYTE_ORDER was defined."
#endif
#elif defined(FREEARC_MOTOROLA_BYTE_ORDER)
#if _M_IX86 || __i386 || __x86_64
#error                                                                         \
    "You're compiling for Intel byte order, but FREEARC_MOTOROLA_BYTE_ORDER was defined."
#endif
#else
#error "You must define byte order!"
#endif

#define WINDOWS_ONLY(_)
#define UNIX_ONLY

#define UNSUPPORTED_PATH_DELIMITERS ":\\"
#define PATH_DELIMITER '/'
#define STR_PATH_DELIMITER "/"

#define DIRECTORY_DELIMITERS "/\\"
#define ALL_PATH_DELIMITERS ":/\\"

/******************************************************************************
** Синонимы для простых типов, используемых в программе ***********************
******************************************************************************/
typedef unsigned long ulong;
typedef unsigned int uint, UINT;
typedef unsigned short int ushort;
typedef unsigned char uchar;
#ifdef __GNUC__
#include <stdint.h>
typedef uint64_t uint64;
typedef uint32_t uint32;
typedef uint16_t uint16;
typedef uint8_t uint8, byte, BYTE;
typedef int64_t sint64, int64;
typedef int32_t sint32, int32;
typedef int16_t sint16, int16;
typedef int8_t sint8, int8;
#else
typedef __int64 sint64, int64;
typedef unsigned __int64 uint64;
typedef __int32 sint32, int32;
typedef unsigned __int32 uint32;
typedef __int16 sint16, int16;
typedef unsigned __int16 uint16;
typedef __int8 sint8, int8;
typedef unsigned __int8 uint8, byte, BYTE;
#endif

typedef size_t MemSize; // объём памяти
typedef uint64 LongMemSize;
#define MEMSIZE_MAX UINT_MAX
typedef off_t FILESIZE;
typedef char *FILENAME; // имя файла

/******************************************************************************
** Коды ошибок ****************************************************************
******************************************************************************/
#define FREEARC_OK 0                 /* ALL RIGHT */
#define FREEARC_ERRCODE_GENERAL (-1) /* Some error when (de)compressing */
#define FREEARC_ERRCODE_INVALID_COMPRESSOR                                     \
  (-2) /* Invalid compression method or parameters */
#define FREEARC_ERRCODE_ONLY_DECOMPRESS                                        \
  (-3) /* Program was compiled with FREEARC_DECOMPRESS_ONLY, so don't try to   \
          use compress() */
#define FREEARC_ERRCODE_OUTBLOCK_TOO_SMALL                                     \
  (-4) /* Output block size in (de)compressMem is not enough for all output    \
          data */
#define FREEARC_ERRCODE_NOT_ENOUGH_MEMORY                                      \
  (-5) /* Can't allocate memory needed for (de)compression */
#define FREEARC_ERRCODE_READ (-6) /* Error when reading data */
#define FREEARC_ERRCODE_BAD_COMPRESSED_DATA                                    \
  (-7) /* Data can't be decompressed */
#define FREEARC_ERRCODE_NOT_IMPLEMENTED                                        \
  (-8) /* Requested feature isn't supported */
#define FREEARC_ERRCODE_NO_MORE_DATA_REQUIRED                                  \
  (-9) /* Required part of data was already decompressed */
#define FREEARC_ERRCODE_OPERATION_TERMINATED                                   \
  (-10)                               /* Operation terminated by user */
#define FREEARC_ERRCODE_WRITE (-11)   /* Error when writing data */
#define FREEARC_ERRCODE_BAD_CRC (-12) /* File failed CRC check */
#define FREEARC_ERRCODE_BAD_PASSWORD                                           \
  (-13) /* Password/keyfile failed checkcode test */
#define FREEARC_ERRCODE_BAD_HEADERS (-14) /* Archive headers are corrupted */
#define FREEARC_ERRCODE_INTERNAL                                               \
  (-15) /* It should never happen: implementation error. Please report this    \
           bug to developers! */

/******************************************************************************
** Стандартные определения ****************************************************
******************************************************************************/
#define make4byte(a, b, c, d)                                                  \
  ((a) + 256 * ((b) + 256 * ((c) + 256 * (((uint32)d)))))
#define iterate(num, statement)                                                \
  {                                                                            \
    for (int i = 0; i < (num); i++) {                                          \
      statement;                                                               \
    }                                                                          \
  }
#define iterate_var(i, num) for (int i = 0; i < (num); i++)
#define iterate_array(i, array) for (int i = 0; i < (array).size; i++)
#ifndef TRUE
#define TRUE 1
#endif
#ifndef FALSE
#define FALSE 0
#endif

#define PATH_CHARS ":/\\"
#define t_str_end(str) (_tcsrchr(str, '\0'))
#define t_last_char(str) (t_str_end(str)[-1])
#define t_strequ(a, b) (_tcscmp((a), (b)) == EQUAL)
#define is_path_char(c) in_set(c, PATH_CHARS)
#define in_set(c, set) (strchr(set, c) != NULL)
#define in_set0(c, set) (memchr(set, c, sizeof(set)) != 0)
#define str_end(str) (strchr(str, '\0'))
#define last_char(str) (str_end(str)[-1])
#define strequ(a, b) (strcmp((a), (b)) == EQUAL)
#define namecmp strcasecmp
#define nameequ(s1, s2) (namecmp(s1, s2) == EQUAL)
#define start_with(str, with)                                                  \
  (strncmp((str), (with), strlen(with)) == EQUAL ? (str) + strlen(with) : NULL)
#define end_with(str, with) (nameequ(str_end(str) - strlen(with), with))
#define find_extension(str) (find_extension_in_entry(drop_dirname(str)))
#define mymax(a, b) ((a) > (b) ? (a) : (b))
#define mymin(a, b) ((a) < (b) ? (a) : (b))
#define inrange(x, a, b) ((a) <= (x) && (x) < (b))
#define elements(arr) (sizeof(arr) / sizeof(*arr))
#define endof(arr) ((arr) + elements(arr))
#define zeroArray(arr) (memset(arr, 0, sizeof(arr)))
#define EQUAL 0 /* result of strcmp/memcmp for equal strings */

// Skip directory in filename
static inline char *drop_dirname(char *filename) {
  char *p;
  for (p = &last_char(filename); p >= filename; p--) {
    if (is_path_char(*p))
      return p + 1;
  }
  return filename;
}

// Check that `subdir` is a sudirectory of `dir`
static inline int is_subdir_of(char *subdir, char *dir) {
  return subdir == NULL || strequ(subdir, "") ||
         (start_with(dir, subdir) && in_set0(dir[strlen(subdir)], PATH_CHARS));
}

// ****************************************************************************
// FILE OPERATIONS ************************************************************
// ****************************************************************************

#define MY_FILENAME_MAX 65536   /* maximum length of filename */
#define MAX_PATH_COMPONENTS 256 /* maximum number of directories in filename   \
                                 */

#include <fcntl.h>
#include <sys/types.h>
#include <unistd.h>
#include <utime.h>

#define __cdecl
#define SSIZE_T ssize_t

typedef char *CFILENAME;
typedef char TCHAR;
#define _T
#define _tcscmp strcmp
#define _tcschr strchr
#define _tcsrchr strrchr
#define _tcscpy strcpy
#define _stprintf sprintf
#define _tcslen strlen
#define _tcslwr strlwr
#define _tstat stat
#define _stat stat
#define _trmdir rmdir
#define _trename rename
#define _tremove remove
#define _taccess access

typedef int (*FARPROC)(void);

static inline int create_dir(CFILENAME name) { return mkdir(name, 0777); }
#define file_seek(stream, pos) (fseek(stream, (pos), SEEK_SET))
#define get_flen(stream) (myfilelength(fileno(stream)))
#define set_binary_mode(file)
#define myeof(file) (get_flen(file) == ftell(file))
static inline off_t myfilelength(int h) {
  off_t saved = lseek(h, 0, SEEK_CUR);
  off_t size = lseek(h, 0, SEEK_END);
  lseek(h, saved, SEEK_SET);
  return size < 0 ? -1 : size;
}

#define file_open_read_binary(filename) fopen((filename), "rb")
#define file_open_write_binary(filename) fopen((filename), "wb")
#define file_read(file, buf, size) fread(buf, 1, size, file)
#define file_write(file, buf, size) fwrite(buf, 1, size, file)

static inline int remove_dir(CFILENAME name) { return _trmdir(name); }
static inline int remove_file(CFILENAME name) { return _tremove(name); }
static inline int file_exists(CFILENAME name) { return _taccess(name, 0) == 0; }

static inline int rename_file(CFILENAME oldname, CFILENAME newname) {
  return _trename(oldname, newname);
}

static inline int dir_exists(const TCHAR *name) {
  struct _stat st;
  _tstat(name, &st);
  return (st.st_mode & S_IFDIR) != 0;
}

typedef int SIMPLE_CALLBACK(void *param);

void BuildPathTo(CFILENAME name); // Создать каталоги на пути к name
uint64 GetPhysicalMemory(void); // Объём физической памяти компьютера
uint64 GetAvailablePhysicalMemory(
    void); // Объём свободной физической памяти компьютера
int GetProcessorsCount(
    void); // Общее количество процессоров (точнее, физических ядер, а ещё
           // точнее - потоков выполнения) в системе. Используется для
           // определения того, сколько "тяжёлых" вычислительных потоков
           // целесообразно запустить в программе
void SetFileDateTime(CFILENAME Filename,
                     time_t t); // Установить время/дату модификации файла
void RunProgram(
    CFILENAME filename, CFILENAME curdir,
    int wait_finish); // Execute program `filename` in the directory `curdir`
                      // optionally waiting until it finished
int RunCommand(CFILENAME command, CFILENAME curdir, int wait_finish,
               SIMPLE_CALLBACK *callback,
               void *auxdata); // Execute `command` in the directory `curdir`
                               // optionally waiting until it finished
void RunFile(CFILENAME filename, CFILENAME curdir,
             int wait_finish); // Execute file `filename` in the directory
                               // `curdir` optionally waiting until it finished
void SetCompressionThreadPriority(
    void); // Установить приоритет треда какой полагается для тредов сжатия
           // (распаковки, шифрования...).
int BeginCompressionThreadPriority(
    void); // Временно установить приоритет треда какой полагается для тредов
           // сжатия (распаковки, шифрования...)
void EndCompressionThreadPriority(
    int old_priority); // Восстановить приоритет треда таким, как мы его
                       // запомнили
void SetTempDir(CFILENAME dir); // Set temporary files directory
CFILENAME GetTempDir(void);     // Return last value set or GetTempPath (%TEMP)

// ****************************************************************************
// ****************************************************************************
// ****************************************************************************

#define GCC_VERSION (__GNUC__ * 100 + __GNUC_MINOR__)

#if (GCC_VERSION >= 302) || (__INTEL_COMPILER >= 800) || defined(__clang__)
#define expect(expr, value) (__builtin_expect((expr), (value)))
#else
#define expect(expr, value) (expr)
#endif

#define likely(expr) expect((expr) != 0, 1)
#define unlikely(expr) expect((expr) != 0, 0)

#if (GCC_VERSION >= 304)
#define prefetch(var) __builtin_prefetch(&(var))
#elif 1
#include <xmmintrin.h>
#define prefetch(var) (_mm_prefetch((char *)&(var), _MM_HINT_T0))
#else
#define prefetch(var) ((void)0)
#endif

#define CACHE_ROW                                                              \
  64 /* size of typical CPU cache line: from long data only every 64'th byte   \
        should be prefetched */

#ifdef FREEARC_INTEL_BYTE_ORDER

// Read unsigned 16/24/32-bit value at given address
#define value16(p) (*(uint16 *)(p))
#define value24(p) (*(uint32 *)(p)&0xffffff)
#define value32(p) (*(uint32 *)(p))
#define value64(p) (*(uint64 *)(p))
// Write unsigned 16/24/32-bit value to given address
#define setvalue16(p, x) (*(uint16 *)(p) = (x))
#define setvalue24(p, x)                                                       \
  (*(uint32 *)(p) = ((x)&0xffffff) + (*(uint32 *)(p)&0xff000000))
#define setvalue32(p, x) (*(uint32 *)(p) = (x))
#define setvalue64(p, x) (*(uint64 *)(p) = (x))

#elif FREEARC_MOTOROLA_BYTE_ORDER
// routines for non-little-endian cpus, written by Joachim Henke
#if _ARCH_PPC
#if __GNUC__ == 4 && __GNUC_MINOR__ > 0 || __GNUC__ > 4
#define PPC_MCONSTR "Z"
#else
#define PPC_MCONSTR "Q"
#endif
#define PPC_LBRX(s, p, x)                                                      \
  __asm__("l" s "brx %0,%y1" : "=r"(x) : PPC_MCONSTR(*p))
#define PPC_STBRX(s, p, x)                                                     \
  __asm__("st" s "brx %1,%y0" : "=" PPC_MCONSTR(*p) : "r"(x))
#endif

static inline uint16 value16(void *p) {
  uint16 x;
#if _ARCH_PPC
  uint16 *m = (uint16 *)p;
  PPC_LBRX("h", m, x);
#else
  uint8 *m = (uint8 *)p;
  x = m[0] + (m[1] << 8);
#endif
  return x;
}

static inline uint32 value24(void *p) {
  uint32 x;
#if __GNUC__ == 4 && __GNUC_MINOR__ > 2 || __GNUC__ > 4
  uint32 *m = (uint32 *)p;
  x = __builtin_bswap32(*m) & 0xffffff;
#elif _ARCH_PPC
  uint32 *m = (uint32 *)p;
  PPC_LBRX("w", m, x);
  x &= 0xffffff;
#else
  uint8 *m = (uint8 *)p;
  x = m[0] + (m[1] << 8) + (m[2] << 16);
#endif
  return x;
}

static inline uint32 value32(void *p) {
  uint32 x;
#if __GNUC__ == 4 && __GNUC_MINOR__ > 2 || __GNUC__ > 4
  uint32 *m = (uint32 *)p;
  x = __builtin_bswap32(*m);
#elif _ARCH_PPC
  uint32 *m = (uint32 *)p;
  PPC_LBRX("w", m, x);
#else
  uint8 *m = (uint8 *)p;
  x = m[0] + (m[1] << 8) + (m[2] << 16) + (m[3] << 24);
#endif
  return x;
}

static inline uint64 value64(void *p) {
  uint64 x;
#if _ARCH_PPC && __PPU__
  uint64 *m = (uint64 *)p;
  PPC_LBRX("d", m, x);
#else
  uint32 *m = (uint32 *)p;
  x = value32(m) + ((uint64)value32(m + 1) << 32);
#endif
  return x;
}

static inline void setvalue16(void *p, uint16 x) {
#if _ARCH_PPC
  uint16 *m = (uint16 *)p;
  PPC_STBRX("h", m, x);
#else
  uint8 *m = (uint8 *)p;
  m[0] = x;
  m[1] = x >> 8;
#endif
}

static inline void setvalue24(void *p, uint32 x) {
  uint8 *m = (uint8 *)p;
  m[0] = x;
  m[1] = x >> 8;
  m[2] = x >> 16;
}

static inline void setvalue32(void *p, uint32 x) {
#if __GNUC__ == 4 && __GNUC_MINOR__ > 2 || __GNUC__ > 4
  uint32 *m = (uint32 *)p;
  *m = __builtin_bswap32(x);
#elif _ARCH_PPC
  uint32 *m = (uint32 *)p;
  PPC_STBRX("w", m, x);
#else
  uint8 *m = (uint8 *)p;
  m[0] = x;
  m[1] = x >> 8;
  m[2] = x >> 16;
  m[3] = x >> 24;
#endif
}

static inline void setvalue64(void *p, uint64 x) {
#if _ARCH_PPC && __PPU__
  uint64 *m = (uint64 *)p;
  PPC_STBRX("d", m, x);
#else
  uint32 *m = (uint32 *)p;
  setvalue32(m, x);
  setvalue32(m + 1, x >> 32);
#endif
}

#endif // FREEARC_MOTOROLA_BYTE_ORDER

static inline uint16 value16b(void *p) {
#if defined(FREEARC_INTEL_BYTE_ORDER) && defined(_MSC_VER)
  return _byteswap_ushort(value16(p));
#else
  uint8 *m = (uint8 *)p;
  return (m[0] << 8) + m[1];
#endif
}

static inline uint32 value32b(void *p) {
#if defined(FREEARC_INTEL_BYTE_ORDER) && defined(_MSC_VER)
  return _byteswap_ulong(value32(p));
#elif __GNUC__ == 4 && __GNUC_MINOR__ > 2 || __GNUC__ > 4
  return __builtin_bswap32(value32(p));
#else
  uint8 *m = (uint8 *)p;
  return (m[0] << 24) + (m[1] << 16) + (m[2] << 8) + m[3];
#endif
}

static inline void setvalue16b(void *p, uint32 x) {
#if defined(FREEARC_INTEL_BYTE_ORDER) && defined(_MSC_VER)
  uint16 *m = (uint16 *)p;
  *m = _byteswap_ushort(x);
#else
  uint8 *m = (uint8 *)p;
  m[0] = x >> 8;
  m[1] = x;
#endif
}

static inline void setvalue32b(void *p, uint32 x) {
#if defined(FREEARC_INTEL_BYTE_ORDER) && defined(_MSC_VER)
  uint32 *m = (uint32 *)p;
  *m = _byteswap_ulong(x);
#elif __GNUC__ == 4 && __GNUC_MINOR__ > 2 || __GNUC__ > 4
  uint32 *m = (uint32 *)p;
  *m = __builtin_bswap32(x);
#else
  uint8 *m = (uint8 *)p;
  m[0] = x >> 24;
  m[1] = x >> 16;
  m[2] = x >> 8;
  m[3] = x;
#endif
}

// Check for equality
#define val16equ(p, q) (*(uint16 *)(p) == *(uint16 *)(q))
#define val24equ(p, q) (value24(p) == value24(q))
#define val32equ(p, q) (*(uint32 *)(p) == *(uint32 *)(q))
#define val64equ(p, q) (*(uint64 *)(p) == *(uint64 *)(q))

// Free memory block and set pointer to NULL
#ifndef FreeAndNil
#define FreeAndNil(p) ((p) && (free(p), (p) = NULL))
#define BigFreeAndNil(p) ((p) && (BigFree(p), (p) = NULL))
#endif

// Exit code used to indicate serious problems in FreeArc utilities
#define FREEARC_EXIT_ERROR 2

// Переменные, используемые для сигнализации об ошибках из глубоко вложеных
// процедур
extern int jmpready;
extern jmp_buf jumper;

// Процедура сообщения о неожиданных ошибочных ситуациях
#ifndef CHECK
#define CHECK(e, a, b)                                                         \
  {                                                                            \
    if (!(a)) {                                                                \
      if (jmpready)                                                            \
        longjmp(jumper, 1);                                                    \
      char s[MY_FILENAME_MAX * 4];                                             \
      sprintf b;                                                               \
      printf("\n%s", s);                                                       \
      ON_CHECK_FAIL();                                                         \
      exit(FREEARC_EXIT_ERROR);                                                \
    }                                                                          \
  }
#endif

#ifndef ON_CHECK_FAIL
#define ON_CHECK_FAIL()
#endif

// Устанавливает Jump Point с переходом на метку label
#define SET_JMP_POINT_GOTO(label)                                                                              \
  {                                                                                                            \
    if (!jmpready &&                                                                                           \
        setjmp(jumper) != 0) /* Сюда мы попадём при возникновении ошибки в \
                                одной из вызываемых процедур */                       \
    {                                                                                                          \
      jmpready = FALSE;                                                                                        \
      goto label;                                                                                              \
    }                                                                                                          \
    jmpready = TRUE;                                                                                           \
  }

// Устанавливает Jump Point с кодом возврата retcode
#define SET_JMP_POINT(retcode)                                                                                 \
  {                                                                                                            \
    if (!jmpready &&                                                                                           \
        setjmp(jumper) != 0) /* Сюда мы попадём при возникновении ошибки в \
                                одной из вызываемых процедур */                       \
    {                                                                                                          \
      jmpready = FALSE;                                                                                        \
      return retcode;                                                                                          \
    }                                                                                                          \
    jmpready = TRUE;                                                                                           \
  }

// Снимает Jump Point
#define RESET_JMP_POINT()                                                      \
  { jmpready = FALSE; }

// Include statements marked as debug(..)  only if we enabled debugging
#ifdef DEBUG
#define debug(stmt) stmt
#else
#define debug(stmt) ((void)0)
#endif

// Include statements marked with stat_only(..)  only if we enabled gathering
// stats
#ifdef STAT
#define stat_only(stmt) stmt
#else
#define stat_only(stmt) ((void)0)
#endif

// Define default parameter value only when compiled as C++
#ifdef __cplusplus
#define DEFAULT(x, n) x = n
#else
#define DEFAULT(x, n) x
#endif

#define then

// ****************************************************************************
// MEMORY ALLOCATION **********************************************************
// ****************************************************************************
#ifndef __cplusplus
#define throw()
#endif

void *MyAlloc(size_t size) throw();
void MyFree(void *address) throw();
extern bool AllocTopDown;
#define MidAlloc(size) MyAlloc(size)
#define MidFree(address) MyFree(address)
#define BigAlloc(size) MyAlloc(size)
#define BigFree(address) MyFree(address)

// ****************************************************************************
// Функции парсинга и арифметики **********************************************
// ****************************************************************************
void strncopy(char *to, char *from,
              int len); // Копирует строчку from в to, но не более len символов
int split(char *str, char splitter, char **result,
          int result_size); // Разбить строку str на подстроки, разделённые
                            // символом splitter
void join(
    char **src, char splitter, char *result,
    int result_size); // Объединить NULL-terminated массив строк src в строку
                      // result, ставя между строками разделитель splitter
char *search_param(char **param,
                   char *prefix); // Найти параметр с заданным именем в массиве
                                  // параметров алгоритма
char *subst(char *original, char *from,
            char *to); // Заменяет в строке original все вхождения from на to
char *trim_spaces(char *s); // Пропускает пробелы в начале строки и убирает их в
                            // конце, модифицируя строку
char *str_replace_n(char *orig, char *from, int how_many,
                    char *to); // Replace from:how_many substring and put result
                               // in new allocated area
char *
str_replace(char *orig, char *from,
            char *to); // Replace substring and put result in new allocated area
double parseDouble(char *param,
                   int *error); // If the string param contains a double, return
                                // it - otherwise set error=1
MemSize parseInt(char *param,
                 int *error); // If the string param contains an integer, return
                              // it - otherwise set error=1
MemSize parseMem(
    char *param, int *error,
    DEFAULT(
        char spec,
        '^')); // Similar, but the string param may have a suffix b/k/m/g/^,
               // representing units of memory, or in the case of '^' (default,
               // overridden by spec parameter), the relevant power of 2
uint64 parseMem64(
    char *param, int *error,
    DEFAULT(
        char spec,
        '^')); // Similar, but the string param may have a suffix b/k/m/g/^,
               // representing units of memory, or in the case of '^' (default,
               // overridden by spec parameter), the relevant power of 2
void showMem(MemSize mem,
             char *result); // Returns string with the amount of memory
void showMem64(uint64 mem,
               char *result); // Returns string with the amount of memory
void encode16(const BYTE *src, int srcSize,
              char *dst); // Кодирование строки в шестнадцатеричный вид плюс \0
void decode16(const char *src,
              BYTE *dst); // Декодирование строки, записанной в
                          // шестнадцатеричном виде, в последовательность байт
void buggy_decode16(
    const char *src,
    BYTE *dst); // ОШИБОЧНОЕ декодирование строки, записанной в
                // шестнадцатеричном виде, в последовательность байт
MemSize
rounddown_mem(MemSize n); // Округляет размер памяти вниз до удобной величины
char *sanitize_filename(
    char *filename); // Replace alternative path delimiters with OS-specific one
                     // and remove "." and ".." from the path

// Round first number *down* to divisible by second one
static inline MemSize roundDown(MemSize a, MemSize b) {
  return b > 1 ? (a / b) * b : a;
}

// Round first number *up* to divisible by second one
static inline MemSize roundUp(MemSize a, MemSize b) {
  return (a != 0 && b > 1) ? roundDown(a - 1, b) + b : a;
}

// Whole part of number's binary logarithm (logb) - please ensure that n > 0
static inline int lb(MemSize n) {
  int result;
#if __INTEL_COMPILER || (_MSC_VER >= 1400)
#if defined(__x86_64) || defined(_M_X64)
  _BitScanReverse64((DWORD *)&result, n);
#else
  _BitScanReverse((DWORD *)&result, n);
#endif
#elif __GNUC__ == 3 && __GNUC_MINOR__ > 3 || __GNUC__ > 3
#if defined(__x86_64) || defined(_M_X64)
  result = __builtin_clzll(n) ^ (8 * sizeof(unsigned long long) - 1);
#else
  result = __builtin_clz(n) ^ (8 * sizeof(unsigned int) - 1);
#endif
#else
  result = 0;
#if defined(__x86_64) || defined(_M_X64)
  if (n > 0xffffffffu)
    result = 32, n >>= 32;
#endif
  if (n > 0xffff)
    result += 16, n >>= 16;
  if (n > 0xff)
    result += 8, n >>= 8;
  if (n > 0xf)
    result += 4, n >>= 4;
  if (n > 0x3)
    result += 2, n >>= 2;
  if (n > 0x1)
    result += 1;
#endif
  return result;
}

#if __INTEL_COMPILER || (_MSC_VER && _MSC_VER < 1800)
static inline double log2(double x) { return log(x) / log(2.); }
static inline double round(double x) {
  return (x > 0.0) ? floor(x + 0.5) : ceil(x - 0.5);
}
#endif

// Эта процедура округляет число к ближайшей сверху степени
// базы, например f(13,2)=16
static inline MemSize roundup_to_power_of(MemSize n, MemSize base) {
  MemSize result = base;
  if (!n)
    return 0;
  if (!(--n))
    return 1;
  if (base == 2)
    result <<= lb(n);
  else
    while (n /= base)
      result *= base;
  return result;
}

// Эта процедура округляет число к ближайшей снизу степени
// базы, например f(13,2)=8
static inline MemSize rounddown_to_power_of(MemSize n, MemSize base) {
  MemSize result = 1;
  if (!n)
    return 1;
  if (base == 2)
    result <<= lb(n);
  else
    while (n /= base)
      result *= base;
  return result;
}

// Эта процедура округляет число к логарифмически ближайшей степени
// базы, например f(9,2)=8  f(15,2)=16
static inline MemSize round_to_nearest_power_of(MemSize n, MemSize base) {
  MemSize result;
  uint64 nn = ((uint64)n) * n / base;
  if (nn == 0)
    return 1;
  for (result = base; (nn /= base * base) != 0; result *= base)
    ;
  return result;
}

// Превращает число в строку, разделённую запятыми: "1,234,567"
static inline char *show3(uint64 n, char *buf, const char *prepend = "") {
  char *p = buf + 27 + strlen(prepend);
  int i = 4;

  *p = '\0';
  do {
    if (!--i)
      *--p = ',', i = 3;
    *--p = '0' + (n % 10);
  } while (n /= 10);

  memcpy(p - strlen(prepend), prepend, strlen(prepend));
  return p - strlen(prepend);
}

// Заменить символы из множества from на символ to
static inline char *replace(char *str, char *from, char to) {
  char *p;
  for (p = str; *p; p++)
    if (in_set(*p, from))
      *p = to;
  return str;
}

// Возращает числовое значение символа, рассматриваемого как шестнадцатеричная
// цифра, и наоборот (плюс старый ошибочный вариант)
static inline char int2char(int i) { return i > 9 ? 'a' + (i - 10) : '0' + i; }
static inline int char2int(char c) {
  return isdigit(c) ? c - '0' : tolower(c) - 'a' + 10;
}
static inline int buggy_char2int(char c) {
  return isdigit(c) ? c - '0' : tolower(c) - 'a';
}

#ifndef FREEARC_NO_TIMING
// Вывод заголовка окна
void EnvSetConsoleTitle(
    CFILENAME title); // Установить заголовок консольного окна
void EnvSetConsoleTitleA(char *title);
void EnvResetConsoleTitle(
    void); // Восстановить заголовок, который был в начале работы программы

// Timing execution
double
GetGlobalTime(void); // Returns number of wall-clock seconds since some moment
double GetCPUTime(void); // Returns number of seconds spent in this process
double GetThreadCPUTime(void); // Returns number of seconds spent in this thread

// Time-based random number
unsigned time_based_random(void);
#endif

// Signal handler
void Install_signal_handler(void(__cdecl *signal_handler)(int));

#ifdef __cplusplus

// Register/unregister temporary files that should be deleted on ^Break
class MYFILE;
void registerTemporaryFile(MYFILE &file);
void unregisterTemporaryFile(MYFILE &file);
void removeTemporaryFiles(void);

// Checked malloc
static inline void *malloc_msg(unsigned long size = MY_FILENAME_MAX * 4) {
  void *ptr = malloc(size);
  CHECK(FREEARC_ERRCODE_NOT_ENOUGH_MEMORY, ptr,
        (s, "ERROR: can't alloc %lu memory bytes", size));
  return ptr;
}

// Checked strdup
static inline char *strdup_msg(char *old) {
  char *str = (char *)malloc_msg(strlen(old) + 1);
  strcpy(str, old);
  return str;
}

/******************************************************************************
** Класс, абстрагирующий работу с файлами *************************************
******************************************************************************/

enum MODE { READ_MODE, WRITE_MODE }; // режим открытия файла
struct MYFILE {
  // Mark file as temporary, removed automatically by destructor
  bool is_temp;
  void mark_as_temporary() {
    registerTemporaryFile(*this);
    is_temp = TRUE;
  }

  int handle;
  TCHAR *filename;
  char *utf8name, *utf8lastname, *oemname;

  void SetBaseDir(char *utf8dir) // Set base dir
  {
    if (utf8dir != utf8name)
      strcpy(utf8name, utf8dir);
    if (utf8name[0] != '\0' && !is_path_char(last_char(utf8name)))
      strcat(utf8name, STR_PATH_DELIMITER);
    utf8lastname = str_end(utf8name);
  }
  void setname(FILENAME _filename) {
    strcpy(utf8lastname, _filename);
    filename = utf8name;
  }
  FILENAME displayname(void) { return utf8name; }

  void init() {
    handle = -1;
    is_temp = FALSE;
    oemname = (char *)malloc_msg(MY_FILENAME_MAX);
    utf8name = (char *)malloc_msg(MY_FILENAME_MAX * 4);
    utf8lastname = utf8name;
    setname((char *)"");
  }

  void setname(MYFILE &base, FILENAME filename) {
    SetBaseDir(base.utf8name);
    setname(filename);
  }

  void change_executable_ext(
      FILENAME
          ext) { // Changes extension of executable file (i.e. on Linux it
                 // probably has no extension, on Windows, in most cases - .exe)
    strcat(utf8lastname, ".");
    strcat(utf8lastname, ext);
    setname(utf8lastname);
  }

  MYFILE() { init(); }
  MYFILE(FILENAME filename) {
    init();
    setname(filename);
  }
  MYFILE(MYFILE &base, FILENAME filename) {
    init();
    setname(base, filename);
  }
  MYFILE(FILENAME filename, MODE mode) {
    init();
    open(filename, mode);
  }
  virtual void done() {
    tryClose();
    if (is_temp)
      remove(), unregisterTemporaryFile(*this), is_temp = FALSE;
  }
  virtual ~MYFILE() {
    done();
    if ((char *)filename != utf8name)
      free(filename);
    free(oemname);
    free(utf8name);
  }
  // File operations
  virtual bool exists() { return file_exists(filename); }
  virtual bool rename(MYFILE &other) {
    return rename_file(filename, other.filename);
  }
  virtual int remove() { return remove_file(filename); }
  virtual int
  remove_readonly_attrib() { /*struct stat buf; if (0==stat(filename, &buf))
                                chmod(filename, buf.st_mode & ~S_IWUSR &
                                ~S_IWGRP & ~S_IWOTH);*/
    return 0;
  }

  bool tryOpen(MODE mode) // Пытается открыть файл для чтения или записи
  {
    if (mode == WRITE_MODE)
      BuildPathTo(filename);
    handle = ::open(filename,
                    mode == READ_MODE ? O_RDONLY : O_WRONLY | O_CREAT | O_TRUNC,
                    S_IREAD | S_IWRITE);
    return handle >= 0;
  }

  MYFILE &open(MODE mode) // Открывает файл для чтения или записи
  {
    bool success = tryOpen(mode);
    CHECK(mode == READ_MODE ? FREEARC_ERRCODE_READ : FREEARC_ERRCODE_WRITE,
          success, (s, "ERROR: can't open file %s", utf8name));
    return *this;
  }

  MYFILE &open(FILENAME _filename,
               MODE mode) // Открывает файл для чтения или записи
  {
    setname(_filename);
    return open(mode);
  }

  void SetFileDateTime(time_t mtime) {
    ::SetFileDateTime(filename, mtime);
  }            // Устанавливает mtime файла
  void close() // Закрывает файл
  {
    CHECK(FREEARC_ERRCODE_READ, ::close(handle) == 0,
          (s, "ERROR: can't close file %s", utf8name));
    handle = -1;
  }
  bool isopen() { return handle >= 0; }
  void tryClose() {
    if (isopen())
      close();
  }

  FILESIZE size() { return myfilelength(handle); }
  FILESIZE curpos() { return lseek(handle, 0, SEEK_CUR); }
  void seek(FILESIZE pos) {
    CHECK(FREEARC_ERRCODE_READ, lseek(handle, pos, SEEK_SET) == pos,
          (s, "ERROR: file seek operation failed"));
  }

  FILESIZE tryRead(void *buf, FILESIZE size) {
    int result = ::read(handle, buf, size);
    CHECK(FREEARC_ERRCODE_READ, result >= 0,
          (s, "ERROR: file read operation failed"));
    return result;
  } // Возвращает кол-во прочитанных байт, которое может быть меньше
    // запрошенного
  void read(void *buf, FILESIZE size) {
    CHECK(FREEARC_ERRCODE_READ, tryRead(buf, size) == size,
          (s, "ERROR: can't read %lu bytes", (unsigned long)size));
  } // Возбуждает исключение, если не удалось прочесть указанное число байт
  void write(void *buf, FILESIZE size) {
    CHECK(FREEARC_ERRCODE_WRITE, ::write(handle, buf, size) == size,
          (s, "ERROR: file write operation failed"));
  }
};

struct MYDIR : MYFILE {
  int create_dir() { return ::create_dir(filename); }
  int remove_dir() { return ::remove_dir(filename); }
  int dir_exists() { return ::dir_exists(filename); }
  virtual int remove() { return remove_dir(); }

  // Make it a temporary directory, removed automatically by destructor
  bool create_tempdir() {
    strcpy(utf8name, GetTempDir());
    SetBaseDir(utf8name);
    for (unsigned i = time_based_random(), cnt = 0; cnt < 1000; cnt++) {
      i = i * 54322457 + 137;
      char dirname[100];
      sprintf(dirname, "freearc%u.tmp", i);
      setname(dirname);
      BuildPathTo(filename);
      if (create_dir() == 0) {
        mark_as_temporary();
        return TRUE;
      } // Success
    }
    return FALSE; // Fail
  }
  virtual ~MYDIR() { done(); }
};

/******************************************************************************
** Bounds-checked arrays ******************************************************
******************************************************************************/

#ifdef DEBUG
#define ARRAYD(type, name, size) Array<type> name
template <class ELEM> struct Array {
  int n;
  ELEM *p;
  Array(int _n) {
    n = _n;
    p = (ELEM *)malloc_msg(sizeof(ELEM) * n);
  }
  ~Array() { free(p); }
  ELEM &operator[](int i) {
    CHECK(FREEARC_ERRCODE_GENERAL, 0 <= i && i < n,
          (s, "INDEXING ERROR: %d instead of [0,%d)", i, n));
    return p[n];
  }
  operator void *() { return p; }
};
#else
#define ARRAYD(type, name, size) type name[size]
#endif

//*****************************************************************************
// Windows 7 taskbar progress indicator ***************************************
//*****************************************************************************

void Taskbar_SetProgressValue(uint64 ullCompleted, uint64 ullTotal);
void Taskbar_Normal();
void Taskbar_Error();
void Taskbar_Pause();
void Taskbar_Resume();
void Taskbar_Done();

// //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Memory-mapped files
// ******************************************************************************************************************************
// //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

struct MMAP_FILE {
  FILE *file;

  MMAP_FILE(bool, FILE *_file, char *, FILESIZE) : file(_file) {}

  bool mmapped() { return false; } // File ISN'T memory-mapped

  // Read len bytes at offset pos; return number of bytes read
  // Data are read either into internal buffer or into provided buf[];  this
  // address placed into *ptr
  int read(char **ptr, void *buf, FILESIZE pos, int len, FILE *f = NULL) {
    *ptr = (char *)buf;
    file_seek(f ? f : file, pos);
    return file_read(f ? f : file, buf, len);
  }
};

// ****************************************************************************************************************************
// ENCRYPTION ROUTINES
// *****************************************************************************************************
// ****************************************************************************************************************************

// Fills buf with OS-provided random data
int systemRandomData(void *buf, int len);

/******************************************************************************
** END. ***********************************************************************
******************************************************************************/

} // extern "C"

#endif // __cplusplus

#endif // FREEARC_COMMON_H
