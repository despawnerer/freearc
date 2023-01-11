#define _WIN32_WINNT 0x0500
#include "Common.h"
#include "Compression.h"
#include <stdlib.h>

// Для обработки ошибок во вложенных процедурах - longjmp сигнализирует
// процедуре верхнего уровня о произошедшей ошибке
int jmpready = FALSE;
jmp_buf jumper;

bool AllocTopDown = true;

// ****************************************************************************
// MEMORY ALLOCATION **********************************************************
// ****************************************************************************

// #define _SZ_ALLOC_DEBUG
/* use _SZ_ALLOC_DEBUG to debug alloc/free operations */
#ifdef _SZ_ALLOC_DEBUG
#include <stdio.h>
int g_allocCount = 0;
int g_allocCountMid = 0;
int g_allocCountBig = 0;
#define alloc_debug_printf(x) fprintf x
#else
#define alloc_debug_printf(x)
#endif

void *MyAlloc(size_t size) throw() {
  if (size == 0)
    return 0;
  alloc_debug_printf(
      (stderr, "  Alloc %10d bytes; count = %10d\n", size, g_allocCount++));
  return ::malloc(size);
}

void MyFree(void *address) throw() {
  if (address != 0)
    alloc_debug_printf((stderr, "  Free; count = %10d\n", --g_allocCount));

  ::free(address);
}

// ****************************************************************************
// Функции парсинга и арифметики **********************************************
// ****************************************************************************

// Копирует строчку from в to, но не более len символов
void strncopy(char *to, char *from, int len) {
  for (int i = len; --i && *from;)
    *to++ = *from++;
  *to = '\0';
}

// Разбить строку str на подстроки, разделённые символом splitter.
// Результат - в строке str splitter заменяется на '\0'
//   и массив result заполняется ссылками на выделенные в str подстроки + NULL
//   (аналогично argv)
// Возвращает число найденных подстрок
int split(char *str, char splitter, char **result_base, int result_size) {
  char **result = result_base;
  char **result_last = result_base + result_size - 1;
  *result++ = str;
  while (*str && result < result_last) {
    while (*str && *str != splitter)
      str++;
    if (*str) {
      *str++ = '\0';
      *result++ = str;
    }
  }
  *result = NULL;
  return result - result_base;
}

// Объединить NULL-terminated массив строк src в строку result, ставя между
// строками разделитель splitter
void join(char **src, char splitter, char *result, int result_size) {
  char *dst = result;
  *dst = '\0';
  while (*src && result + result_size - dst - 1 > 0) {
    if (dst > result)
      *dst++ = splitter;
    strncopy(dst, *src++, result + result_size - dst);
    dst = strchr(dst, '\0');
  }
}

// Найти параметр с заданным именем в массиве параметров алгоритма
char *search_param(char **param, char *prefix) {
  for (; *param; param++)
    if (start_with(*param, prefix))
      return *param + strlen(prefix);
  return NULL;
}

// Заменяет в строке original все вхождения from на to,
// возвращая вновь выделенную new строку и освобождая оригинал, если была хоть
// одна замена
char *subst(char *original, char *from, char *to) {
  while (1) {
    char *p = strstr(original, from);
    if (!p)
      return original;
    char *newstr = new char[strlen(original) + strlen(to) - strlen(from) + 1];
    memcpy(newstr, original, p - original);
    strcpy(newstr + (p - original), to);
    strcat(newstr + (p - original), p + strlen(from));
    delete (original);
    original = newstr;
  }
}

// Пропускает пробелы в начале строки и убирает их в конце, модифицируя строку
char *trim_spaces(char *s) {
  while (isspace(*s))
    s++;
  char *last = &last_char(s);
  while (last >= s && isspace(*last))
    *last-- = '\0';
  return s;
}

// Replace from:how_many substring and put result in new allocated area
char *str_replace_n(char *orig, char *from, int how_many, char *to) {
  char *result = new char[strlen(orig) + strlen(to) - how_many + 1],
       *p = result;
  memcpy(p, orig, from - orig);
  p += from - orig;
  strcpy(p, to);
  strcat(p, from + how_many);
  return result;
}

// Replace substring and put result in new allocated area
char *str_replace(char *orig, char *from, char *to) {
  char *p = strstr(orig, from);
  if (p)
    return str_replace_n(orig, p, strlen(from), to);
  else
    return strdup_msg(orig);
}

// If the string param contains a double, return it - otherwise set error=1
double parseDouble(char *param, int *error) { return atof(param); }

// If the string param contains an integer, return it - otherwise set error=1
MemSize parseInt(char *param, int *error) {
  MemSize n = 0;
  char c = *param == '=' ? *++param : *param;
  if (c == '\0')
    *error = 1;
  while (c >= '0' && c <= '9')
    n = n * 10 + c - '0', c = *++param;
  if (c != '\0')
    *error = 1;
  return n;
}

// Similar to parseInt, but the string param may have a suffix b/k/m/g/^,
// representing units of memory, or in the case of '^' (default, overridden by
// spec parameter), the relevant power of 2
uint64 parseMem64(char *param, int *error, char spec) {
  uint64 n = 0;
  char c = *param == '=' ? *++param : *param;
  if (!(c >= '0' && c <= '9')) {
    *error = 1;
    return 0;
  }
  while (c >= '0' && c <= '9')
    n = n * 10 + c - '0', c = *++param;
  switch (c ? c : spec) {
  case 'b':
    return n;
  case 'k':
    return n * kb;
  case 'm':
    return n * mb;
  case 'g':
    return n * gb;
  case '^':
    return MemSize(1) << n;
  }
  *error = 1;
  return 0;
}

MemSize parseMem(char *param, int *error, char spec) {
  return parseMem64(param, error, spec);
}

// Returns a string with the amount of memory
void showMem(MemSize mem, char *result) {
  if (mem == 0)
    sprintf(result, "0b");
  else if (mem % gb == 0)
    sprintf(result, "%.0lfgb", double(mem / gb));
  else if (mem % mb == 0)
    sprintf(result, "%.0lfmb", double(mem / mb));
  else if (mem % kb == 0)
    sprintf(result, "%.0lfkb", double(mem / kb));
  else
    sprintf(result, "%.0lfb", double(mem));
}

// Returns a string with the amount of memory
void showMem64(uint64 mem, char *result) {
  if (mem == 0)
    sprintf(result, "0b");
  else if (mem % terabyte == 0)
    sprintf(result, "%.0lftb", double(mem / terabyte));
  else if (mem % gb == 0)
    sprintf(result, "%.0lfgb", double(mem / gb));
  else if (mem % mb == 0)
    sprintf(result, "%.0lfmb", double(mem / mb));
  else if (mem % kb == 0)
    sprintf(result, "%.0lfkb", double(mem / kb));
  else
    sprintf(result, "%.0lfb", double(mem));
}

// Кодирование строки в шестнадцатеричный вид плюс \0
void encode16(const BYTE *src, int srcSize, char *dst) {
  for (; srcSize--; src++)
    *dst++ = int2char(*src / 16), *dst++ = int2char(*src % 16);
  *dst++ = '\0';
}

// Декодирование строки, записанной в шестнадцатеричном виде, в
// последовательность байт
void decode16(const char *src, BYTE *dst) {
  for (; src[0] && src[1]; src += 2)
    *dst++ = char2int(src[0]) * 16 + char2int(src[1]);
}

// ОШИБОЧНОЕ декодирование строки, записанной в шестнадцатеричном виде, в
// последовательность байт
void buggy_decode16(const char *src, BYTE *dst) {
  for (; src[0] && src[1]; src += 2)
    *dst++ = buggy_char2int(src[0]) * 16 + buggy_char2int(src[1]);
}

// Округляет размер памяти вниз до удобной величины
MemSize rounddown_mem(MemSize n) {
  if (n < 32 * kb)
    return n;
  else if (n < 32 * mb)
    return roundDown(n, 1 * kb);
  else
    return roundDown(n, 1 * mb);
}

// ****************************************************************************
// Filename manipulations *****************************************************
// ****************************************************************************

// Replace alternative path delimiters with OS-specific one and remove "." and
// ".." from the path
char *sanitize_filename(char *filename) {
  char *dirs[MAX_PATH_COMPONENTS];
  int n = 0;
  dirs[n++] = filename;
  bool isdir = true;
  char *dst = filename;
  for (char *src = filename; *src;) // Copy filename from src to dst, removing
                                    // "." and ".." path components
  {
    // printf("%d:%s -> %d:%s\n", src-filename, src, dst-filename, dst);
    if (isdir) // If it's a first letter of next path component
    {
      if (src[0] == '.' &&
          in_set0(src[1], ALL_PATH_DELIMITERS)) // Skip "." path component
      {
        src += src[1] ? 2 : 1;
        continue;
      }
      if (src[0] == '.' && src[1] == '.' &&
          in_set0(src[2],
                  ALL_PATH_DELIMITERS)) // Remove path component preceding ".."
      {
        src += src[2] ? 3 : 2;
        dst = dirs[n - 1];
        n > 1 && n--;
        continue;
      }
      dirs[n++] = dst;
      if (n == elements(dirs)) // prevent out-of-array-bounds
        n = 1;
    }
    *dst = (in_set(*src, UNSUPPORTED_PATH_DELIMITERS) ? PATH_DELIMITER : *src);
    isdir = in_set(*dst, ALL_PATH_DELIMITERS);
    src++, dst++;
  }
  if (dst > filename && dst[-1] == PATH_DELIMITER) // Remove trailing slash
    dst--;
  *dst = 0;
  return filename;
}

/* Checking code
char *ptr[] =
{"abcde","abc\\de","abc/de","abc/./de","./abc/de","abc/de/.","abc/de/","abc/de/..","abc/../de","../abc/de","abc/../../de","a/bc/de/../..","a/bc/de/../../fg",0};
for (char **p=ptr; *p; p++)
{
  char a[12345]; strcpy(a,*p);
  printf("%s %s\n", *p, sanitize_filename (a));
}
*/

//*****************************************************************************
// File/system operations *****************************************************
//*****************************************************************************

// Directory for temporary files
static CFILENAME TempDir = 0;

// Set temporary files directory
void SetTempDir(CFILENAME dir) {
  if (dir && TempDir && _tcscmp(dir, TempDir) == 0)
    return; // the same string
  FreeAndNil(TempDir);
  if (dir && *dir) {
    TempDir = (CFILENAME)malloc_msg((_tcslen(dir) + 1) * sizeof(*dir));
    _tcscpy(TempDir, dir);
  }
}

// Return last value set or OS-default temporary directory
CFILENAME GetTempDir(void) {
  if (!TempDir) {
    TempDir = tempnam(NULL, NULL);
    CFILENAME basename = drop_dirname(TempDir);
    if (basename > TempDir)
      basename[-1] = '\0';
  }
  return TempDir;
}

#include <sys/resource.h>
#include <unistd.h>

uint64 GetPhysicalMemory(void) {
  return uint64(sysconf(_SC_PHYS_PAGES)) * sysconf(_SC_PAGE_SIZE);
}

uint64 GetAvailablePhysicalMemory(void) {
  return uint64(sysconf(_SC_AVPHYS_PAGES)) * sysconf(_SC_PAGE_SIZE);
}

int GetProcessorsCount(void) { return sysconf(_SC_NPROCESSORS_ONLN); }

void SetFileDateTime(CFILENAME Filename, time_t t) {
  if (t < 0)
    t = INT_MAX; // Иначе получаем вылет :(
  struct stat st;
  stat(Filename, &st);
  struct utimbuf times;
  times.actime = st.st_atime;
  times.modtime = t;
  utime(Filename, &times);
}

// Execute `command` in the directory `curdir` optionally waiting until it
// finished
int RunCommand(CFILENAME command, CFILENAME curdir, int wait_finish,
               SIMPLE_CALLBACK *callback, void *auxdata) {
  char *olddir = (char *)malloc_msg(),
       *cmd = (char *)malloc_msg(strlen(command) + 10);
  getcwd(olddir, MY_FILENAME_MAX);

  chdir(curdir);
  const char *prefix = (memcmp(command, "/", 1) == 0)     ? ""
                       : (memcmp(command, "\"/", 2) == 0) ? ""
                       : (memcmp(command, "\"", 1) == 0)  ? (command++, "\"./")
                                                          : "./";
  sprintf(cmd, "%s%s%s", prefix, command, wait_finish ? "" : " &");
  int ExitCode = system(cmd);

  if (callback)
    callback(auxdata); // Action that should be executed BEFORE waiting for
                       // process finish

  chdir(olddir);
  free(cmd);
  free(olddir);
  return ExitCode;
}

// Execute file `filename` in the directory `curdir` optionally waiting until it
// finished
void RunFile(CFILENAME filename, CFILENAME curdir, int wait_finish) {
  RunCommand(filename, curdir, wait_finish, NULL, NULL);
}

// Установить приоритет треда какой полагается для тредов сжатия (распаковки,
// шифрования...). Используется для тредов, которые выполняют только сжатие
void SetCompressionThreadPriority(void) {
  int old = getpriority(PRIO_PROCESS, 0);
  setpriority(PRIO_PROCESS, 0, old + 1);
}

// Временно установить приоритет треда какой полагается для тредов сжатия
// (распаковки, шифрования...)
int BeginCompressionThreadPriority(void) {
  int old = getpriority(PRIO_PROCESS, 0);
  // setpriority(PRIO_PROCESS, 0, old+1);        закомментировано из-за проблем
  // с восстановлением старого приоритета :(
  return old;
}

// Восстановить приоритет треда таким, как мы его запомнили
void EndCompressionThreadPriority(int old_priority) {
  // setpriority(PRIO_PROCESS, 0, old_priority);
}

// Создать каталоги на пути к name
void BuildPathTo(CFILENAME name) {
  CFILENAME path_ptr = NULL;
  for (CFILENAME p = _tcschr(name, 0); --p >= name;)
    if (_tcschr(_T(DIRECTORY_DELIMITERS), *p)) {
      path_ptr = p;
      break;
    }
  if (path_ptr == NULL)
    return;

  TCHAR oldc = *path_ptr;
  *path_ptr = 0;

  if (!file_exists(name)) {
    BuildPathTo(name);
    create_dir(name);
  }
  *path_ptr = oldc;
}

// ****************************************************************************************************************************
// ПОДДЕРЖКА СПИСКА ВРЕМЕННЫХ ФАЙЛОВ И УДАЛЕНИЕ ИХ ПРИ АВАРИЙНОМ ВЫХОДЕ ИЗ
// ПРОГРАММЫ ******************************************
// ****************************************************************************************************************************

// Table of temporary files that should be deleted on ^Break
static int TemporaryFilesCount = 0;
static MYFILE *TemporaryFiles[100];

void registerTemporaryFile(MYFILE &file) {
  unregisterTemporaryFile(
      file); // First, delete all existing registrations of the same file
  TemporaryFiles[TemporaryFilesCount] = &file;
  if (TemporaryFilesCount < elements(TemporaryFiles))
    TemporaryFilesCount++;
}

void unregisterTemporaryFile(MYFILE &file) {
  iterate_var(i, TemporaryFilesCount) if (TemporaryFiles[i] == &file) {
    memmove(TemporaryFiles + i, TemporaryFiles + i + 1,
            (TemporaryFilesCount - (i + 1)) * sizeof(TemporaryFiles[i]));
    TemporaryFilesCount--;
    return;
  }
}

void removeTemporaryFiles(void) {
  // Enum files in reverse order in order to delete dirs after files they
  // contain
  for (int i = TemporaryFilesCount - 1; i >= 0; i--)
    TemporaryFiles[i]->tryClose(), TemporaryFiles[i]->remove();
}

#ifndef FREEARC_NO_TIMING

//*****************************************************************************
// Вывод заголовка окна *******************************************************
//*****************************************************************************

void EnvSetConsoleTitle(char *title) {
  // Commented out since 1) we can't restore title on exit and 2) it looks
  // unusual on Linux
  //   fprintf (stderr, "\033]0;%s\a", title);
}
void EnvSetConsoleTitleA(char *title) {
  // See EnvSetConsoleTitle() definition
}

void EnvResetConsoleTitle(void){};

//*****************************************************************************
// Timing execution ***********************************************************
//*****************************************************************************

#include <sys/resource.h>
#include <sys/time.h>
// Returns number of wall-clock seconds since some moment
double GetGlobalTime(void) {
  struct timespec ts;
  int res = clock_gettime(CLOCK_MONOTONIC, &ts);
  return res ? -1 : (ts.tv_sec + ((double)ts.tv_nsec) / 1000000000);
}

// Returns number of seconds spent in this process
double GetCPUTime(void) {
  struct rusage usage;
  int res = getrusage(RUSAGE_SELF, &usage);
  return res ? -1
             : (usage.ru_utime.tv_sec +
                ((double)usage.ru_utime.tv_usec) / 1000000);
}

// Returns number of seconds spent in this thread
double GetThreadCPUTime(void) {
  struct rusage usage;
  int res = getrusage(RUSAGE_THREAD, &usage);
  return res ? -1
             : (usage.ru_utime.tv_sec +
                ((double)usage.ru_utime.tv_usec) / 1000000);
}
#endif // FREEARC_UNIX

// Time-based random number
unsigned time_based_random(void) {
  double t = GetGlobalTime();
  return (unsigned)t + (unsigned)(t * 1000000000);
}

//*****************************************************************************
// Signal handler *************************************************************
//*****************************************************************************

#include <csignal>

void Install_signal_handler(void(__cdecl *signal_handler)(int)) {
  signal(SIGINT, signal_handler);
  signal(SIGTERM, signal_handler);
#ifdef SIGBREAK
  signal(SIGBREAK, signal_handler);
#endif
}

// ****************************************************************************************************************************
// ENCRYPTION ROUTINES
// *****************************************************************************************************
// ****************************************************************************************************************************

// Fills buf with OS-provided random data
int systemRandomData(void *buf, int len) {
  static bool initialized = false;
  if (len == 0)
    return 0;

  static int f = -1;

  if (!initialized) {
    f = open("/dev/random", O_RDONLY);
    initialized = true;
  }

  if (f >= 0) {
    int bytes = read(f, buf, len);
    if (bytes > 0)
      return bytes;
  }

  return 0;
}
