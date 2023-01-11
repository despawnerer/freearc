#define NAME "unpacker"
#define HEADER1 "FreeArc 0.67 "
#define HEADER2 "  http://freearc.org  2014-03-16\n"

/******************************************************************************
** Callbacks для выполняемой команды ******************************************
******************************************************************************/
class COMMAND;

typedef FILENAME MYFILENAME;

class BASEUI {
public:
  virtual ~BASEUI() {}
  virtual void DisplayHeader(char *header) {}
  virtual bool AllowProcessing(char cmd, int silent, MYFILENAME arcname,
                               char *comment, int cmtsize, FILENAME outdir) {
    return TRUE;
  }
  virtual FILENAME GetOutDir() { return ""; }
  virtual bool BeginProgress(uint64 totalBytes) { return TRUE; }
  virtual bool ProgressRead(uint64 readBytes) { return TRUE; }
  virtual bool ProgressWrite(uint64 writtenBytes) { return TRUE; }
  virtual bool ProgressFile(bool isdir, const char *operation,
                            MYFILENAME filename, uint64 filesize) {
    return TRUE;
  }
  virtual void EndProgress(COMMAND *) {}
  virtual char AskOverwrite(MYFILENAME filename, uint64 size, time_t modified) {
    return 'n';
  }
  virtual char AskPassword(char *pwd, int pwdbuf_size) { return 'n'; }
  virtual void ListHeader(COMMAND &) {}
  virtual void ListFooter(COMMAND &) {}
  virtual void ListFiles(DIRECTORY_BLOCK *, COMMAND &) {}
  virtual void Abort(COMMAND *, int errcode, char *errmsg) {
    exit(FREEARC_EXIT_ERROR);
  }
};

/******************************************************************************
** Информация о выполняемой деархиватором команде *****************************
******************************************************************************/
class COMMAND {
public:
  char cmd; // Выполняемая команда
  FILENAME arcname; // Имя обрабатываемого командой архива
  FILENAME *filenames; // Имена обрабатываемых командой файлов из архива
  char arc_base_dir[MY_FILENAME_MAX * 4]; // Базовый каталог внутри архива
  MYDIR
      outpath; // Каталог, куда распаковываются файлы (опция -dp или временный)
  MYDIR workdir; // Каталог для временных файлов
  MYFILE runme; // Файл, запускаемый после распаковки
  BOOL tempdir; // Мы извлекали файлы во временный каталог?
  BOOL wipeoutdir; // Удалить файлы из outpath после завершения работы runme?
  BOOL ok;         // Команда выполняется успешно?
  BOOL noLimitMem; // Не ограничивать потребление памяти для распаковки?
  MemSize limitMem; // Лимит использования памяти при распаковке
  char *pwd;        // Опция -p (пароль расшифровки)
  int silent;       // Опция -s
  BOOL yes;         // Опция -o+
  BOOL no;          // Опция -o-
  BOOL noarcext;    // Опция --noarcext
  BOOL nooptions;   // Опция --

  COMMAND(int argc, char *argv[]); // Разбор командной строки
  void Prepare(); // Приготовиться к выполнению команды
  bool list_cmd() {
    return cmd == 'l' || cmd == 'v';
  } // TRUE, если это команда получения листинга архива
  BOOL accept_file(DIRECTORY_BLOCK *dirblock,
                   int i); // TRUE, если i-й файл каталога dirblock следует
                           // включить в обработку
};

#define PASSWORDBUF_SIZE 2000

/******************************************************************************
** External compressors support ***********************************************
******************************************************************************/
extern "C" {
#include "../Compression/External/C_External.h"
}

// Register external compressors declared in arc.ini
//   using either filename specified in -cfg option or arc.ini in the same dir
//   as progname==argv[0]
void RegisterExternalCompressors(char *progname, char *cfg_option) {
#ifndef FREEARC_TINY
  // Open config file arc.ini found in the same dir as sfx/unarc
  char *cfgfile = "arc.ini";
  char *name = (char *)malloc_msg(strlen(progname) + strlen(cfgfile));

  strcpy(name, progname);
  strcpy(drop_dirname(name), cfgfile);
  MYFILE f(cfg_option ? cfg_option : name);
  free(name);
  if (!f.tryOpen(READ_MODE))
    return;

  // Read config file into memory
  FILESIZE size = f.size();
  if (size < 0)
    return;
  char *contents = (char *)malloc_msg(size + 2);
  contents[0] = '\n';
  size = f.tryRead(contents + 1, size);
  if (size < 0)
    return;
  contents[size + 1] = '\0';

  // Register each external compressor found in config file
  char *ANY_HEADING = "\n[", *EXT_HEADING = "[External compressor:";
  ClearExternalCompressorsTable();
  for (char *p, *section = strstr(contents, ANY_HEADING); section != NULL;
       section = p) {
    section++;
    p = strstr(section, ANY_HEADING);
    if (p)
      *p = '\0';
    if (start_with(section, EXT_HEADING) &&
        AddExternalCompressor(section) != 1) {
      // printf("Error in config file %s section:\n%s\n", cfgfile, section);
    }
  }

  free(contents);
  f.close();
#endif
}

/******************************************************************************
** Разбор командной строки ****************************************************
******************************************************************************/
COMMAND::COMMAND(int argc, char *argv[]) {
  // Default options
  noarcext = FALSE;
  nooptions = FALSE;
  strcpy(arc_base_dir, "");
  outpath.setname("");
  workdir.setname("");
  runme.setname("");
  pwd = (char *)malloc_msg(PASSWORDBUF_SIZE + 1);
  strcpy(pwd, "");
  wipeoutdir = FALSE;
  tempdir = FALSE;
  yes = FALSE;
  no = FALSE;
  noLimitMem = FALSE;
  limitMem = 0;
  silent = 0;
  int error = 0;
  char *cfg = NULL;
  char *progname = argv[0];

  // Parse unarc.exe/unarc.dll options
  cmd = ' ';
  arcname = NULL;
  for (ok = TRUE; ok && *++argv;) {
    if (!nooptions && argv[0][0] == '-') {
      if (strequ(argv[0], "--noarcext"))
        noarcext = TRUE;
      else if (strequ(argv[0], "-o+"))
        yes = TRUE;
      else if (strequ(argv[0], "-o-"))
        no = TRUE;
      else if (strequ(argv[0], "-ld-"))
        noLimitMem = TRUE;
      else if (start_with(argv[0], "-ld"))
        limitMem = parseMem(argv[0] + 3, &error, 'm'), ok = !error;
      else if (start_with(argv[0], "-ap"))
        strcpy(arc_base_dir, argv[0] + 3),
            (arc_base_dir[0] && is_path_char(last_char(arc_base_dir)) &&
             (last_char(arc_base_dir) = '\0'));
      else if (start_with(argv[0], "-dp"))
        outpath.setname(argv[0] + 3);
      else if (start_with(argv[0], "-w"))
        workdir.setname(argv[0] + 2);
      else if (start_with(argv[0], "-p"))
        strncopy(pwd, argv[0] + 2, PASSWORDBUF_SIZE);
      else if (start_with(argv[0], "-cfg"))
        cfg = argv[0] + 4, (strequ(cfg, "-") && (cfg = ""));
      else if (strequ(argv[0], "--"))
        nooptions = TRUE;
      else
        ok = FALSE;
    } else if (cmd == ' ')
      cmd = argv[0][0], ok = ok && strlen(argv[0]) == 1;
    else if (!arcname)
      arcname = argv[0];
    else
      break;
  }

  filenames = argv; // the rest of arguments are filenames
  ok = ok && strchr("lvtex", cmd) && arcname;
  if (ok) {
    RegisterExternalCompressors(progname, cfg);
    return;
  }
#ifndef FREEARC_LIBRARY
  printf(HEADER2 "Usage: unarc command [options] archive[.arc] [filenames...]\n"
                 "Available commands:\n"
                 "  l - display archive listing\n"
                 "  v - display verbose archive listing\n"
                 "  e - extract files into current directory\n"
                 "  x - extract files with pathnames\n"
                 "  t - test archive integrity\n"
                 "Available options:\n"
                 "  -ap{Path}   - set base directory inside archive\n"
                 "  -dp{Path}   - set destination path\n"
                 "  -w{Path}    - set temporary files directory\n"
                 "  -p{Pwd}     - set decryption password\n"
                 "  -ld{Mem}    - limit memory used for decompression (-ld- "
                 "means no limit)\n"
                 "  -o+         - overwrite existing files\n"
                 "  -o-         - don't overwrite existing files\n"
                 "  --noarcext  - don't add default extension to archive name\n"
                 "  -cfg{Path}  - config file name (default: arc.ini, -cfg- "
                 "means no config)\n"
                 "  --          - no more options\n");
#endif // FREEARC_LIBRARY
}

// Приготовиться к выполнению команды
void COMMAND::Prepare() {
  SetTempDir(workdir.filename);
  SetCompressionThreads(GetProcessorsCount());
}

// TRUE, если i-й файл каталога dirblock следует включить в обработку
BOOL COMMAND::accept_file(DIRECTORY_BLOCK *dirblock, int i) {
  if (!is_subdir_of(arc_base_dir, dirblock->dirname(i)))
    return FALSE; // Сначала проверим что каталог файла является подкаталогом
                  // -ap
  if (!*filenames)
    return TRUE; // В командной строке не указано ни одного имени файла -
                 // значит, нужно обрабатывать любой файл
  for (FILENAME *f = filenames; *f; f++) {
    if (strequ(dirblock->name[i], *f))
      return TRUE; // О! Совпало!
  }
  return FALSE; // Совпадающего имени не найдено
}
