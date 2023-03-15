// to do: отбор файлов по именам ("name" или "dir/name"),
//        дешифрование данных/заголовка
//        добавление ".arc", listfiles/-ap/-kb

// Обработка сбоев при распаковке архива
#undef ON_CHECK_FAIL
#define ON_CHECK_FAIL() UnarcQuit()
void UnarcQuit();

// Доступ к структуре архива, парсингу командной строки и выполнению операций
// над архивом
#include "ArcCommand.h"
#include "ArcProcess.h"
#include "ArcStructure.h"

// Экстренный выход из программы в случае ошибки
void UnarcQuit() { CurrentProcess->quit(FREEARC_ERRCODE_GENERAL, ""); }

// Весь диалог с пользователем описан в сменных модулях, включаемых здесь
#include "CUI.h"
CUI UI;

/******************************************************************************
** Основная программа *********************************************************
******************************************************************************/

int main(int argc, char *argv[]) {
  UI.DisplayHeader(HEADER1 NAME);
  COMMAND command(argc, argv); // Распарсить команду
  if (command.ok) // Если парсинг был удачен и можно выполнить команду
    PROCESS(&command, &UI); //   Выполнить разобранную команду
  printf(command.ok ? (command.list_cmd() ? "" : "All OK\n")
                    : "Error(s) found\n");
  return command.ok ? EXIT_SUCCESS : FREEARC_EXIT_ERROR;
}
