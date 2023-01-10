#include <time.h>
#include "Compression/Common.h"

#ifdef  __cplusplus
extern "C" {
#endif

// Environment.cpp
int long_path_size (void);                                 // ������������ ����� ����� �����
void FormatDateTime (char *buf, int bufsize, time_t t);    // ��������������� �����/���� ��� ������� ��������
CFILENAME GetExeName (CFILENAME buf, int bufsize);         // ������� ��� ������������ ����� ���������
int MyGetAppUserDataDirectory (CFILENAME buf);             // ������� %APPDATA%
unsigned GetMaxBlockToAlloc (void);                        // ����. ����� �����, ������� �� ����� �������� � �������� ������������ ������ ��������
unsigned GetTotalMemoryToAlloc (void);                     // ����� ����� ������ ������� �� ����� �������� � �������� ������������ ������ ��������
void TestMalloc (void);                                    // �������� ���������� ��������� ������
int PowerOffComputer();                                    // ������������ ���������� ����������
void GetOSDisplayString(char* buf);                        // ��������� ����� ������� � ��������� ������ ��
void memxor (char *dest, char *src, uint size);            // ��-xor-��� ��� ����� ������

// GuiEnvironment.cpp
int BrowseForFolder(TCHAR *prompt, TCHAR *in_filename, TCHAR *out_filename);                      // ���� ������������ ������� �������
int BrowseForFile(TCHAR *prompt, TCHAR *filters, TCHAR *in_filename, TCHAR *out_filename);        // ���� ������������ ������� ����
void GuiFormatDateTime (time_t t, char *buf, int bufsize, char *date_format, char *time_format);  // ���������� �����/���� ����� � ������ � ������������ � ����������� locale ��� ��������� ��������� ������� � ����
void GuiGetFileType (TCHAR *ext, TCHAR *buf);                                                     // �������� ��� ����� �� ��� ����������

#ifdef  __cplusplus
}
#endif
