include ../common.mak

unix: mkdir unarc

Extractor_DEFINES = -DFREEARC_DECOMPRESS_ONLY -D_NO_EXCEPTIONS -DUNARC
OBJDIR  = $(TEMPDIR)-unarc
LINKOBJ_FAR_PLUGIN = $(OBJDIR)/Environment.o $(OBJDIR)/Common.o $(OBJDIR)/CompressionLibrary.o \
                     $(OBJDIR)/C_LZMA.o
LINKOBJ_TINY = $(LINKOBJ_FAR_PLUGIN) $(OBJDIR)/C_BCJ.o $(OBJDIR)/C_Dict.o $(OBJDIR)/C_Delta.o
LINKOBJ_MINI = $(LINKOBJ_TINY) $(OBJDIR)/C_REP.o $(OBJDIR)/C_LZP.o $(OBJDIR)/C_PPMD.o $(OBJDIR)/C_External.o
LINKOBJ      = $(LINKOBJ_MINI) $(OBJDIR)/C_MM.o $(OBJDIR)/C_TTA.o $(OBJDIR)/C_Tornado.o $(OBJDIR)/C_GRZip.o $(OBJDIR)/C_4x4.o \
                               $(OBJDIR)/C_DisPack.o $(OBJDIR)/C_LZ4.o $(OBJDIR)/C_Encryption.o $(OBJDIR)/C_CLS.o

UNARC = ArcStructure.h ArcCommand.h ArcProcess.h
CUI = CUI.h
HEADERS =  ../Compression/Compression.h ../Compression/Common.h

LINK_FLAGS = -L$(LIBDIR) -Xlinker --large-address-aware -lstdc++ -s
UNIX_LINK_FLAGS = -L$(LIBDIR) -lstdc++ -lrt -lpthread -s

CONSOLE_LINK_FLAGS = $(LINK_FLAGS) -lole32


CODE_FLAGS  = -fno-exceptions -fno-rtti -Wall \
              -Wno-unknown-pragmas -Wno-sign-compare -Wno-conversion
OPT_FLAGS   = -march=i486 -mtune=pentiumpro -ffast-math \
              -fomit-frame-pointer -fstrict-aliasing \
              -fforce-addr
DEBUG_FLAGS = -g0
CFLAGS = $(CODE_FLAGS) $(OPT_FLAGS) -O2 $(DEBUG_FLAGS) $(DEFINES) $(Extractor_DEFINES)
CFLAGS1= $(CODE_FLAGS) $(OPT_FLAGS) -O1 $(DEBUG_FLAGS) $(DEFINES) $(Extractor_DEFINES)

#No more use UPX due to problems with antivirus software
UPX = cmd /c rem
#UPX = upx.exe -9 --lzma --compress-icons=2

mkdir:
	-mkdir $(OBJDIR)

# Executable
unarc: $(OBJDIR)/unarc.o $(LINKOBJ)
	$(GCC) $< $(LINKOBJ) $(UNIX_LINK_FLAGS) -o $@


# Object files
$(OBJDIR)/unarc.o: unarc.cpp $(UNARC) $(CUI) $(HEADERS)
	$(GCC) -c $(CFLAGS) -DUNARC_DECRYPTION -o $*.o $<

$(OBJDIR)/Environment.o: ../Environment.cpp ../Environment.h $(HEADERS)
	$(GCC) -c $(CFLAGS1) -o $*.o $<

$(OBJDIR)/CompressionLibrary.o: ../Compression/CompressionLibrary.cpp $(HEADERS)
	$(GCC) -c $(CFLAGS1) -o $*.o $<

$(OBJDIR)/Common.o: ../Compression/Common.cpp $(HEADERS)
	$(GCC) -c $(CFLAGS1) -o $*.o $<

$(OBJDIR)/C_External.o: ../Compression/External/C_External.cpp ../Compression/External/C_External.h $(HEADERS)
	$(GCC) -c $(CFLAGS) -o $*.o $<


$(OBJDIR)/C_4x4.o: ../Compression/4x4/C_4x4.cpp ../Compression/4x4/C_4x4.h ../Compression/MultiThreading.h $(HEADERS)
	$(GCC) -c $(CFLAGS) -o $*.o $<

$(OBJDIR)/C_DisPack.o: ../Compression/DisPack/C_DisPack.cpp ../Compression/DisPack/C_DisPack.h $(HEADERS)
	$(GCC) -c $(CFLAGS) -o $*.o $<

$(OBJDIR)/C_BCJ.o: ../Compression/LZMA2/C_BCJ.cpp ../Compression/LZMA2/C_BCJ.h $(HEADERS)
	$(GCC) -c $(CFLAGS) -o $*.o $<

$(OBJDIR)/C_LZMA.o: ../Compression/LZMA2/C_LZMA.cpp ../Compression/LZMA2/C_LZMA.h $(HEADERS)
	$(GCC) -c $(CFLAGS) -o $*.o $<

$(OBJDIR)/C_Dict.o: ../Compression/Dict/C_Dict.cpp ../Compression/Dict/C_Dict.h $(HEADERS)
	$(GCC) -c $(CFLAGS) -o $*.o $<

$(OBJDIR)/C_Delta.o: ../Compression/Delta/C_Delta.cpp ../Compression/Delta/C_Delta.h $(HEADERS)
	$(GCC) -c $(CFLAGS) -o $*.o $<

$(OBJDIR)/C_REP.o: ../Compression/REP/C_REP.cpp ../Compression/REP/C_REP.h ../Compression/REP/rep.cpp $(HEADERS)
	$(GCC) -c $(CFLAGS) -o $*.o $<

$(OBJDIR)/C_LZP.o: ../Compression/LZP/C_LZP.cpp ../Compression/LZP/C_LZP.h $(HEADERS)
	$(GCC) -c $(CFLAGS) -o $*.o $<

$(OBJDIR)/C_PPMD.o: ../Compression/PPMD/C_PPMD.cpp ../Compression/PPMD/C_PPMD.h $(HEADERS)
	$(GCC) -c $(CFLAGS) -O1 -o $*.o $<

$(OBJDIR)/C_MM.o: ../Compression/MM/C_MM.cpp ../Compression/MM/C_MM.h $(HEADERS)
	$(GCC) -c $(CFLAGS) -o $*.o $<

$(OBJDIR)/C_TTA.o: ../Compression/MM/C_TTA.cpp ../Compression/MM/C_TTA.h $(HEADERS)
	$(GCC) -c $(CFLAGS) -o $*.o $<

$(OBJDIR)/C_Tornado.o: ../Compression/Tornado/C_Tornado.cpp ../Compression/Tornado/C_Tornado.h $(HEADERS)
	$(GCC) -c $(CFLAGS) -o $*.o $<

$(OBJDIR)/C_GRZip.o: ../Compression/GRZip/C_GRZip.cpp ../Compression/GRZip/C_GRZip.h $(HEADERS)
	$(GCC) -c $(CFLAGS) -o $*.o $<

$(OBJDIR)/C_LZ4.o: ../Compression/LZ4/C_LZ4.cpp ../Compression/LZ4/C_LZ4.h ../Compression/LZ4/lz4.c $(HEADERS)
	$(GCC) -c $(CFLAGS) -o $*.o $<

$(OBJDIR)/C_Encryption.o: ../Compression/_Encryption/C_Encryption.cpp ../Compression/_Encryption/C_Encryption.h $(HEADERS)
	$(GCC) -c $(CFLAGS) -I../Compression/_Encryption/headers -o $*.o $<

$(OBJDIR)/C_CLS.o: ../Compression/CLS/C_CLS.cpp ../Compression/CLS/C_CLS.h $(HEADERS)
	$(GCC) -c $(CFLAGS) -fexceptions -o $*.o $<
