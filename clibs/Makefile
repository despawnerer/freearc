include common.mak

ALL: $(TEMPDIR)/Environment.o $(TEMPDIR)/URL.o

CODE_FLAGS = -fno-exceptions -fno-rtti -Wall \
                -Wno-unknown-pragmas -Wno-sign-compare -Wno-conversion
OPT_FLAGS   = -O3 \
              -fomit-frame-pointer -fstrict-aliasing \
              -ffast-math -fforce-addr
DEBUG_FLAGS = -g0
CFLAGS = $(CODE_FLAGS) $(OPT_FLAGS) $(DEBUG_FLAGS) $(DEFINES)

$(TEMPDIR)/Environment.o:  Environment.cpp Environment.h Compression/Common.h Makefile
	$(GCC) -c $(CFLAGS) -fexceptions -o $*.o $<

$(TEMPDIR)/URL.o:  URL.cpp URL.h Compression/Common.h Makefile
	$(GCC) -c $(CFLAGS) -o $*.o $<

clean:
	rm -rf $(TEMPDIR)-unarc/*.o
	rm -rf $(TEMPDIR)/*.o
	rm -rf $(TEMPDIR)/*.hi
	rm -rf $(TEMPDIR)-O2/*.o
	rm -rf $(TEMPDIR)-O2/*.hi
