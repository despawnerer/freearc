How to compile FreeArc:

1. Install GNU make, gcc, gcc c++, curl-devel, ncurses-devel, GHC 6.10.4 (http://haskell.org/ghc/download_ghc_6_10_4.html),
   Package names for Fedora 12 are: make, gcc, gcc-c++, ghc, ghc-gtk2hs, curl-devel, ncurses-devel
2. Extract sources to some directory and go to this directory. Execute command:
     chmod +x compile*
3. Extract p7zip 9.04 sources into 7zip subdirectory of this directory:
     https://sourceforge.net/projects/p7zip/files/p7zip/9.04/p7zip_9.04_src_all.tar.bz2/download
5. Compile console version (arc will be placed to Tests directory):
     ./compile-O2
6. Compile Unarc:
     cd Unarc
     make unix
