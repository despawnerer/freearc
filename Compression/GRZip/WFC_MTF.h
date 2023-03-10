/*-------------------------------------------------*/
/* GRZipII/libGRZip compressor           WFC_MTF.h */
/* MTF and WFC Common Tables                       */
/*-------------------------------------------------*/

/*--
  This file is a part of GRZipII and/or libGRZip, a program
  and library for lossless, block-sorting data compression.

  Copyright (C) 2002-2003 Grebnov Ilya. All rights reserved.

  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public
  License as published by the Free Software Foundation; either
  version 2.1 of the License, or (at your option) any later version.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
  Lesser General Public License for more details.

  Grebnov Ilya, Ivanovo, Russian Federation.
  Ilya.Grebnov@magicssoft.ru, http://magicssoft.ru/

  This program is based on (at least) the work of:
  Juergen Abel, Jon L. Bentley, Edgar Binder,
  Charles Bloom, Mike Burrows, Andrey Cadach,
  Damien Debin, Sebastian Deorowicz, Peter Fenwick,
  Michael Schindler, Robert Sedgewick, Julian Seward,
  David Wheeler, Vadim Yoockin.

  For more information on these sources, see the manual.
--*/

#ifndef GRZip_WFC_MTF_Common_Tables_H
#define GRZip_WFC_MTF_Common_Tables_H

#include "libGRZip.h"

#define MaxByte 256

uint32 WFCMTF_Rank2GrNum[MaxByte] = {
    00, 00, 00, 00, 00, 01, 01, 01, 01, 02, 02, 02, 02, 02, 02, 02, 02, 03, 03,
    03, 03, 03, 03, 03, 03, 03, 03, 03, 03, 03, 03, 03, 03, 04, 04, 04, 04, 04,
    04, 04, 04, 04, 04, 04, 04, 04, 04, 04, 04, 04, 04, 04, 04, 04, 04, 04, 04,
    04, 04, 04, 04, 04, 04, 04, 04, 05, 05, 05, 05, 05, 05, 05, 05, 05, 05, 05,
    05, 05, 05, 05, 05, 05, 05, 05, 05, 05, 05, 05, 05, 05, 05, 05, 05, 05, 05,
    05, 05, 05, 05, 05, 05, 05, 05, 05, 05, 05, 05, 05, 05, 05, 05, 05, 05, 05,
    05, 05, 05, 05, 05, 05, 05, 05, 05, 05, 05, 05, 05, 05, 05, 06, 06, 06, 06,
    06, 06, 06, 06, 06, 06, 06, 06, 06, 06, 06, 06, 06, 06, 06, 06, 06, 06, 06,
    06, 06, 06, 06, 06, 06, 06, 06, 06, 06, 06, 06, 06, 06, 06, 06, 06, 06, 06,
    06, 06, 06, 06, 06, 06, 06, 06, 06, 06, 06, 06, 06, 06, 06, 06, 06, 06, 06,
    06, 06, 06, 06, 06, 06, 06, 06, 06, 06, 06, 06, 06, 06, 06, 06, 06, 06, 06,
    06, 06, 06, 06, 06, 06, 06, 06, 06, 06, 06, 06, 06, 06, 06, 06, 06, 06, 06,
    06, 06, 06, 06, 06, 06, 06, 06, 06, 06, 06, 06, 06, 06, 06, 06, 06, 06, 06,
    06, 06, 06, 06, 06, 06, 06, 06, 06};

uint32 WFCMTF_Rank2GrPos[MaxByte] = {
    0,   0,   0,   0,   1,   0,   1,   2,   3,   0,   1,   2,   3,   4,   5,
    6,   7,   0,   1,   2,   3,   4,   5,   6,   7,   8,   9,   10,  11,  12,
    13,  14,  15,  0,   1,   2,   3,   4,   5,   6,   7,   8,   9,   10,  11,
    12,  13,  14,  15,  16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,
    27,  28,  29,  30,  31,  0,   1,   2,   3,   4,   5,   6,   7,   8,   9,
    10,  11,  12,  13,  14,  15,  16,  17,  18,  19,  20,  21,  22,  23,  24,
    25,  26,  27,  28,  29,  30,  31,  32,  33,  34,  35,  36,  37,  38,  39,
    40,  41,  42,  43,  44,  45,  46,  47,  48,  49,  50,  51,  52,  53,  54,
    55,  56,  57,  58,  59,  60,  61,  62,  63,  0,   1,   2,   3,   4,   5,
    6,   7,   8,   9,   10,  11,  12,  13,  14,  15,  16,  17,  18,  19,  20,
    21,  22,  23,  24,  25,  26,  27,  28,  29,  30,  31,  32,  33,  34,  35,
    36,  37,  38,  39,  40,  41,  42,  43,  44,  45,  46,  47,  48,  49,  50,
    51,  52,  53,  54,  55,  56,  57,  58,  59,  60,  61,  62,  63,  64,  65,
    66,  67,  68,  69,  70,  71,  72,  73,  74,  75,  76,  77,  78,  79,  80,
    81,  82,  83,  84,  85,  86,  87,  88,  89,  90,  91,  92,  93,  94,  95,
    96,  97,  98,  99,  100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110,
    111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125,
    126};

uint32 WFCMTF_Rank2Mask[MaxByte] = {
    0,  0,  0,  1,  1,  2,  2,  2,  2,  4,  4,  4,  4,  4,  4,  4,  4,  8,  8,
    8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  16, 16, 16, 16, 16,
    16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16,
    16, 16, 16, 16, 16, 16, 16, 16, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32,
    32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32,
    32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32,
    32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 64, 64, 64, 64,
    64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64,
    64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64,
    64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64,
    64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64,
    64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64,
    64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64,
    64, 64, 64, 64, 64, 64, 64, 64, 64};

uint32 WFCMTF_GrNum2GrBegin[7] = {3, 5, 9, 17, 33, 65, 129};

uint32 WFCMTF_Log2RLESize[GRZ_Log2MaxBlockSize + 1] = {
    1,     2,      4,      8,      16,      32,      64,     128,
    256,   512,    1024,   2048,   4096,    8192,    16384,  32768,
    65536, 131072, 262144, 524288, 1048576, 2097152, 4194304};

#undef MaxByte

#endif

/*-------------------------------------------------*/
/* End                                   WFC_MTF.h */
/*-------------------------------------------------*/
