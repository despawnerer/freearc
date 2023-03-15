----------------------------------------------------------------------------------------------------
---- Работа с архивами через 7z.dll.                                                          ------
---- Этот модуль содержит процедуры для:                                                      ------
----   * чтения структуры входного архива (т.е. каталогов и других служебных блоков)          ------
----   * распаковки архивов                                                                   ------
----------------------------------------------------------------------------------------------------
module Arhive7zLib where

import Prelude hiding (catch)
import Control.Concurrent
import Control.Monad
import Control.Exception
import OldHash as Hash
import Data.Char
import Data.Int
import Data.List
import Data.Maybe
import Data.Word
import Foreign.C
import Foreign.Marshal
import Foreign.Ptr
import Foreign.Storable
import System.Mem
import System.IO.Unsafe

import TABI
import Utils
import Errors
import Files
import qualified ByteStream
import Charsets
import FileInfo
import CompressionLib   (aFREEARC_OK, aFREEARC_ERRCODE_NOT_IMPLEMENTED, compressionQuery)
import Compression
import Encryption       (generateSzDecryption)
import UI
import Options
import ArhiveStructure

----------------------------------------------------------------------------------------------------
---- Описание структуры входного архива ------------------------------------------------------------
----------------------------------------------------------------------------------------------------

-- |Вся необходимая информация о входном архиве
data ArchiveInfo = ArchiveInfo
         { arcArchive     :: !(Maybe Archive)    -- открытый файл архива .arc
         , arcFooter      :: FooterBlock         -- FOOTER BLOCK архива
         , arcDirectory   :: [CompressedFile]    -- файлы, содержащиеся в архиве
         , arcDataBlocks  :: [ArchiveBlock]      -- список солид-блоков
         , arcDirBytes    :: FileSize            -- размер служебных блоков в распакованном виде
         , arcDirCBytes   :: FileSize            -- размер служебных блоков в упакованном виде
         , arcDataBytes   :: FileSize            -- размер данных в распакованном виде
         , arcDataCBytes  :: FileSize            -- размер данных в упакованном виде
         , arcArchiveType :: String              -- Тип архива
         }

-- |Проверка, что это архив, поддерживаемый самим FreeArc
isArcArchive  =  isJust . arcArchive

-- |True, если архива на самом деле нет (используется для main_archive)
isArcPhantom = not . any_function [isArcArchive]

-- Процедуры, упрощающие работу с архивами
arcGetPos  = archiveGetPos . fromJust . arcArchive
arcSeek    = archiveSeek   . fromJust . arcArchive
arcComment = ftComment . arcFooter

-- |Фантомный, несуществующий архив, необходимый для применения в некоторых операциях
-- (слияние списков файлов, закрытие входных архивов)
phantomArc  =  dirlessArchive Nothing (FooterBlock [] False "" "" 0)

-- |Архив без каталога файлов - используется только для вызова writeSFX из runArchiveRecovery
dirlessArchive archive footer   =   ArchiveInfo archive footer [] [] (error "emptyArchive:arcDirBytes") (error "emptyArchive:arcDirCBytes") (error "emptyArchive:arcDataBytes") (error "emptyArchive:arcDataCBytes") "-"

-- |Закрыть архивный файл, если только это не фантомный архив
arcClose arc  =  do whenJust (arcArchive arc) archiveClose
                    return ()

----------------------------------------------------------------------------------------------------
---- Получение тех. информации об архиве в виде текстового блока -----------------------------------
----------------------------------------------------------------------------------------------------

arcGetTechinfo archive dirs_and_files = do
    let filelist    = map cfFileInfo (arcDirectory archive)
        footer      = arcFooter archive
        dataBlocks  = arcDataBlocks archive                                                         -- список солид-блоков
        numOfBlocks = length dataBlocks
        empty       = "-"
        arc x       = isArcArchive archive &&& x                                                    -- включить строчку x только для архивов .arc
        a7z x       = (isArcArchive archive || arcArchiveType archive=="7z")  &&&  x                -- .. только для .arc и .7z
        ifArc       = iif (isArcArchive archive)
    ;   yes        <- i18n"0101 Yes" >>== replaceAll "_" ""

    let origsize = arcDataBytes  archive                                                            -- суммарный объём файлов в распакованном виде
        compsize = arcDataCBytes archive                                                            -- суммарный объём файлов в упакованном виде
        getCompressors = partition isEncryption.blCompressor                                        -- разделить алг-мы шифрования и сжатия для блока
        (encryptors, tmp_compressors) = unzip$ map getCompressors dataBlocks                        -- списки алг. шифрования и сжатия
        header_encryptors = deleteIf null$ map (fst.getCompressors) (ftBlocks footer)               -- алгоритмы шифрования служебных блоков
        all_encryptors = deleteIf null encryptors ++ header_encryptors                              -- а теперь все вместе :)
        ciphers = joinWith "\n"$ removeDups$ map (join_compressor.map method_name) all_encryptors   -- имена алг. шифрования.
        ciphers_text = [("0097 Encryption algorithms:",  ciphers ||| empty)]

    let -- Максимальные словари алгоритмов сжатия
        compressors  = tmp_compressors.$ id
        dictionaries = compressors
                       .$ map (map algomem)                                      -- каждый метод сжатия превращаем в алгоритм+квазисловарь
                       .$ (filter (not.null) . (map (filter ((>0).cmAlgoMem))))                     -- оставляем только методы с ненулевым словарём и состоящие из них непустые группы
                       .$ id
                       .$ sort_and_groupOn (map cmAlgorithm)                                        -- группируем по сочетанию алгоритмов
                       .$ map (maxOn (maximum.map cmAlgoMem))                                       -- выбираем в каждой группе по максимуму любого из словарей
                       .$ Utils.sortOn (negate.maximum.map cmAlgoMem)                                     -- выносим вперёд цепочки с наибольшими словарями
                       .$ (joinWith " " . map (join_compressor . map showAlgoMem))                  -- репрезентация
        formatMem s =  x++" "++y  where (x,y) = span isDigit$ showMem s

    return$ filter(not.null) . map (concatMap (\x -> fst x &&& [x]))                                -- удаление пустых строк для не-.arc архивов
          $ [[(    "0465 Archive type:",          arcArchiveType archive)]
            ++ dirs_and_files ++
            [(    "0089 Total bytes:",           show3$ origsize)
            ,(    "0090 Compressed bytes:",      show3$ compsize)
            ,(    "0091 Ratio:",                 compression_ratio compsize origsize)]

           ,[(arc "0104 Directory blocks:",      show3$ length$ filter ((DIR_BLOCK==).blType) (ftBlocks footer))
            ,(arc "0463 Directory, bytes:",      show3$ arcDirBytes archive)
            ,(a7z "0464 Directory, compressed:", show3$ arcDirCBytes archive)
            ,(a7z "0092 Solid blocks:",          show3$ numOfBlocks)
            ,(a7z "0093 Avg. blocksize:",        formatMem$ origsize `div` i(max numOfBlocks 1))]

           ,a7z [("0099 Compression memory:",    formatMem$ maximum$ 0: map getMinCompressionMem   compressors)
                ,("0100 Decompression memory:",  formatMem$ maximum$ 0: map getMinDecompressionMem compressors)
                ,("0105 Dictionary:",            dictionaries ||| empty)]

           ,arc [("0094 Archive locked:",        ftLocked footer   &&& yes ||| empty)
                ,("0098 Archive comment:",       ftComment footer  &&& yes ||| empty)
                ,("0095 Recovery info:",         ftRecovery footer ||| empty)
                ,("0096 SFX size:",              ftSFXSize footer .$show3 .$changeTo [("0", empty)])
                ,("0156 Headers encrypted:",     header_encryptors &&& yes ||| empty)]
             ++ arc (if ciphers>""   then []   else ciphers_text)
           ] ++ arc (ciphers &&& [ciphers_text])



-- Структура данных для хранения алгоритма+словаря метода сжатия
data AlgoMem  =  AlgoMem {cmAlgorithm :: !String,  cmAlgoMem :: !Integer,  cmCompressionMem :: Integer,  cmDecompressionMem :: Integer}

showAlgoMem AlgoMem{..}  =  join_method [cmAlgorithm, showMem cmAlgoMem]

-- | Вернуть алгоритм+квазисловарь. Для 4x4 алгоритм записываем как xПОДАЛГОРИТМ, а словарь берём из подалгоритма
algomem method  =  AlgoMem { cmAlgorithm        = name
                           , cmAlgoMem          = i$compressionQuery (const 0) "GetAlgoMem" name_with_params
                           , cmCompressionMem   = error "cmCompressionMem"
                           , cmDecompressionMem = error "cmDecompressionMem"
                           }
                       where (name,name_with_params) = case (split_method method) of
                                                         "4x4":m | name_with_params@(name:_) <- subMethod4x4 m  ->  ("x"++name, join_method name_with_params)
                                                         name:_                                                 ->  (name, method)

-- |Подметод в 4x4 (удаляем из начала списка параметров имеющие вид \d.* или [a-z]\d.*, остаток и есть подметод 4x4)
subMethod4x4  =  dropWhile (\param -> (length(param)>0 && isDigit (param!!0))  ||  (length(param)>1 && isDigit (param!!1)))

----------------------------------------------------------------------------------------------------
---- Helper functions ------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

-- |Выполнить action с сериализованным в память массивом строк
withCWStringArray strings action =
  bracket (Control.Monad.mapM newCWString strings)
          (Control.Monad.mapM_ free)
          (\array -> withArray0 nullPtr array action)

breakHandling_need_initialisation = unsafePerformIO$ ref True


----------------------------------------------------------------------------------------------------
---- Constants -------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

-- |NArchive::NExtract::NAskMode
kExtract:
  kTest:
  kSkip:_ = [0..] :: [Int]

-- |NArchive::NExtract::NOperationResult
kOK:
  kUnSupportedMethod:
  kDataError:
  kCRCError:_ = [0..] :: [Int]

-- |7-zip property IDs
kpidNoProperty:
  kpidMainSubfile:
  kpidHandlerItemIndex:
  kpidPath:
  kpidName:
  kpidExtension:
  kpidIsDir:
  kpidSize:
  kpidPackSize:
  kpidAttrib:
  kpidCTime:
  kpidATime:
  kpidMTime:
  kpidSolid:
  kpidCommented:
  kpidEncrypted:
  kpidSplitBefore:
  kpidSplitAfter:
  kpidDictionarySize:
  kpidCRC:
  kpidType:
  kpidIsAnti:
  kpidMethod:
  kpidHostOS:
  kpidFileSystem:
  kpidUser:
  kpidGroup:
  kpidBlock:
  kpidComment:
  kpidPosition:
  kpidPrefix:
  kpidNumSubDirs:
  kpidNumSubFiles:
  kpidUnpackVer:
  kpidVolume:
  kpidIsVolume:
  kpidOffset:
  kpidLinks:
  kpidNumBlocks:
  kpidNumVolumes:
  kpidTimeType:
  kpidBit64:
  kpidBigEndian:
  kpidCpu:
  kpidPhySize:
  kpidHeadersSize:
  kpidChecksum:
  kpidCharacts:
  kpidVa:
  kpidId:
  kpidShortName:
  kpidCreatorApp:
  kpidSectorSize:
  kpidPosixAttrib:
  kpidLink:_ = [0..]


----------------------------------------------------------------------------------------------------
---- Упаковываемый файл (или с диска, или из уже существующего архива) -----------------------------
----------------------------------------------------------------------------------------------------

-- |File to compress: either file on disk or compressed file in existing archive
data FileToCompress
  = DiskFile
      { cfFileInfo           :: !FileInfo
      }
  | CompressedFile
      { cfFileInfo           :: !FileInfo
      , cfArcBlock           ::  ArchiveBlock   -- Archive datablock which contains file data
      , cfPos                ::  FileSize       -- Starting byte of file data in datablock
      , cfCRC :: {-# UNPACK #-} !CRC            -- File's CRC
      }

-- |Assign type synonym because variant label can't be used in another types declarations
type CompressedFile = FileToCompress


-- |Проверка того, что упаковываемый файл - из уже существующего архива, а не с диска
isCompressedFile CompressedFile{} = True
isCompressedFile DiskFile{}       = False

-- |Алгоритм сжатия, использованный для данного (сжатого) файла
cfCompressor cf = blCompressor (cfArcBlock cf)

-- |Это сжатый файл, использующий фейковый метод компрессии?
isCompressedFake file  =  isCompressedFile file  &&  isFakeCompressor (cfCompressor file)

-- |Это запаролированный файл?
cfIsEncrypted cf  =  blIsEncrypted (cfArcBlock cf)   -- определяем по методу сжатия солид-блока

-- |Определить тип файла по группе, если она не проставлена - вычислить по имени
cfType command file | group/=fiUndefinedGroup  =  opt_group2type command group
                    | otherwise                =  opt_find_type command fi
                                                    where fi    = cfFileInfo file
                                                          group = fiGroup fi


----------------------------------------------------------------------------------------------------
---- Файл и его CRC - используется для передачи результатов упаковки -------------------------------
----------------------------------------------------------------------------------------------------

-- |File and it's CRC
data FileWithCRC = FileWithCRC { fwCRC  :: {-# UNPACK #-} !CRC
                               , fwType :: {-# UNPACK #-} !FileType
                               , fwFileInfo            :: !FileInfo
                               }

data FileType = FILE_ON_DISK | FILE_IN_ARCHIVE  deriving (Eq)

-- |Проверка того, что упакованный файл - из исходного архива, а не с диска
isFILE_ON_DISK fw  =  fwType fw == FILE_ON_DISK

-- |Convert FileToCompress to FileWithCRC
fileWithCRC (DiskFile       fi)          =  FileWithCRC 0   FILE_ON_DISK    fi
fileWithCRC (CompressedFile fi _ _ crc)  =  FileWithCRC crc FILE_IN_ARCHIVE fi
