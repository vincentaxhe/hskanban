module KbUtils
  ( generateViewportId,
    findDuplicateNoteIndex,
    generateTodoViewportId,
    generateTaskId
  ) where

import Data.Digest.CRC32 (crc32)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import Types (Note(..), Task(..))
import Data.List (findIndex)

-- | 基于笔记标题和内容生成唯一的视口ID
-- 将笔记的标题和内容转换为字节串，并计算CRC32校验和
generateViewportId :: Note -> Int
generateViewportId note =
  let titleContent = T.unpack (_title note) ++ T.unpack (_content note)
      byteString = BS.pack titleContent
  in fromIntegral $ crc32 byteString

-- | 为Todo列表生成视口ID
-- 基于普通视口ID但有不同的哈希计算方式以区分
generateTodoViewportId :: Note -> Int -> Int
generateTodoViewportId note idx =
  let baseId = generateViewportId note * 13 + 7  -- 使用乘13加7来区分普通笔记内容视口
  in baseId

-- | 查找相同内容的笔记索引
-- 在给定的笔记列表中查找与指定索引位置笔记具有相同内容的笔记
-- 只返回第一个出现在当前索引之前的笔记索引
findDuplicateNoteIndex :: [Note] -> Int -> Maybe Int
findDuplicateNoteIndex notes idx
  | idx < 0 || idx >= length notes = Nothing
  | otherwise =
      let currentNote = notes !! idx
          currentId = generateViewportId currentNote

          -- 在当前索引之前查找具有相同视口ID的笔记
          isDuplicate i note = i < idx && generateViewportId note == currentId
      in findIndex (uncurry isDuplicate) (zip [0..] notes)

generateTaskId :: Task -> Int
generateTaskId task =
  let taskContent = T.unpack (_task task)
      byteString = BS.pack taskContent
  in fromIntegral $ crc32 byteString