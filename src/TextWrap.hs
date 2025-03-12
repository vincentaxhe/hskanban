module TextWrap 
  ( wrapText
  , renderWrappedText
  ) where

import qualified Data.Text as T
import           Brick.Types
import           Brick.Widgets.Core
import           Data.Char (ord)

-- 改进的文本折行函数，严格考虑显示宽度
wrapText :: Int -> T.Text -> [T.Text]
wrapText width text = 
  if T.null text
    then []
    else let (line, rest) = splitAtDisplayWidth width text
         in line : wrapText width rest

-- 判断字符是否为宽字符（CJK等）
isWideChar :: Char -> Bool
isWideChar c =
    let cp = ord c
    in  (cp >= 0x1100 && cp <= 0x11FF) ||  -- 朝鲜文字母
        (cp >= 0x2E80 && cp <= 0xA4CF) ||  -- CJK部首和笔画、康熙部首等
        (cp >= 0xAC00 && cp <= 0xD7AF) ||  -- 朝鲜文音节
        (cp >= 0xF900 && cp <= 0xFAFF) ||  -- CJK兼容表意文字
        (cp >= 0xFF00 && cp <= 0xFFEF) ||  -- 全角ASCII、全角标点等
        (cp >= 0x4E00 && cp <= 0x9FFF) ||  -- CJK统一表意文字
        (cp >= 0x3000 && cp <= 0x30FF) ||  -- CJK符号和标点、日文平假名、片假名
        (cp >= 0x20000 && cp <= 0x2FFFF)   -- CJK统一表意文字扩展B-G

-- 计算字符的显示宽度
charWidth :: Char -> Int
charWidth c = if isWideChar c then 2 else 1

-- 计算文本的显示宽度
textDisplayWidth :: T.Text -> Int
textDisplayWidth = T.foldl' (\acc c -> acc + charWidth c) 0

-- 按显示宽度拆分文本，确保不会超出边界
splitAtDisplayWidth :: Int -> T.Text -> (T.Text, T.Text)
splitAtDisplayWidth width text =
  if textDisplayWidth text <= width 
    then (text, T.empty)
    else splitByDisplayWidth width text

-- 改进的按显示宽度拆分方法
splitByDisplayWidth :: Int -> T.Text -> (T.Text, T.Text)
splitByDisplayWidth width text =
  let
    -- 判断是否为可分割字符（空格或标点）
    isSplitChar c = c == ' ' || c == ',' || c == '.' || c == '。' || c == '，' || c == '、' || 
                   c == '；' || c == '：' || c == '!' || c == '！' || c == '?' || c == '？'
    
    -- 贪婪收集满足宽度的字符，同时记录最后一个分割点
    collectChars :: Int -> Int -> Maybe Int -> Maybe Int -> (Int, Maybe Int)
    collectChars i curWidth lastSplitPos lastWideCharPos
      | i >= T.length text = (i, lastSplitPos <|> lastWideCharPos)
      | newWidth > width = (i, lastSplitPos <|> lastWideCharPos <|> Just (i-1))
      | otherwise = 
          let 
            char = T.index text i
            newSplitPos = if isSplitChar char then Just i else lastSplitPos
            newWideCharPos = if isWideChar char then Just i else lastWideCharPos
          in collectChars (i+1) newWidth newSplitPos newWideCharPos
      where
        newWidth = curWidth + charWidth (T.index text i)
    
    -- 主要逻辑
    (endPos, maybeSplitPos) = collectChars 0 0 Nothing Nothing
    
    -- 确定最终的分割位置
    finalSplitPos = case maybeSplitPos of
                      Just pos -> pos + 1 -- 分割点后一个字符，保留分割点在当前行
                      Nothing -> max 1 endPos -- 至少取一个字符，防止无限循环
  in
    (T.take finalSplitPos text, T.drop finalSplitPos text)
  where
    -- 定义 <|> 运算符，类似于 Maybe 的 <|> 操作
    Nothing <|> y = y
    x <|> _ = x

-- 渲染已折行的文本
renderWrappedText :: T.Text -> Int -> Widget n
renderWrappedText text width = 
  vBox $ map txt $ wrapText (max 1 width) text  -- 确保宽度至少为1