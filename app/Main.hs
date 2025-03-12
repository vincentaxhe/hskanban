module Main where

import           Actions
import qualified Brick.Main          as M (defaultMain)
import           Control.Monad       (void)
import           Options.Applicative
import           Types
import           Widgets

main :: IO ()
main =  void $ execParser opts >>= initApp >>= M.defaultMain app

-- ...existing code...

-- 修改为接收多个文件路径
input :: Parser InputType
input = FileInput
        <$> some (argument str
              (  metavar "FILE..."
              <> help "Files to use for persistence of Note data (first file is primary)"
              ))
        <|> pure (FileInput ["kanban.json"])  -- 默认值改为列表

-- ...existing code...
fullOptions :: Parser CmdOptions
fullOptions = CmdOptions <$> input

opts = info (fullOptions <**> helper)
        ( fullDesc
          <> progDesc "Kanban App with note, todo list and multi kanban offered"
          <> header "hskanban - Kanban on the terminal" )