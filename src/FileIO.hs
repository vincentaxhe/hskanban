module FileIO (
  getKanbanData,
  saveKanbanData,
  createEmptyKanban,
  decodeOptions,
  saveAndExit,
  saveAndUpdateState,
  getKanbanTitleFromPath
) where

import           Control.Exception
import           Control.Monad          (when, unless)
import           Data.Aeson             (eitherDecode, encode)

import           System.Directory       (doesFileExist)
import           Data.List              as L (isSuffixOf)
import           Brick.Types
import qualified Brick.Main                 as M
import           Lens.Micro    ((^.))
import Control.Monad.IO.Class ( MonadIO(liftIO) )
import           Data.ByteString.Lazy   as BL (readFile, writeFile)
import System.FilePath (takeFileName, takeBaseName)
import Data.Text as T (pack, Text)
import           Types
import           Task

-- 创建默认空看板，使用文件名（无扩展名）作为标题
createEmptyKanban :: FilePath -> KanbanData
createEmptyKanban path = KanbanData [Kanban {
  _kanbanTitle = getKanbanTitleFromPath path,
  _childs = [],
  _kanbanSelected = True
}]

-- 从文件路径提取看板标题
getKanbanTitleFromPath :: FilePath -> Text
getKanbanTitleFromPath path =
  let
    -- 获取文件名（不含路径）
    fileName = takeFileName path
    -- 去除扩展名
    baseName = takeBaseName fileName
  in
    -- 如果结果为空，返回 "New Kanban"，否则返回文件名
    if null baseName
    then T.pack "New Kanban"
    else T.pack baseName

-- 保存数据并退出应用
saveAndExit :: AppState e Name -> EventM Name (Next (AppState e Name))
saveAndExit st = do
    -- 使用 saveAndUpdateState 函数来保存数据
    _ <- saveAndUpdateState st
    -- 退出应用
    M.halt st

-- 创建备份文件，添加~后缀，但避免对已经是备份的文件再次添加后缀
createBackup :: FilePath -> IO ()
createBackup path
  | "~" `L.isSuffixOf` path = return ()
  | otherwise = do
      let backupPath = path ++ "~"
      fileExists <- doesFileExist path
      when fileExists $ do
        contents <- BL.readFile path
        BL.writeFile backupPath contents

-- 修改为支持多文件输入
decodeOptions :: CmdOptions -> IO ([FileInfo], KanbanData)
decodeOptions CmdOptions{inputType = FileInput paths} = do
  -- 处理每个文件并收集结果
  filesWithData <- mapM processFile paths

  -- 提取所有看板数据并合并
  let allKanbans = concatMap snd filesWithData
      mergedKanbanData = KanbanData allKanbans

  -- 提取文件信息
  let fileInfos = [FileInfo path (length kanbans) | (path, kanbans) <- filesWithData]

  -- 返回文件信息列表和合并后的看板数据
  return (fileInfos, mergedKanbanData)
  where
    -- 处理单个文件并返回 (路径, 看板列表)
    processFile :: FilePath -> IO (FilePath, [Kanban])
    processFile path = do
      fileExists <- doesFileExist path
      kanbanData <- if fileExists
        then getKanbanData path
        else do
          -- 文件不存在时创建一个空文件
          let emptyKanban = createEmptyKanban path
          BL.writeFile path $ encode emptyKanban
          return emptyKanban
      return (path, _kanbans kanbanData)

-- 更新从文件读取看板数据的函数
getKanbanData :: FilePath -> IO KanbanData
getKanbanData path = do
  -- 在读取前创建备份文件
  createBackup path
  -- 读取文件内容
  contents <- BL.readFile path `catch` \e -> do
                putStrLn $ "Error reading file: " ++ show (e :: IOException)
                error "Failed to read kanban data file"

  -- 解析为完整的KanbanData格式，确保解析整个结构
  case eitherDecode contents :: Either String KanbanData of
    Right kanbanData ->
      -- 成功解析后，更新所有任务的级别信息
      return $ updateKanbanTaskLevels kanbanData
    Left err -> do
      putStrLn $ "Error: Invalid JSON format in file " ++ path
      putStrLn $ "Details: " ++ err
      putStrLn $ "Hint: Check the backup file at " ++ path ++ "~ for recovery"
      error "Failed to parse kanban data file"


saveAndUpdateState :: AppState e Name -> EventM Name (AppState e Name)
saveAndUpdateState st = do
  -- 从状态中的 currentLocation 读取看板数量列表
  let lengthList = st ^. (notes . currentLocation . kanbanLengthList)
      -- 取原有文件信息列表，并用 _kanbanLengthList 中的数值更新每个文件的 _kanbanCount 字段
      newFileInfos = zipWith (\fi cnt -> fi { _kanbanCount = cnt }) (st ^. fileInfos) lengthList
  -- 如果有文件信息，则保存看板数据到各文件中
  unless (null newFileInfos) $
    liftIO $ saveKanbanDataToFiles newFileInfos (st ^. (notes . kanbanData))
  return st


-- 保存看板数据到多个文件
saveKanbanDataToFiles :: [FileInfo] -> KanbanData -> IO ()
saveKanbanDataToFiles fileInfos kanbanData = do
  let allKanbans = _kanbans kanbanData
  saveToFiles fileInfos allKanbans 0
  where
    saveToFiles :: [FileInfo] -> [Kanban] -> Int -> IO ()
    saveToFiles [] _ _ = return ()
    saveToFiles (info:rest) kanbans offset = do
      -- 提取当前文件应该包含的看板
      let count = _kanbanCount info
          path = _filePath info
          fileKanbans = take count $ drop offset kanbans
          fileKanbanData = KanbanData fileKanbans

      -- 保存到当前文件
      saveKanbanData path fileKanbanData

      -- 处理下一个文件
      saveToFiles rest kanbans (offset + count)


-- 保存看板数据到文件
saveKanbanData :: FilePath -> KanbanData -> IO ()
saveKanbanData path kanbanData =
  BL.writeFile path $ encode kanbanData