module AppState (
    syncAppStateFocus,
    selectNoteByIndex,
    getCurrentKanbanIndex,
    updateLocationFromIndex,
    syncNotesToKanban,
    globalToLocalIndex,
    syncAndSave
) where
import qualified Brick.Main             as M (continue)
import Brick.Types (EventM,Next)
import Types
import Lens.Micro
import FileIO

-- | 根据 KanbanLocation 获取当前看板在全局列表中的索引
getCurrentKanbanIndex :: KanbanLocation -> Int
getCurrentKanbanIndex location =
  let fileIdx = location^.kanbanFileIndex
      localIdx = location^.kanbanLocalIndex
      lengthList = location^.kanbanLengthList
      -- 计算当前文件之前的所有看板数量总和
      previousFilesLength = sum $ take fileIdx lengthList
  in
    -- 当前索引 = 之前所有文件的看板总数 + 当前文件内的索引
    previousFilesLength + localIdx

-- | 将全局看板索引转换为文件索引和文件内本地索引
globalToLocalIndex :: Int -> [Int] -> (Int, Int)
globalToLocalIndex globalIdx lengthList =
  let
    -- 递归辅助函数，查找包含指定全局索引的文件
    findFile :: Int -> Int -> Int -> (Int, Int)
    findFile remainingIdx fileIdx accum
      -- 如果已处理的看板数量 + 当前文件的看板数量 > 全局索引
      -- 说明该看板位于当前文件中
      | accum + (lengthList !! fileIdx) > remainingIdx =
          -- 文件索引,本地索引(全局索引减去之前累积的看板数)
          (fileIdx, remainingIdx - accum)
      -- 否则继续查找下一个文件
      | fileIdx < length lengthList - 1 =
          findFile remainingIdx (fileIdx + 1) (accum + (lengthList !! fileIdx))
      -- 已到达最后一个文件，但仍未找到，返回最后位置
      | otherwise =
          (fileIdx, (lengthList !! fileIdx) - 1)
  in
    -- 从第一个文件开始查找
    findFile globalIdx 0 0

-- | 根据全局看板索引更新 KanbanLocation
updateLocationFromIndex :: Int -> KanbanLocation -> KanbanLocation
updateLocationFromIndex globalIdx location =
  let
    lengthList = location^.kanbanLengthList
    (fileIdx, localIdx) = globalToLocalIndex globalIdx lengthList
  in
    location & kanbanFileIndex .~ fileIdx
            & kanbanLocalIndex .~ localIdx

-- 增强版 syncAppStateFocus 函数，确保全局状态一致性
syncAppStateFocus :: AppState e Name -> AppState e Name
syncAppStateFocus st =
  let
    -- 1. 同步笔记选择相关状态
    noteCount = st^.notes.totalNotes
    oldIndex  = st^.selectedIndex
    newIndex
      | noteCount == 0 = -1
      | oldIndex >= noteCount = noteCount - 1
      | oldIndex < 0 = 0  -- 确保当有笔记时，索引不为负
      | otherwise = oldIndex

    -- 清除所有笔记的选中状态
    stCleared = st & (notes . noteData . each . selected) .~ False

    -- 设置新的选中状态
    stWithSelectedNote = if newIndex >= 0 && newIndex < noteCount
                then stCleared & selectedIndex .~ newIndex
                               & (notes . noteData . ix newIndex . selected) .~ True
                else stCleared & selectedIndex .~ newIndex

    -- 2. 确保 currentLocation 和 currentKanban 一致
    curLocation = stWithSelectedNote^.notes.currentLocation
    calculatedKanbanIndex = getCurrentKanbanIndex curLocation
    stWithSyncedKanbanIndex = stWithSelectedNote & notes.currentKanban .~ calculatedKanbanIndex

    -- 3. 确保当前看板的 childs 与 noteData 同步
    kanbanList = stWithSyncedKanbanIndex^.notes.kanbanData.kanbans
    currentNotes = stWithSyncedKanbanIndex^.notes.noteData

    -- 更新当前看板的笔记数据
    updatedKanbans = if calculatedKanbanIndex >= 0 && calculatedKanbanIndex < length kanbanList
                     then kanbanList & ix calculatedKanbanIndex . childs .~ currentNotes
                     else kanbanList

    -- 4. 确保侧边栏中的看板选择状态与当前查看的看板一致
    -- 先清除所有看板的选中状态
    kanbansWithClearedSelection = updatedKanbans & each . kanbanSelected .~ False

    -- 然后设置当前看板的选中状态
    finalKanbans = if calculatedKanbanIndex >= 0 && calculatedKanbanIndex < length kanbansWithClearedSelection
                   then kanbansWithClearedSelection & ix calculatedKanbanIndex . kanbanSelected .~ True
                   else kanbansWithClearedSelection

    updatedKanbanData = stWithSyncedKanbanIndex^.notes.kanbanData & kanbans .~ finalKanbans
  in
    -- 最终更新应用状态
    stWithSyncedKanbanIndex & notes.kanbanData .~ updatedKanbanData

-- | 将当前笔记列表同步到对应的看板中
-- 在任何修改笔记列表后使用此函数，确保看板数据得到更新
syncNotesToKanban :: AppState e Name -> AppState e Name
syncNotesToKanban st = 
  let currentNotes = st^.notes.noteData
      currentKanbanIndex = getCurrentKanbanIndex (st^.notes.currentLocation)
      updatedKanbans = st^.notes.kanbanData.kanbans &
                        ix currentKanbanIndex . childs .~ currentNotes
      updatedKanbanData = st^.notes.kanbanData & kanbans .~ updatedKanbans
  in st & notes.kanbanData .~ updatedKanbanData


-- | 选择指定索引的笔记
selectNoteByIndex :: AppState e Name -> Int -> AppState e Name
selectNoteByIndex st idx =
  let noteCount = length (st^.notes.noteData)
      validIdx = idx >= 0 && idx < noteCount
      
      -- 如果当前有选中笔记，先取消选中
      unselectCurrent = 
        if st^.selectedIndex >= 0 && st^.selectedIndex < noteCount
        then st & (notes . noteData . ix (st^.selectedIndex) . selected) .~ False
        else st
      
      -- 如果索引有效，更新选择
      updatedSt = if validIdx
                 then syncAppStateFocus $
                      unselectCurrent & selectedIndex .~ idx
                                     & (notes . noteData . ix idx . selected) .~ True
                 else unselectCurrent
  in
    updatedSt

-- 结合 syncNotesToKanban 使用，确保数据和界面都同步更新
syncAndSave :: AppState e Name -> EventM Name (Next (AppState e Name))
syncAndSave st = do
  let synced = syncAppStateFocus $ syncNotesToKanban st
  _ <- saveAndUpdateState synced
  M.continue synced


