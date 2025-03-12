module Actions (
  onSave,
  onFormUpdate,
  onExit,
  remove,
  handleEnter,
  handleTypingEvents,
  handleTab,
  handleShiftTab,
  cloneNote,
  select,
  initApp,
  swapNoteLeft,
  swapNoteRight,
  syncAppStateFocus,
  switchKanban,
  cutNote,
  pasteNote,
  deleteKanbanAtIndex,
  convertNoteToTodo,
  kanbanManagerSelect,
  swapKanbanUp,
  swapKanbanDown,
  initKanbanEditor,
  resetKanbanEdit,
  saveKanbanTitle,
  currentKanbanTitle,
  undoLastDelete,
  getCurrentKanbanIndex,
  exitKanbanManager,  -- 添加这行来导出函数
  enterKanbanManager,
  toggleNoteDisplayMode,
  scrollNoteContent,
  navigateGrid
  ) where

import Brick.Focus   ( focusGetCurrent, focusNext, focusPrev, focusRing, focusSetCurrent)
import Brick.Forms ( handleFormEvent )
import qualified Brick.Main             as M (continue, vScrollBy, viewportScroll, lookupViewport)
import Brick.Types
import Data.List              as L (delete, elem, length, intercalate, findIndex, sortOn)
import Data.Map               as M (findWithDefault, fromList)
import Data.Maybe             (fromMaybe)
import Data.Text as T ( null, pack, splitOn, strip, empty, unpack, Text )  -- 添加 Text 类型
import Brick.Widgets.Edit as E ( editor, getEditContents, handleEditorEvent, Editor )  -- 添加 Editor 类型
import Dialog ( getDialog )
import Form ( setForm, emptyForm )
import qualified Graphics.Vty           as V
import System.IO ()
import Task
import Types
import FileIO
import Note
import KbUtils
import AppState
import Layout
import Lens.Micro

syncedCurrentLocation :: Lens' Notes KanbanLocation
syncedCurrentLocation = lens getter setter
  where
    getter notes = notes^.currentLocation
    setter notes newLocation =
      let newKanbanIndex = getCurrentKanbanIndex newLocation
      in notes & currentLocation .~ newLocation
              & currentKanban .~ newKanbanIndex

-- 这部分开始保留 Actions.hs 中的其他函数
onFormUpdate st ev =
    handleFormEvent ev (st^.form) >>= (\f' -> M.continue $ syncAppStateFocus (st & form .~ f'))

onSave st ev =
  case st^.dialogMode of
    NoteCreate -> handleFormEvent ev (st^.form) >>=
      (\f' -> do
        -- 创建新笔记
        let newNote = getFreeNote f' st False
            -- 使用通用函数添加到当前看板
            updatedSt = addNoteToCurrentKanban st newNote
                        & resetDialog
                        & form .~ emptyForm

        -- 保存到文件并返回更新后的状态
        _ <- saveAndUpdateState updatedSt
        return updatedSt)

    NoteEdit -> handleFormEvent ev (st^.form) >>=
      (\f' -> do
        -- 更新已有笔记
        let updatedSt = syncAppStateFocus $
                st  & (notes . noteData . ix (st^.selectedIndex)) .~ getFreeNote f' st True
                    & resetDialog
                    & editMode  .~ False
                    & form      .~ emptyForm

        -- 保存到文件并返回更新后的状态
        _ <- saveAndUpdateState updatedSt
        return updatedSt)

    -- 修改 onSave 函数中的 TodoCreate 和 TodoEdit 分支

    -- 在 TodoCreate 分支中：
    TodoCreate -> do
      -- 创建新待办事项笔记，并清理任务高亮状态
      let newTodoNote = createTodoNote st
          -- 清理所有任务的高亮状态
          cleanedTasks = map (\t -> t & status .~ False) (newTodoNote^.tasks)
          -- 重建任务树
          rebuildedTasks = rebuildTaskTree cleanedTasks
          -- 更新笔记的任务
          cleanedNote = newTodoNote & tasks .~ rebuildedTasks
          -- 使用通用函数添加到当前看板
          updatedSt = addNoteToCurrentKanban st cleanedNote
                      & resetDialog
                      & resetTodo

      -- 保存到文件并返回更新后的状态
      _ <- saveAndUpdateState updatedSt
      return updatedSt

    -- 在 TodoEdit 分支中：
    TodoEdit -> do
      -- 获取更新后的笔记，同时清理任务高亮状态
      let updatedTodoNote = createTodoNote st
          -- 清理所有任务的高亮状态
          cleanedTasks = map (\t -> t & status .~ False) (updatedTodoNote^.tasks)
          -- 重建任务树
          rebuildedTasks = rebuildTaskTree cleanedTasks
          -- 更新笔记的任务
          finalNote = updatedTodoNote & tasks .~ rebuildedTasks
          updatedSt = syncAppStateFocus $
                st  & (notes . noteData . ix (st^.selectedIndex)) .~ finalNote
                    & resetDialog
                    & resetTodo
                    & editMode .~ False

      -- 保存到文件并返回更新后的状态
      _ <- saveAndUpdateState updatedSt
      return updatedSt

    KanbanCreate -> do
      -- 创建新看板，使用带位置计算的版本
      let updatedSt = createNewKanbanWithPosition st

      -- 保存到文件并返回更新后的状态
      _ <- saveAndUpdateState updatedSt
      return updatedSt

    _ -> return st


handleTypingEvents st ev e =
  case st^.dialogMode of
    ChoiceCreate  | st^.showDialog -> M.continue $ st & dialogMode .~ (st^.dialogSelect)
                  | otherwise -> M.continue st
    TodoCreate    | st^.showDialog -> handleToDoEditEvents st ev e
                  | otherwise -> M.continue st
    TodoEdit      | st^.showDialog -> handleToDoEditEvents st ev e
                  | otherwise -> M.continue st
    NoteCreate    -> onFormUpdate st ev
    NoteEdit      -> onFormUpdate st ev
    KanbanCreate  | st^.showDialog -> handleKanbanEditEvents st ev e  -- 需要实现这个函数
                  | otherwise -> M.continue st

handleEnter st ev =
  case st^.dialogMode of
    ChoiceCreate | st^.showDialog -> M.continue $ st & dialogMode .~ (st^.dialogSelect)
                 | otherwise -> M.continue $ st  & dialogMode .~ getDialogMode st & setFormState
    TodoCreate   -> handleTaskEdit st
    TodoEdit     -> handleTaskEdit st
    NoteCreate   -> onFormUpdate st ev
    NoteEdit     -> onFormUpdate st ev
    KanbanCreate -> M.continue st

handleTab st ev =
  case st^.dialogMode of
    ChoiceCreate  | (st^.dialogSelect) == NoteCreate -> M.continue $ st & dialogSelect .~ TodoCreate
                  | (st^.dialogSelect) == TodoCreate -> M.continue $ st & dialogSelect .~ KanbanCreate
                  | otherwise -> M.continue $ st & dialogSelect .~ NoteCreate
    TodoCreate  -> M.continue $ syncAppStateFocus $ handleFocus st
    TodoEdit    -> M.continue $ syncAppStateFocus $ handleFocus st
    NoteCreate  -> onFormUpdate st ev
    NoteEdit    -> onFormUpdate st ev
    KanbanCreate -> M.continue st

handleShiftTab st ev =
  case st^.dialogMode of
    ChoiceCreate  | (st^.dialogSelect) == KanbanCreate -> M.continue $ st & dialogSelect .~ TodoCreate
                  | (st^.dialogSelect) == TodoCreate -> M.continue $ st & dialogSelect .~ NoteCreate
                  | otherwise -> M.continue $ st & dialogSelect .~ KanbanCreate
    TodoCreate  -> M.continue $ syncAppStateFocus $ handleFocusPrev st
    TodoEdit    -> M.continue $ syncAppStateFocus $ handleFocusPrev st
    NoteCreate  -> onFormUpdate st ev
    NoteEdit    -> onFormUpdate st ev
    KanbanCreate -> M.continue st

select st by = M.continue $
                syncAppStateFocus $
                st  & (notes . noteData . ix (st^.selectedIndex) . selected)  %~ not
                    & selectedIndex .~ nextSelIndex
                    & (notes . noteData . ix nextSelIndex  . selected)  %~ not
  where nextSelIndex = moveSelect by (L.length (st^.(notes . noteData))) (st^.selectedIndex)

-- 替换 remove 函数实现，直接使用 syncAndSave
remove :: AppState e Name -> EventM Name (Next (AppState e Name))
remove st | not (st^.showDialog) = do
            -- 获取被删除的笔记，确保选中状态为 False
            let idx = st^.selectedIndex
                deletedNote = case st^.notes.noteData ^? ix idx of
                                Nothing -> Nothing
                                Just note -> Just (note & selected .~ False)

            -- 更新状态，删除选定笔记，保存删除的笔记信息
            let updatedSt = st & (notes . noteData) .~ removeNote st
                              & (notes . totalNotes) %~ subtract 1
                              & lastDeletedNote .~ deletedNote
                              & (lastDeletedKanbanIndex
                                 ?~ getCurrentKanbanIndex (st ^. notes . currentLocation))

            -- 使用 syncAndSave 函数同步状态并保存
            syncAndSave updatedSt
          | otherwise = M.continue st

-- 替换 onExit 函数实现，使用 syncNotesToKanban
onExit :: AppState e Name -> EventM Name (AppState e Name)
onExit st = do
    -- 清除笔记的选中状态
    let cleanedSt = st & (notes.noteData.each.selected) .~ False

    -- 同步数据并保存
    let syncedSt = syncNotesToKanban cleanedSt
    saveAndUpdateState syncedSt

-- 定义 unselect 函数来清除一个笔记的选中状态
unselect :: Note -> Note
unselect note = note & selected .~ False

-- 替换 cloneNote 函数实现，使用 syncAndSave
cloneNote :: AppState e Name -> EventM Name (Next (AppState e Name))
cloneNote st
  | st^.selectedIndex < 0 = M.continue st  -- 没有选中笔记时不执行操作
  | otherwise =
      case st^.(notes.noteData) ^? ix (st^.selectedIndex) of
        Nothing -> M.continue st
        Just n ->
          -- 创建笔记副本（确保未被选中）
          let clonedNote = n & selected .~ False
              -- 使用通用函数添加到当前看板
              updatedSt = addNoteToCurrentKanban st clonedNote
          in syncAndSave updatedSt  -- 同步并保存

removeNote :: AppState e n -> [Note]
removeNote st = removeItem (st^. (notes . noteData)) (st^.(notes . noteData) ^? ix (st^.selectedIndex))

removeTask :: AppState e n -> [Task]
removeTask st = removeItem (st^. (notes . tempTodoNote . tasks)) (st^.(notes . tempTodoNote . tasks) ^? ix (st^.notes.tempTodoNote.selectedTaskIndex))

removeItem :: Eq a => [a] -> Maybe a -> [a]
removeItem curList selItem = fromMaybe curList newList
  where newList = L.delete <$> selItem <*> Just curList

-- 修改后的函数定义
moveSelect :: Integral a => a -> a -> a -> a
moveSelect delta len idx = (idx + delta) `mod` len

-- 修改 setFormState 函数，在编辑待办事项时展平任务列表

setFormState :: AppState e Name -> AppState e Name
setFormState st = newState & showDialog .~ True
                           & editMode .~ True
  where
    newState | st^.dialogMode == NoteEdit =
               st & form .~ setForm (st^.(notes . noteData) ^? ix (st^.selectedIndex))
            | st^.dialogMode == TodoEdit =
               let
                   -- 获取待编辑的笔记
                   noteToEdit = st^.notes.noteData ^? ix (st^.selectedIndex) ^. non emptyNote
                   -- 展平任务列表，设置正确的级别
                   flattenedTasks = flattenTaskTree (noteToEdit^.tasks)
                   flattenedNote = noteToEdit & tasks .~ flattenedTasks
               in
                   st & (notes . tempTodoNote) .~ flattenedNote
                     & (notes . taskTitle) .~ editor TaskTitle Nothing (T.unpack (noteToEdit^.title))
            | otherwise = st

resetTodo :: AppState e n -> AppState e n
resetTodo st = st & (notes . focusEdit) .~ focusRing [TaskTitle, TaskEdit, Tasks, Checkbox]
                  & (notes . taskTitle) .~ emptyEditor TaskTitle
                  & (notes . taskEdit) .~ emptyEditor TaskEdit
                  & (notes . tempTodoNote) .~ getTodoNote "" []
                  & (notes . tempTodoNote . selectedTaskIndex) .~ (-1)  -- 确保没有选中的任务

resetDialog :: AppState e n -> AppState e n
resetDialog st = st & dialogMode    .~ ChoiceCreate
                    & dialogSelect  .~ NoteCreate
                    & showDialog    .~ False

-- 空格键只改变 _selectedTask，不更改 _status
handleTaskEvents :: AppState e n -> V.Event -> AppState e n
handleTaskEvents st (V.EvKey (V.KChar ' ') []) =
  st & (notes . tempTodoNote . tasks . ix (st^.notes.tempTodoNote.selectedTaskIndex) . selectedTask) %~ not
handleTaskEvents st (V.EvKey V.KDel []) =
  let curIdx   = st^.notes.tempTodoNote.selectedTaskIndex
      newTasks = removeTask st
      newIdx
        | Prelude.null newTasks = -1  -- 任务列表为空
        | curIdx < L.length newTasks = curIdx  -- 中间或首项删除，索引不变
        | otherwise = curIdx - 1  -- 如果删除的是尾项，则索引向前移一位
  in st & (notes . tempTodoNote . tasks) .~ newTasks
        & (notes . focusEdit) .~ ( if newIdx == -1
                                   then focusSetCurrent TaskTitle defaultTaskFocus
                                   else focusSetCurrent Tasks (focusRing [Tasks]) )
        & updateSelectedTaskIndex True (const newIdx)
        -- 删除后不再调用 handleFocus，避免额外“+1”
handleTaskEvents st _ = st

handleFocus :: AppState e n -> AppState e n
handleFocus st = nextFocus
  where curFocus = fromMaybe TaskTitle $ focusGetCurrent (st^.notes.focusEdit)
        totalTasks = L.length (st^.notes.tempTodoNote.tasks)
        taskIndex = totalTasks -1
        selIndex = st^.notes.tempTodoNote.selectedTaskIndex
        canFocusTask curFocus totalTasks  = curFocus `L.elem` [TaskEdit, Tasks] && totalTasks > 0
        isTask index = index < taskIndex
        isLastTask index = index == taskIndex
        nextFocus | curFocus == TaskTitle = st & (notes . focusEdit) %~ focusNext
                  | curFocus == TaskEdit && totalTasks > 0 = st & (notes . focusEdit) .~ focusSetCurrent Tasks (focusRing [Tasks])
                                                               & updateSelectedTaskIndex True (const 0)
                                                               & setTaskSelected
                  | curFocus == TaskEdit && totalTasks == 0 = st & (notes . focusEdit) .~ focusSetCurrent Checkbox defaultTaskFocus
                                                                & updateSelectedTaskIndex True (\_ -> -1)
                                                                & (notes . tempTodoNote . checkBoxSelected) .~ True  -- 添加这行
                  | canFocusTask curFocus totalTasks && isTask selIndex = st  & (notes . focusEdit) .~ focusSetCurrent Tasks (focusRing [Tasks])
                                                                              & updateSelectedTaskIndex True (+1)
                                                                              & setTaskSelected
                  | canFocusTask curFocus totalTasks && isLastTask selIndex = st  & (notes . focusEdit) .~ focusSetCurrent Checkbox defaultTaskFocus
                                                                                  & updateSelectedTaskIndex True (\_ -> -1)
                                                                                  & (notes . tempTodoNote . checkBoxSelected) .~ True  -- 添加这行
                  | curFocus == Checkbox = st & notes . tempTodoNote . checkBoxSelected .~ False
                                             & (notes . focusEdit) .~ focusSetCurrent TaskTitle defaultTaskFocus
                  | otherwise = st & (notes . focusEdit) %~ focusNext

-- 实现反向焦点切换的函数
handleFocusPrev :: AppState e n -> AppState e n
handleFocusPrev st = prevFocus
  where curFocus = fromMaybe TaskTitle $ focusGetCurrent (st^.notes.focusEdit)
        totalTasks = L.length (st^.notes.tempTodoNote.tasks)
        taskIndex = totalTasks -1
        selIndex = st^.notes.tempTodoNote.selectedTaskIndex
        canFocusTask curFocus totalTasks  = curFocus `L.elem` [TaskEdit, TaskTitle] && totalTasks > 0
        isFirstTask index = index == 0
        prevFocus | curFocus == TaskTitle = st & (notes . focusEdit) .~ focusSetCurrent Checkbox defaultTaskFocus
                                               & (notes . tempTodoNote . checkBoxSelected) .~ True
                  | curFocus == TaskEdit = st & (notes . focusEdit) .~ focusSetCurrent TaskTitle defaultTaskFocus
                  | curFocus == Tasks && selIndex > 0 = st  & (notes . focusEdit) .~ focusSetCurrent Tasks (focusRing [Tasks])
                                                            & updateSelectedTaskIndex True (\i -> i - 1)
                                                            & setTaskSelected
                  | curFocus == Tasks && selIndex == 0 = st & (notes . focusEdit) .~ focusSetCurrent TaskEdit defaultTaskFocus
                                                            & updateSelectedTaskIndex False (const (-1))
                  | curFocus == Checkbox && totalTasks > 0 = st & notes . tempTodoNote . checkBoxSelected .~ False
                                                               & (notes . focusEdit) .~ focusSetCurrent Tasks (focusRing [Tasks])
                                                               & updateSelectedTaskIndex True (const taskIndex)
                                                               & setTaskSelected
                  | curFocus == Checkbox = st & notes . tempTodoNote . checkBoxSelected .~ False
                                             & (notes . focusEdit) .~ focusSetCurrent TaskEdit defaultTaskFocus
                  | otherwise = st & (notes . focusEdit) %~ focusPrev

handleTaskEdit :: AppState e Name -> EventM n (Next (AppState e Name))
handleTaskEdit st = case fromMaybe None $ focusGetCurrent (st^.notes.focusEdit) of
    TaskEdit  | st^.notes.taskEditMode ->
                -- 编辑现有任务的情况
                M.continue $
                  syncAppStateFocus $
                  let
                    -- 获取当前任务的缩进级别
                    currentTask = (st^.notes.tempTodoNote.tasks) !! (st^.notes.tempTodoNote.selectedTaskIndex)
                    currentLevel = _level currentTask
                    -- 创建新任务但保持原来的缩进级别
                    newTask = getTask st False & level .~ currentLevel
                  in
                  st  & (notes . tempTodoNote . tasks . ix (st^.notes.tempTodoNote.selectedTaskIndex)) .~ newTask
                      & (notes . taskEdit)  .~ emptyEditor TaskEdit
                      -- 不修改selectedTaskIndex，保持焦点在原来的任务上
                      & (notes . taskEditLabel) .~ "New Task"
                      & (notes . taskEditMode)  .~ False
                      -- 确保焦点回到任务列表
                      & (notes . focusEdit)  .~ focusSetCurrent Tasks (focusRing [Tasks])
                      -- 确保任务保持选中状态
                      & setTaskSelected
              | otherwise ->
                  -- 创建新任务的情况
                  let newTask = getTask st False
                      currentTasks = st^.notes.tempTodoNote.tasks
                      -- 检查是否有指定的插入位置
                      mInsertIdx = st^.notes.insertTaskIndex
                      -- 根据插入位置构建新任务列表
                      updatedTasks = case mInsertIdx of
                        -- 如果有指定位置，在该位置之后插入
                        Just idx ->
                          let (before, after) = Prelude.splitAt (idx + 1) currentTasks
                          in before ++ [newTask] ++ after
                        -- 否则添加到列表末尾
                        Nothing -> currentTasks ++ [newTask]
                  in M.continue $
                     syncAppStateFocus $
                     st  & (notes . tempTodoNote . tasks) .~ updatedTasks
                         & (notes . taskEdit) .~ emptyEditor TaskEdit
                         -- 插入完成后重置插入位置
                         & (notes . insertTaskIndex) .~ Nothing
                         -- 保持焦点在任务输入框上，便于继续添加任务
                         & (notes . focusEdit)  .~ focusSetCurrent TaskEdit defaultTaskFocus

    Tasks ->
            -- 从任务列表进入编辑模式
            M.continue $
              syncAppStateFocus $
              st  & (notes . taskEditLabel) .~ "Edit Task"
                  & (notes . taskEdit)      .~ editor TaskEdit Nothing (getSelectedTaskContent st)
                  & (notes . focusEdit)     .~ focusSetCurrent TaskEdit defaultTaskFocus
                  & (notes . taskEditMode)  .~ True

    _ -> M.continue st

-- setTaskSelected 只设置高亮，不依赖 X 的状态
setTaskSelected :: AppState e n -> AppState e n
setTaskSelected st =
  st & (notes . tempTodoNote . tasks . ix (st^.notes.tempTodoNote.selectedTaskIndex) . status) .~ True

-- updateSelectedTaskIndex 仅用于更新高亮状态 (_status)，而不影响 _selectedTask
updateSelectedTaskIndex :: Bool -> (Int -> Int) -> AppState e n -> AppState e n
updateSelectedTaskIndex sel by st =
  let newIdx = by (st^.notes.tempTodoNote.selectedTaskIndex)
      tasksOld = st^.notes.tempTodoNote.tasks
      tasksNew = [ if i == newIdx then t & status .~ sel
                              else t & status .~ False
                 | (i, t) <- Prelude.zip [0..] tasksOld ]
  in st & (notes . tempTodoNote . selectedTaskIndex) .~ newIdx
        & (notes . tempTodoNote . tasks) .~ tasksNew

getSelectedTaskContent :: AppState e n -> String
getSelectedTaskContent st = T.unpack $ st^.(notes . tempTodoNote . tasks) ^? ix (st^.notes.tempTodoNote.selectedTaskIndex) ^. (non emptyTask . task)

getDialogMode :: AppState e n -> DialogMode
getDialogMode st = dlgMode
  where modes = M.fromList [(FreeNote,NoteEdit),(TodoList,TodoEdit)]
        dlgMode = M.findWithDefault ChoiceCreate (st^.(notes . noteData) ^? ix (st^.selectedIndex) ^. (non emptyNote . mode)) modes

initApp :: CmdOptions -> IO (AppState e Name)
initApp opts = do
  (fileInfos, kanbanData) <- decodeOptions opts

  -- 直接获取第一个看板，不再需要检查为空的情况
  let currentKanban = head (kanbanData^.kanbans)
  let notes = currentKanban^.childs

  -- 创建带有额外编辑器初始化的状态
  -- 通过 fileInfos 获取路径信息
  let baseState = syncAppStateFocus $ blankAppState notes fileInfos kanbanData
  return $ baseState
          & kanbanTitleEditor .~ editor KanbanTitleField (Just 1) ""
          & kanbanFocus .~ focusRing [KanbanTitleField]
          & kanbanFocusMap .~ []  -- 初始化为空列表

-- 修改函数签名，接受 [FileInfo] 而不是 Maybe FilePath
blankAppState :: [Note] -> [FileInfo] -> KanbanData -> AppState e Name
blankAppState notes fileInfos kanbanData = AppState{
  _notes = initTasks notes kanbanData fileInfos,  -- 使用Task.hs中的initTasks函数
  _selectedIndex = if Prelude.null notes then -1 else 0,
  _form = emptyForm,
  _editMode = False,
  _showDialog = False,
  _dialogMode = ChoiceCreate,
  _dialogSelect = NoteCreate,
  _showHelp = False,
  _dlg = getDialog,
  _kanbanTitleEditor = emptyEditor KanbanTitleField,
  _kanbanFocus = focusRing [KanbanTitleField],
  _clipboard = [],  -- 修正剪贴板类型为空列表
  _showKanbanList = length (_kanbans kanbanData) > 1,
  _expandKanbanNotes = False,  -- 添加缺失的字段
  _showKanbanManager = False,  -- 添加此行
  _kanbanManagerFocus = 0,     -- 确保这些字段也存在
  _editingKanbanTitle = False,
  _kanbanEditor = editor KanbanTitle (Just 1) "",
  _lastDeletedNote = Nothing,           -- 添加这一行
  _lastDeletedKanbanIndex = Nothing,    -- 添加这一行
  _fileInfos = fileInfos,  -- 直接使用传入的 fileInfos
  _kanbanFocusMap = []
}

-- 辅助函数：交换列表中两个位置的元素
swapList :: [a] -> (Int, Int) -> [a]
swapList xs (i,j) =
  let elemI = xs !! i
      elemJ = xs !! j
      replaceAt k x ys = Prelude.take k ys ++ [x] ++ Prelude.drop (k+1) ys
      xs' = replaceAt i elemJ xs
  in replaceAt j elemI xs'

-- 替换 swapNoteLeft 函数实现，使用 syncAndSave
swapNoteLeft :: AppState e Name -> EventM Name (Next (AppState e Name))
swapNoteLeft st =
  let idx = st^.selectedIndex
      notesList = st^.notes.noteData
  in if idx > 0 then  -- 确保可以向左交换
       let newNotes = swapList notesList (idx, idx - 1)
           st' = st & (notes . noteData) .~ newNotes
                    & selectedIndex .~ (idx - 1)
       in syncAndSave st'
      else
       M.continue st  -- 不做改变

-- 替换 swapNoteRight 函数实现，使用 syncAndSave
swapNoteRight :: AppState e Name -> EventM Name (Next (AppState e Name))
swapNoteRight st =
  let idx = st^.selectedIndex
      notesList = st^.notes.noteData
      len = L.length notesList
  in if idx >= 0 && idx < len - 1 then  -- 确保可以向右交换
       let newNotes = swapList notesList (idx, idx + 1)
           st' = st & (notes . noteData) .~ newNotes
                    & selectedIndex .~ (idx + 1)
       in syncAndSave st'
      else
       M.continue st  -- 不做改变

-- 处理看板编辑相关的事件
handleKanbanEditEvents :: AppState e Name -> BrickEvent Name e -> V.Event -> EventM Name (Next (AppState e Name))
handleKanbanEditEvents st _ e =
  handleEventLensed st kanbanTitleEditor E.handleEditorEvent e >>= M.continue

-- 创建新看板的辅助函数
createNewKanban :: AppState e Name -> AppState e Name
createNewKanban = createNewKanbanWithPosition  -- 直接调用新函数

-- 重置看板编辑状态
resetKanbanEdit :: AppState e Name -> AppState e Name
resetKanbanEdit st =
  st & kanbanTitleEditor .~ emptyEditor KanbanTitleField
     & kanbanFocus .~ focusRing [KanbanTitleField]

switchKanban :: AppState e Name -> Int -> EventM Name (Next (AppState e Name))
switchKanban st direction = do
  -- 修改判断条件，排除showKanbanManager的情况
  if (st^.editMode || st^.showDialog) && not (st^.showKanbanManager)
    then M.continue st
    else do
      let kanbanCount = length (st^.notes.kanbanData.kanbans)

      -- 如果没有看板或者只有一个看板，保持不变
      if kanbanCount <= 1
        then M.continue st
        else do
          -- 计算新的看板索引（循环）
          let currentKanbanIdx = getCurrentKanbanIndex (st^.notes.currentLocation)
              newKanbanIdx = (currentKanbanIdx + direction) `mod` kanbanCount

          -- 同时更新kanbanManagerFocus（如果在管理模式下）
          let stWithFocus = if st^.showKanbanManager
                            then st & kanbanManagerFocus .~ newKanbanIdx
                            else st

          -- 最后在switchToKanban中重置视口位置
          switchToKanban stWithFocus newKanbanIdx

-- 替换 pasteNote 函数实现，使用 syncAndSave
pasteNote :: AppState e Name -> EventM Name (Next (AppState e Name))
pasteNote st =
  case st^.clipboard of
    [] -> M.continue st  -- 剪贴板为空
    clipboardNotes -> do
      -- 将剪贴板中的笔记添加到当前看板
      let updatedSt = addNotesToKanban st (st^.notes.currentKanban) clipboardNotes True
                      & clipboard .~ []  -- 清空剪贴板

      -- 使用 syncAndSave 函数同步状态并保存
      syncAndSave updatedSt

-- 替换 cutNote 函数实现，使用 syncAndSave
cutNote :: AppState e Name -> EventM Name (Next (AppState e Name))
cutNote st =
  case st^.selectedIndex of
    -1 -> M.continue st  -- 没有选中笔记
    idx -> do
      let selectedNote = (st^.notes.noteData) !! idx
          -- 从列表中移除笔记
          updatedNotes = Prelude.take idx (st^.notes.noteData) ++
                         Prelude.drop (idx + 1) (st^.notes.noteData)
          -- 将笔记放入剪贴板，确保选中状态为 False
          clipboardNote = selectedNote & selected .~ False

          -- 更新应用状态
          updatedSt = st & notes.noteData .~ updatedNotes
                         & notes.totalNotes %~ subtract 1
                         & clipboard %~ (clipboardNote :)  -- 添加到剪贴板

      -- 使用 syncAndSave 函数同步状态并保存
      syncAndSave updatedSt

-- 修改 handleToDoEditEvents 函数，使焦点随着滚动移动
handleToDoEditEvents :: AppState e Name -> BrickEvent Name e -> V.Event -> EventM Name (Next (AppState e Name))
handleToDoEditEvents st ev e =
  case e of
    V.EvKey (V.KChar 'u') [V.MCtrl] -> togglePackTask st
    -- 处理向下滚动，滚动视口的同时移动焦点
    V.EvKey V.KDown [] -> do
      -- 滚动视口
      M.vScrollBy (M.viewportScroll TaskListVP) 1

      -- 确定是否应该移动焦点
      let currentIdx = st^.notes.tempTodoNote.selectedTaskIndex
          tasksCount = L.length (st^.notes.tempTodoNote.tasks)

          -- 如果有选中的任务，且不是最后一个，则将焦点下移
          nextIdx = if currentIdx >= 0 && currentIdx < tasksCount - 1
                    then currentIdx + 1
                    else currentIdx

          -- 更新任务状态和选中索引
          updatedSt = if nextIdx /= currentIdx
                      then updateSelectedTaskIndex True (const nextIdx) st
                      else st

      -- 返回修改后的状态
      M.continue updatedSt

    -- 处理向上滚动，滚动视口的同时移动焦点
    V.EvKey V.KUp [] -> do
      -- 滚动视口
      M.vScrollBy (M.viewportScroll TaskListVP) (-1)

      -- 确定是否应该移动焦点
      let currentIdx = st^.notes.tempTodoNote.selectedTaskIndex

          -- 如果有选中的任务，且不是第一个，则将焦点上移
          nextIdx = if currentIdx > 0
                    then currentIdx - 1
                    else currentIdx

          -- 更新任务状态和选中索引
          updatedSt = if nextIdx /= currentIdx
                      then updateSelectedTaskIndex True (const nextIdx) st
                      else st

      -- 返回修改后的状态
      M.continue updatedSt

    V.EvKey V.KIns [] ->
      let currentIndex = st^.notes.tempTodoNote.selectedTaskIndex
          -- 使用 Task.hs 中定义的 emptyTask 而不是手动构造不完整的 Task
          newTask = emptyTask
          currentTasks = st^.notes.tempTodoNote.tasks
          -- 如果当前有选中的任务，需要继承其缩进级别
          taskWithLevel = if currentIndex >= 0 && not (Prelude.null currentTasks) then
                            let parentLevel = _level (currentTasks !! currentIndex)
                            in newTask & level .~ parentLevel
                          else
                            newTask
          (updatedTasks, newSelectedIdx) =
            if currentIndex >= 0 then
              let (before, after) = Prelude.splitAt (currentIndex + 1) currentTasks
                  newTasks = before ++ [taskWithLevel] ++ after
              in (newTasks, currentIndex + 1)
            else
              (currentTasks ++ [taskWithLevel], L.length currentTasks)
          stWithNewTask = st & notes.tempTodoNote.tasks .~ updatedTasks
                             & updateSelectedTaskIndex True (const newSelectedIdx)
      in M.continue stWithNewTask

    -- 其余处理分支维持不变...
    V.EvKey V.KUp [V.MCtrl] ->
      let currentIndex = st^.notes.tempTodoNote.selectedTaskIndex
          currentTasks = st^.notes.tempTodoNote.tasks
      in
        -- 只在有选中任务且不是第一个任务时执行交换
        if currentIndex > 0 then
          let
            -- 使用之前定义的swapList函数交换任务
            updatedTasks = swapList currentTasks (currentIndex, currentIndex - 1)
            -- 更新任务列表，保持焦点在同一个任务上（现在位置上移了）
            stWithSwappedTask = st & notes.tempTodoNote.tasks .~ updatedTasks
                                  & updateSelectedTaskIndex True (const (currentIndex - 1))
          in M.continue stWithSwappedTask
        else
          -- 如果是第一个任务，则不做任何改变
          M.continue st

    -- 使用ctrl+Up/Down增加条目交换位置一致性
    V.EvKey V.KDown [V.MCtrl] ->
      let currentIndex = st^.notes.tempTodoNote.selectedTaskIndex
          currentTasks = st^.notes.tempTodoNote.tasks
          taskCount = L.length currentTasks
      in
        -- 只在有选中任务且不是最后一个任务时执行交换
        if currentIndex >= 0 && currentIndex < taskCount - 1 then
          let
            -- 使用之前定义的swapList函数交换任务
            updatedTasks = swapList currentTasks (currentIndex, currentIndex + 1)
            -- 更新任务列表，保持焦点在同一个任务上（现在位置下移了）
            stWithSwappedTask = st & notes.tempTodoNote.tasks .~ updatedTasks
                                  & updateSelectedTaskIndex True (const (currentIndex + 1))
          in M.continue stWithSwappedTask
        else
          -- 如果是最后一个任务，则不做任何改变
          M.continue st

    -- 增加任务缩进级别（使用Ctrl+右箭头）- 简化版本，无需检查上一行
    V.EvKey V.KRight [V.MCtrl] -> do
      let currentIdx = st^.notes.tempTodoNote.selectedTaskIndex
          currentTasks = st^.notes.tempTodoNote.tasks

      -- 确保有选中的任务且索引有效
      if currentIdx >= 0 && currentIdx < length currentTasks then
        let
            -- 获取当前任务
            currentTask = currentTasks !! currentIdx

            -- 直接增加缩进级别
            newLevel = _level currentTask + 1

            -- 更新任务
            updatedTasks = take currentIdx currentTasks ++
                          [currentTask & level .~ newLevel] ++
                          drop (currentIdx + 1) currentTasks

            -- 更新应用状态 - 这里确保重新设置当前任务的高亮状态
            updatedSt = st & notes.tempTodoNote.tasks .~ updatedTasks
                          & updateSelectedTaskIndex True (const currentIdx)
        in
          M.continue updatedSt
      else
        M.continue st

    -- 类似地修改 Ctrl+左箭头部分：
    V.EvKey V.KLeft [V.MCtrl] -> do
      let currentIdx = st^.notes.tempTodoNote.selectedTaskIndex
          currentTasks = st^.notes.tempTodoNote.tasks

      -- 确保有选中的任务且索引有效
      if currentIdx >= 0 && currentIdx < length currentTasks then
        let
            -- 获取当前任务
            currentTask = currentTasks !! currentIdx

            -- 减少缩进级别，但不小于0
            newLevel = max 0 (_level currentTask - 1)

            -- 更新任务
            updatedTasks = take currentIdx currentTasks ++
                          [currentTask & level .~ newLevel] ++
                          drop (currentIdx + 1) currentTasks

            -- 更新应用状态 - 这里确保重新设置当前任务的高亮状态
            updatedSt = st & notes.tempTodoNote.tasks .~ updatedTasks
                          & updateSelectedTaskIndex True (const currentIdx)
        in
          M.continue updatedSt
      else
        M.continue st
    -- 其他事件按原来的逻辑处理
    _ -> M.continue =<< case fromMaybe None $ focusGetCurrent (st^.notes.focusEdit) of
           TaskEdit  -> handleEventLensed st (notes . taskEdit) E.handleEditorEvent e
           TaskTitle -> handleEventLensed st (notes . taskTitle) E.handleEditorEvent e
           Tasks     -> return $ syncAppStateFocus $ handleTaskEvents st e
           Checkbox  -> return $ st & (notes . tempTodoNote . highlighted) %~ not
                                    & (notes . tempTodoNote . checkBoxSelected) .~ True
           _ -> return st

-- 通用函数：向指定看板添加一个或多个笔记
-- 参数：
--   - st: 应用状态
--   - kanbanIdx: 目标看板索引
--   - notesToAdd: 要添加的笔记列表
--   - setFocus: 是否设置焦点在新添加的笔记上 (只有当添加到当前看板时生效)
addNotesToKanban :: AppState e Name -> Int -> [Note] -> Bool -> AppState e Name
addNotesToKanban st kanbanIdx notesToAdd setFocus =
  -- 如果笔记列表为空，直接返回原状态
  if Prelude.null notesToAdd then st
  else
    let
      -- 获取目标看板的当前笔记
      targetKanbanNotes = fromMaybe [] $ st^.notes.kanbanData.kanbans ^? ix kanbanIdx . childs

      -- 添加新笔记
      updatedTargetNotes = targetKanbanNotes ++ notesToAdd

      -- 更新目标看板
      updatedKanbans = st^.notes.kanbanData.kanbans & ix kanbanIdx . childs .~ updatedTargetNotes
      updatedKanbanData = st^.notes.kanbanData & kanbans .~ updatedKanbans

      -- 更新基本状态 (只包含看板数据更新)
      baseUpdatedSt = st & notes.kanbanData .~ updatedKanbanData

      -- 如果添加到当前看板且需要设置焦点
      finalSt = if kanbanIdx == st^.notes.currentKanban && setFocus
                then
                  -- 更新当前看板的数据
                  let newNoteIndex = length updatedTargetNotes - 1  -- 焦点设置在最后一个新添加的笔记上
                  in baseUpdatedSt & notes.noteData .~ updatedTargetNotes
                                   & notes.totalNotes .~ length updatedTargetNotes
                                   & selectedIndex .~ newNoteIndex
                else
                  baseUpdatedSt
    in
      -- 同步焦点状态
      syncAppStateFocus finalSt

-- 兼容旧代码的单笔记版本
addNoteToKanban :: AppState e Name -> Int -> Note -> Bool -> AppState e Name
addNoteToKanban st kanbanIdx note = addNotesToKanban st kanbanIdx [note]

-- 更新 addNoteToCurrentKanban 函数使用新的通用函数
addNoteToCurrentKanban :: AppState e Name -> Note -> AppState e Name
addNoteToCurrentKanban st note =
  addNoteToKanban st (st^.notes.currentKanban) note True
-- 修改 convertNoteToTodo 函数
convertNoteToTodo :: AppState e Name -> EventM Name (Next (AppState e Name))
convertNoteToTodo st =
  case st^.selectedIndex of
    -1 -> M.continue st
    idx ->
      case (st^.notes.noteData) !! idx of
        note@Note{_mode = FreeNote} -> do
          let noteContent = note^.content
              lines = T.splitOn (T.pack "\n") noteContent
              tasks = [ Task { _task = line, _status = False, _selectedTask = False, _subTasks = [], _level = 0 }
                      | line <- lines, not (T.null (T.strip line)) ]

          let todoNote = Note {
                _title = note^.title,
                _content = T.empty,
                _selected = False,  -- 设置为未选中状态
                _mode = TodoList,
                _tasks = tasks,
                _selectedTaskIndex = if Prelude.null tasks then -1 else 0,
                _checkBoxSelected = False,
                _highlighted = False
              }

          -- 取消原笔记的选中状态
          let stWithDeselectedNote = st & (notes . noteData . ix idx . selected) .~ False
              -- 使用通用函数添加新笔记并设置焦点
              finalSt = addNoteToCurrentKanban stWithDeselectedNote todoNote

          -- 如果有持久化文件，保存修改
          _ <- saveAndUpdateState finalSt

          M.continue finalSt

        _ -> M.continue st  -- 如果不是自由笔记，不做任何修改

-- 选择看板
kanbanManagerSelect :: AppState e Name -> Int -> EventM Name (Next (AppState e Name))
kanbanManagerSelect st delta =
  let kanbanList = _kanbans (st^.notes.kanbanData)  -- 修正：先获取 kanbanData，再访问 _kanbans
      currentIdx = st^.kanbanManagerFocus
      newIdx = max 0 (min (length kanbanList - 1) (currentIdx + delta))
  in M.continue $ st & kanbanManagerFocus .~ newIdx

swapKanban :: AppState e Name -> Int -> Int -> EventM Name (Next (AppState e Name))
swapKanban st idxA idxB =
  let kanbanList = _kanbans (st^.notes.kanbanData)
  in if idxA < 0 || idxB < 0 || idxA >= length kanbanList || idxB >= length kanbanList || Prelude.null kanbanList
     then M.continue st
     else
       -- 获取当前查看的看板索引
       let currentKanbanIdx = getCurrentKanbanIndex (st^.notes.currentLocation)

           -- 跟踪交换操作会如何影响当前看板索引
           -- 如果当前正在查看的看板是被交换的其中之一，则需要更新currentKanbanIdx
           newCurrentKanbanIdx
                | currentKanbanIdx == idxA = idxB
                | currentKanbanIdx == idxB = idxA
                | otherwise = currentKanbanIdx

           -- 使用swapElements函数交换整个看板对象
           swappedKanbans = swapElements idxA idxB kanbanList

           -- 更新看板数据
           updatedKanbanData = st^.notes.kanbanData & kanbans .~ swappedKanbans

           -- 更新当前位置信息，指向交换后的看板
           newLocation = updateLocationFromIndex newCurrentKanbanIdx (st^.notes.currentLocation)

           -- 获取新的当前看板的笔记列表
           newCurrentNotes = swappedKanbans !! newCurrentKanbanIdx ^. childs

           -- 更新看板管理器焦点，如果当前焦点是被交换的看板之一
           newManagerFocus
                | st ^. kanbanManagerFocus == idxA = idxB
                | st ^. kanbanManagerFocus == idxB = idxA
                | otherwise = st ^. kanbanManagerFocus

           -- 更新应用状态
           baseUpdatedState = st & notes.kanbanData .~ updatedKanbanData
                                & notes.syncedCurrentLocation .~ newLocation  -- 使用同步镜头
                                & notes.noteData .~ newCurrentNotes  -- 更新当前显示的笔记
                                & notes.totalNotes .~ length newCurrentNotes
                                & kanbanManagerFocus .~ newManagerFocus
                                & selectedIndex .~ (if Prelude.null newCurrentNotes then -1 else 0)

       in M.continue $ syncAppStateFocus baseUpdatedState

swapKanbanUp :: AppState e Name -> EventM Name (Next (AppState e Name))
swapKanbanUp st =
  let idx = st^.kanbanManagerFocus
  in if idx <= 0
     then M.continue st
     else swapKanban st (idx - 1) idx

swapKanbanDown :: AppState e Name -> EventM Name (Next (AppState e Name))
swapKanbanDown st =
  let idx = st^.kanbanManagerFocus
      kanbans = _kanbans (st^.notes.kanbanData)
      maxIdx = length kanbans - 1
  in if idx >= maxIdx
     then M.continue st
     else swapKanban st idx (idx + 1)
-- 交换列表中的两个元素
swapElements :: Int -> Int -> [a] -> [a]
swapElements i j xs
  | i < 0 || j < 0 || i >= length xs || j >= length xs = xs
  | otherwise =
      let elemI = xs !! i
          elemJ = xs !! j
      in [ if n == i then elemJ else if n == j then elemI else xs !! n
           | n <- [0..length xs - 1] ]

-- 更新列表中指定位置的元素
updateAt :: Int -> (a -> a) -> [a] -> [a]
updateAt idx f xs
  | idx < 0 || idx >= length xs = xs
  | otherwise = take idx xs ++ [f (xs !! idx)] ++ drop (idx + 1) xs

-- 获取当前看板的标题，用于初始化编辑器
currentKanbanTitle :: AppState e Name -> Text
currentKanbanTitle st =
  let kanbans = _kanbans (st^.notes.kanbanData)  -- 修正：先获取 kanbanData，再访问 _kanbans
      idx = st^.kanbanManagerFocus
  in if not (Prelude.null kanbans) && idx < length kanbans
     then _kanbanTitle (kanbans !! idx)
     else T.empty

-- 初始化看板标题编辑器
initKanbanEditor :: Text -> Name -> Editor Text Name
initKanbanEditor text name = editor name (Just 1) text

-- 保存看板标题
saveKanbanTitle :: AppState e Name -> EventM Name (AppState e Name)
saveKanbanTitle st = do
  let idx = st^.kanbanManagerFocus
      kanbans = _kanbans (st^.notes.kanbanData)  -- 修正：先获取 kanbanData，再访问 _kanbans
      editorContent = getEditContents (st^.kanbanEditor)
      newTitle = if Prelude.null editorContent then T.pack "Untitled" else head editorContent

      -- 更新看板标题
      updatedKanbans = if idx < length kanbans
                      then updateAt idx (\k -> k{_kanbanTitle = newTitle}) kanbans
                      else kanbans

      -- 更新看板数据
      updatedKanbanData = (st^.notes.kanbanData) { _kanbans = updatedKanbans }
      -- 使用记录语法更新 _kanbans 字段
      updatedState = st & notes.kanbanData .~ updatedKanbanData
                       & editingKanbanTitle .~ False

  -- 如果有持久化文件，则保存看板数据

  saveAndUpdateState updatedState

-- 在 Actions.hs 中添加这个函数

-- | 删除指定索引的看板
deleteKanbanAtIndex :: AppState e Name -> Int -> EventM Name (Next (AppState e Name))
deleteKanbanAtIndex = deleteKanbanWithPosition  -- 直接调用新函数

-- 使用通用的 addNoteToKanban 函数重构恢复最近删除笔记的函数
undoLastDelete :: AppState e Name -> EventM Name (Next (AppState e Name))
undoLastDelete st =
  case (st^.lastDeletedNote, st^.lastDeletedKanbanIndex) of
    (Just deletedNote, Just kanbanIdx) -> do
      -- 使用通用函数将笔记恢复到目标看板
      -- 如果是当前看板，设置焦点；否则不设置焦点
      let setFocus = kanbanIdx == st^.notes.currentKanban
          finalSt = addNoteToKanban st kanbanIdx deletedNote setFocus
                    & lastDeletedNote .~ Nothing  -- 清除已恢复的笔记
                    & lastDeletedKanbanIndex .~ Nothing

      -- 保存到文件
      _ <- saveAndUpdateState finalSt
      M.continue finalSt

    _ -> M.continue st  -- 没有可恢复的笔记

-- 添加到 Actions.hs 中

-- | 计算看板操作相关的位置信息
-- 用于新建和删除看板时确定正确的位置、更新对应文件的看板数量等
data KanbanPositionInfo = KanbanPositionInfo {
  -- 当前所在文件的索引
  fileIndex :: Int,
  -- 当前所在文件内的看板索引
  localIndex :: Int,
  -- 更新后的文件看板数量列表
  updatedLengthList :: [Int],
  -- 目标看板在全局列表中的索引
  globalTargetIndex :: Int,
  -- 更新后的 KanbanLocation
  updatedLocation :: KanbanLocation
}

-- | 计算新建看板时的位置信息
-- 新看板将添加到当前文件的最后位置
calculateNewKanbanPosition :: AppState e Name -> KanbanPositionInfo
calculateNewKanbanPosition st =
  let
    -- 获取当前位置信息
    curLocation = st^.notes.currentLocation
    fileIdx = curLocation^.kanbanFileIndex
    lengthList = curLocation^.kanbanLengthList

    -- 获取当前文件中的看板数量
    currentFileLength = lengthList !! fileIdx

    -- 新看板在当前文件中的索引 (添加到末尾)
    newLocalIndex = currentFileLength

    -- 更新看板长度列表，增加当前文件的看板数量
    updatedLengths = take fileIdx lengthList ++
                     [currentFileLength + 1] ++
                     drop (fileIdx + 1) lengthList

    -- 计算新看板在全局看板列表中的插入位置
    -- 需要计算当前文件之前的所有看板数量总和，加上当前文件中的位置
    previousFilesLength = sum $ take fileIdx lengthList
    newGlobalIndex = previousFilesLength + currentFileLength

    -- 更新位置信息，指向新创建的看板
    newLocation = curLocation & kanbanLocalIndex .~ newLocalIndex
                             & kanbanLengthList .~ updatedLengths
  in
    KanbanPositionInfo {
      fileIndex = fileIdx,
      localIndex = newLocalIndex,
      updatedLengthList = updatedLengths,
      globalTargetIndex = newGlobalIndex,
      updatedLocation = newLocation
    }

-- | 使用计算的位置信息创建新看板
createNewKanbanWithPosition :: AppState e Name -> AppState e Name
createNewKanbanWithPosition st =
  let
    -- 获取看板标题
    title = T.pack $ L.intercalate "" $ E.getEditContents (st^.kanbanTitleEditor)

    -- 创建新的Kanban对象
    newKanban = Kanban {
      _kanbanTitle = if T.null title then "New Kanban" else title,
      _childs = [],
      _kanbanSelected = True
    }

    -- 计算新看板的位置信息
    posInfo = calculateNewKanbanPosition st

    -- 首先保存当前看板数据
    currentNotes = st^.notes.noteData
    currentKanbanIndex = getCurrentKanbanIndex (st^.notes.currentLocation)
    kanbansList = st^.notes.kanbanData.kanbans

    -- 更新当前看板的笔记并取消选中状态
    updatedCurrentKanban = kanbansList & ix currentKanbanIndex . childs .~ currentNotes
                                      & ix currentKanbanIndex . kanbanSelected .~ False

    -- 添加新看板到计算出的位置
    updatedKanbans = take (globalTargetIndex posInfo) updatedCurrentKanban ++
                     [newKanban] ++
                     drop (globalTargetIndex posInfo) updatedCurrentKanban

    updatedKanbanData = st^.notes.kanbanData & kanbans .~ updatedKanbans
  in
    syncAppStateFocus $
    st & notes.kanbanData .~ updatedKanbanData
       & notes.syncedCurrentLocation .~ updatedLocation posInfo  -- 使用同步镜头更新位置
       & notes.noteData .~ []  -- 清空当前显示的笔记列表，因为新看板没有笔记
       & notes.totalNotes .~ 0 -- 更新笔记总数
       & selectedIndex .~ (-1) -- 重置选择索引
       & resetDialog
       & resetKanbanEdit

-- | 计算删除看板时的位置信息
-- 删除指定全局索引的看板，更新相关位置信息
calculateDeleteKanbanPosition :: AppState e Name -> Int -> (KanbanPositionInfo, Int)
calculateDeleteKanbanPosition st idxToDelete =
  let
    -- 获取当前位置信息
    curLocation = st^.notes.currentLocation
    lengthList = curLocation^.kanbanLengthList

    -- 找出要删除的看板所在的文件
    (deletedFileIdx, deletedLocalIdx) = globalToLocalIndex idxToDelete lengthList

    -- 更新该文件的看板数量 (减1)
    updatedLengths = take deletedFileIdx lengthList ++
                     [max 0 (lengthList !! deletedFileIdx - 1)] ++  -- 确保不会变为负数
                     drop (deletedFileIdx + 1) lengthList

    -- 确定要合并到的目标看板的全局索引
    -- 如果删除的是第一个看板，则合并到第二个看板
    -- 否则合并到前一个看板
    targetGlobalIdx = if idxToDelete == 0 && length (_kanbans (st^.notes.kanbanData)) > 1
                      then 1  -- 有多个看板时，如果删除第一个则合并到第二个
                      else max 0 (idxToDelete - 1)  -- 否则合并到前一个，确保不小于0

    -- 计算目标看板的文件和本地索引
    (targetFileIdx, targetLocalIdx) = globalToLocalIndex targetGlobalIdx lengthList

    -- 根据目前正在查看的看板和被删除的看板的关系，更新当前位置
    currentKanbanIdx = getCurrentKanbanIndex curLocation
    newLocation
      | currentKanbanIdx == idxToDelete
      = updateLocationFromIndex
          targetGlobalIdx (curLocation & kanbanLengthList .~ updatedLengths)
      | currentKanbanIdx > idxToDelete
      = let newGlobalIdx = currentKanbanIdx - 1
        in
          updateLocationFromIndex
            newGlobalIdx (curLocation & kanbanLengthList .~ updatedLengths)
      | otherwise = curLocation & kanbanLengthList .~ updatedLengths
  in
    (KanbanPositionInfo {
      fileIndex = deletedFileIdx,
      localIndex = deletedLocalIdx,
      updatedLengthList = updatedLengths,
      globalTargetIndex = targetGlobalIdx,
      updatedLocation = newLocation
    }, targetGlobalIdx)  -- 同时返回目标看板索引用于合并笔记

-- | 使用计算的位置信息删除看板
deleteKanbanWithPosition :: AppState e Name -> Int -> EventM Name (Next (AppState e Name))
deleteKanbanWithPosition st idx = do
  -- 计算删除看板的位置信息和目标看板索引
  let (posInfo, targetIdx) = calculateDeleteKanbanPosition st idx
      kanbans = _kanbans (st^.notes.kanbanData)

  -- 如果只有一个看板，不允许删除
  if length kanbans <= 1
    then M.continue st
    else do
      -- 获取要删除的看板和目标看板的笔记
      let kanbanToDelete = kanbans !! idx
          notesToMigrate = kanbanToDelete^.childs
          targetKanban = kanbans !! targetIdx
          targetNotes = targetKanban^.childs

          -- 合并笔记 - 将被删除看板的笔记添加到目标看板
          combinedNotes = targetNotes ++ notesToMigrate

          -- 更新目标看板的笔记
          updatedKanbans = kanbans & ix targetIdx . childs .~ combinedNotes

          -- 从看板列表中移除当前看板
          newKanbans = take idx updatedKanbans ++ drop (idx + 1) updatedKanbans

          -- 更新状态
          -- 使用记录语法更新 KanbanData
          updatedKanbanData = (st^.notes.kanbanData) { _kanbans = newKanbans }

          -- 获取当前查看的看板索引
          currentKanbanIndex = getCurrentKanbanIndex (st^.notes.currentLocation)
          -- 基本状态更新
          baseUpdatedSt = st & notes.kanbanData .~ updatedKanbanData
                            -- 调整看板管理器焦点
                            & kanbanManagerFocus .~ (if idx < st^.kanbanManagerFocus
                                                   then st^.kanbanManagerFocus - 1
                                                   else min (st^.kanbanManagerFocus) (length newKanbans - 1))
                            -- 使用同步镜头更新位置信息，确保文件长度列表正确更新
                            & notes.syncedCurrentLocation .~ updatedLocation posInfo

          -- 根据是否删除当前看板决定是否需要更新笔记数据
          updatedSt | currentKanbanIndex == idx
                      = baseUpdatedSt & notes.noteData .~ combinedNotes
                                         & notes.totalNotes .~ length combinedNotes
                                         & selectedIndex .~ (if Prelude.null combinedNotes then -1 else 0)
                    | currentKanbanIndex == targetIdx
                           -- 如果正在查看的是目标看板，更新其笔记列表为合并后的列表
                           = baseUpdatedSt & notes.noteData .~ combinedNotes
                                             & notes.totalNotes .~ length combinedNotes
                           -- 否则不需要特殊处理
                    | otherwise = baseUpdatedSt

      -- 使用 syncAndSave 函数同步状态并保存
      syncAndSave updatedSt

-- 在进入看板管理器时同步焦点到当前看板
enterKanbanManager :: AppState e Name -> EventM Name (Next (AppState e Name))
enterKanbanManager st = do
  -- 获取当前看板的全局索引
  let currentKanbanIdx = getCurrentKanbanIndex (st^.notes.currentLocation)

  -- 更新管理器焦点到当前看板
  let updatedSt = st & showKanbanManager .~ True
                     & kanbanManagerFocus .~ currentKanbanIdx

  M.continue updatedSt

-- 退出看板管理器并切换到选中的看板
exitKanbanManager :: AppState e Name -> EventM Name (Next (AppState e Name))
exitKanbanManager st = do
  -- 获取当前在看板管理器中选中的看板
  let selectedKanbanIdx = st^.kanbanManagerFocus
      currentKanbanIdx = getCurrentKanbanIndex (st^.notes.currentLocation)

  -- 如果选择的是当前看板，只需关闭管理器
  if selectedKanbanIdx == currentKanbanIdx
    then M.continue $ st & showKanbanManager .~ False
    else do
      -- 否则，切换到选中的看板
      let baseState = st & showKanbanManager .~ False
      switchToKanban baseState selectedKanbanIdx

-- 替换 switchToKanban 函数实现，使用 syncAndSave
switchToKanban :: AppState e Name -> Int -> EventM Name (Next (AppState e Name))
switchToKanban st targetKanbanIdx = do
  -- 基本检查
  let currentNotes = st^.notes.noteData
      currentKanbanIdx = getCurrentKanbanIndex (st^.notes.currentLocation)
      kanbansList = st^.notes.kanbanData.kanbans
      kanbanCount = L.length kanbansList

  -- 检查目标看板索引
  if targetKanbanIdx < 0 || targetKanbanIdx >= kanbanCount || targetKanbanIdx == currentKanbanIdx
    then M.continue st
    else do
      -- 1. 保存当前选择状态到映射表
      let currentSelectedIdx = st^.selectedIndex
          updatedFocusMap = if currentSelectedIdx >= 0
                          then updateOrInsertFocusMap currentKanbanIdx currentSelectedIdx (st^.kanbanFocusMap)
                          else st^.kanbanFocusMap

      -- 2. 使用 syncNotesToKanban 同步当前看板数据
      let syncedSt = syncNotesToKanban st

      -- 3. 更新看板选择状态
      let updatedKanbans = syncedSt^.notes.kanbanData.kanbans
                          & ix currentKanbanIdx . kanbanSelected .~ False
                          & ix targetKanbanIdx . kanbanSelected .~ True

      -- 4. 获取新看板的笔记和焦点信息
      let newKanbanNotes = (updatedKanbans !! targetKanbanIdx)^.childs
          targetFocusIdx = findKanbanFocus targetKanbanIdx updatedFocusMap
                          >>= \idx -> if idx >= 0 && idx < L.length newKanbanNotes
                                    then Just idx
                                    else Nothing
          finalFocusIdx = fromMaybe (if Prelude.null newKanbanNotes then -1 else 0) targetFocusIdx

      -- 5. 计算新的位置信息
      let newLocation = updateLocationFromIndex targetKanbanIdx (syncedSt^.notes.currentLocation)

      -- 6. 更新应用状态
      let updatedKanbanData = syncedSt^.notes.kanbanData & kanbans .~ updatedKanbans
          updatedSt = syncedSt & notes.kanbanData .~ updatedKanbanData
                              & notes.syncedCurrentLocation .~ newLocation
                              & notes.noteData .~ newKanbanNotes
                              & notes.totalNotes .~ L.length newKanbanNotes
                              & selectedIndex .~ finalFocusIdx
                              & kanbanFocusMap .~ updatedFocusMap

      -- 重置视口位置
      M.vScrollBy (M.viewportScroll MainViewPort) (-100)

      -- 使用 syncAndSave 函数同步状态并保存
      syncAndSave updatedSt

-- | 更新或插入看板焦点映射
updateOrInsertFocusMap :: Int -> Int -> [(Int, Int)] -> [(Int, Int)]
updateOrInsertFocusMap kanbanIdx focusIdx focusMap =
  let
    -- 查看是否已有该看板的映射
    existingEntry = Prelude.lookup kanbanIdx focusMap
  in
    case existingEntry of
      -- 如果已有，删除旧的映射并添加新的
      Just _ -> (kanbanIdx, focusIdx) : Prelude.filter (\(k, _) -> k /= kanbanIdx) focusMap
      -- 如果没有，直接添加
      Nothing -> (kanbanIdx, focusIdx) : focusMap

-- | 查找看板的焦点位置
findKanbanFocus :: Int -> [(Int, Int)] -> Maybe Int
findKanbanFocus  = Prelude.lookup

-- 函数实现
toggleNoteDisplayMode :: AppState e Name -> EventM Name (Next (AppState e Name))
toggleNoteDisplayMode st =
  case st^.selectedIndex of
    -- 没有选中笔记时不执行操作
    -1 -> M.continue st
    idx -> do
      -- 获取当前笔记
      let notesList = st^.(notes.noteData)
          currentNote = notesList !! idx
          -- 切换展示模式
          newMode = case st^.(notes.displayMode) of
                      ScrollDisplay -> FullDisplay
                      FullDisplay -> ScrollDisplay

      -- 更新应用状态中的显示模式
      let updatedSt = st & (notes.displayMode) .~ newMode

      -- 保存更新后的状态
      _ <- saveAndUpdateState updatedSt

      M.continue $ syncAppStateFocus updatedSt

-- 添加通用的笔记内容滚动函数
-- direction参数: 1表示向下滚动，-1表示向上滚动
scrollNoteContent :: AppState e Name -> Int -> EventM Name (Next (AppState e Name))
scrollNoteContent st direction = do
    -- 首先检查是否处于滚动显示模式
    if st^.(notes.displayMode) /= ScrollDisplay
      then M.continue st  -- 非滚动模式下不执行任何操作
      else do
        let selectedNote = st^.(notes.noteData) ^? ix (st^.selectedIndex)
        case selectedNote of
          Just note -> do
            -- 根据笔记类型选择正确的视口
            let viewportName = case note^.mode of
                  TodoList ->
                    let idx = st^.selectedIndex
                        baseId = generateTodoViewportId note idx
                        duplicateIdx = findDuplicateNoteIndex (st^.(notes.noteData)) idx
                        finalId = case duplicateIdx of
                                    Just _ -> baseId * 31 + idx
                                    Nothing -> baseId
                    in TodoListViewPort finalId

                  _ ->
                    let idx = st^.selectedIndex
                        baseId = generateViewportId note
                        duplicateIdx = findDuplicateNoteIndex (st^.(notes.noteData)) idx
                        finalId = case duplicateIdx of
                                    Just _ -> baseId * 31 + idx
                                    Nothing -> baseId
                    in NoteContentViewPort finalId

            -- 执行滚动
            M.vScrollBy (M.viewportScroll viewportName) direction
            M.continue st
          Nothing -> M.continue st

{-
-- 根据当前状态和方向键计算新的 (列, 行)
computeNewPosition :: (Int, Int) -> [(Int, Int, Int)] -> V.Key -> (Int, Int)
computeNewPosition (curCol, curRow) mapping key =
  let -- 根据列号查询该列高度
      colHeight c = maximum [ r + 1 | (c', r, _) <- mapping, c' == c ]
      -- 向上：如果行号为0，则移动到本列最后一行
      newPosUp    = (curCol, if curRow == 0 then colHeight curCol - 1 else curRow - 1)
      -- 向下：如果行号已达最后一行，则回到本列首行
      newPosDown  = (curCol, if curRow == colHeight curCol - 1 then 0 else curRow + 1)
      -- 向左：列向左（循环），行号保持不变或取新列最大可用行
      leftCol     = (curCol - 1 + (maximum (map (\(c,_,_) -> c+1) mapping))) `mod` (maximum (map (\(c,_,_) -> c+1) mapping))
      newPosLeft  = (leftCol, let nh = colHeight leftCol in if curRow < nh then curRow else nh - 1)
      -- 向右：类似向左
      rightCol    = (curCol + 1) `mod` (maximum (map (\(c,_,_) -> c+1) mapping))
      newPosRight = (rightCol, let nh = colHeight rightCol in if curRow < nh then curRow else nh - 1)
  in case key of
       V.KUp    -> newPosUp
       V.KDown  -> newPosDown
       V.KLeft  -> newPosLeft
       V.KRight -> newPosRight
       _        -> (curCol, curRow)

navigateGrid :: AppState e Name -> V.Key -> EventM Name (Next (AppState e Name))
navigateGrid st direction = do
  layout <- calculateNavigationLayout st
  let currentIdx = st^.selectedIndex
      noteCount = length (st^.notes.noteData)
      maxxCols = layout^.maxCols
      mapping = generateColumnMajorMapping noteCount maxxCols
      -- 查找当前笔记的 (col, row)；若找不到则默认 (0,0)
      (curCol, curRow, _) = fromMaybe (0,0,0) (find (\(_,_,idx) -> idx == currentIdx) mapping)
      (newCol, newRow) = computeNewPosition (curCol, curRow) mapping direction
      -- 从 mapping 中查找对应 (newCol, newRow) 的全局索引
      newIdx = case find (\(c, r, _) -> c == newCol && r == newRow) mapping of
                 Just (_,_,idx) -> idx
                 Nothing      -> currentIdx
  if noteCount == 0 || newIdx == currentIdx
    then M.continue st
    else M.continue $ selectNoteByIndex st newIdx
-}

navigateGrid :: AppState e Name -> V.Key -> EventM Name (Next (AppState e Name))
navigateGrid st direction = do
  -- 获取视口信息
  mVp <- M.lookupViewport MainViewPort
  let totalWidth = maybe 80 (fst . (^. vpSize)) mVp  -- 默认宽度为80

      -- 使用 calculateLayoutPure 计算布局
      (_, _, gridPositions, updatedSt) = calculateLayoutPure st totalWidth

      noteCount = length (updatedSt^.notes.noteData)

      -- 计算新索引
      currentIdx = updatedSt^.selectedIndex
      newIdx = case direction of
                 V.KUp   -> (currentIdx - 1 + noteCount) `mod` noteCount
                 V.KDown -> (currentIdx + 1) `mod` noteCount
                 V.KLeft ->
                   let sortedPositions = sortOn (\(_, pos) -> (pos^.row, pos^.col)) gridPositions
                       currentPos = fromMaybe 0 $ findIndex (\(idx, _) -> idx == currentIdx) sortedPositions
                       newPos = if currentPos == 0 then length sortedPositions - 1 else currentPos - 1
                   in fst (sortedPositions !! newPos)
                 V.KRight ->
                   let sortedPositions = sortOn (\(_, pos) -> (pos^.row, pos^.col)) gridPositions
                       currentPos = fromMaybe 0 $ findIndex (\(idx, _) -> idx == currentIdx) sortedPositions
                       newPos = if currentPos == length sortedPositions - 1 then 0 else currentPos + 1
                   in fst (sortedPositions !! newPos)
                 _ -> currentIdx

  if noteCount == 0 || newIdx == currentIdx
    then M.continue updatedSt
    else M.continue $ selectNoteByIndex updatedSt newIdx  -- 使用更新后的状态选择新笔记

-- 修复 togglePackTask 函数中的镜头使用
togglePackTask :: AppState e Name -> EventM Name (Next (AppState e Name))
togglePackTask appState = do
  let currentIdx = appState^.notes.tempTodoNote.selectedTaskIndex
      tasksList = appState^.notes.tempTodoNote.tasks  -- 使用正确的镜头路径

  -- 确保当前选中了有效的任务
  if currentIdx < 0 || currentIdx >= length tasksList
  then M.continue appState  -- 无选中任务
  else
    let task = tasksList !! currentIdx
    in if _packed task
       then handleUnpackTask appState currentIdx  -- 任务已打包，执行解包
       else handlePackTask appState currentIdx    -- 任务未打包，执行打包

-- 修改 togglePackTask 函数 - 无需更改，它只需基于索引找到任务并检查打包状态

-- 修改 handlePackTask 函数
handlePackTask :: AppState e Name -> Int -> EventM Name (Next (AppState e Name))
handlePackTask st idx = do
  let tasksList = st^.notes.tempTodoNote.tasks
      totalTasks = length tasksList

  -- 确保有效索引
  if idx < 0 || idx >= totalTasks
  then M.continue st
  else do
    let task = tasksList !! idx
        currentLevel = _level task

        -- 查找子任务
        childrenIndices = takeWhile
                          (\i -> i < totalTasks && _level (tasksList !! i) > currentLevel)
                          [idx + 1..totalTasks - 1]

    -- 生成当前任务的ID
    let taskId = generateTaskId task
        oldPackMap = st^.notes.taskPackMap
        existingPackInfo = lookup taskId oldPackMap

    -- 两种情况处理:
    -- 1. 有子任务时，执行正常打包
    -- 2. 无子任务但在映射中找到打包信息时，标记为打包状态
    if not (Prelude.null childrenIndices)
    then do
      -- 有子任务，执行打包操作
      let (packedTask, updatedTasks) = packTasks tasksList idx

          -- 更新任务包映射
          newPackMap = (taskId, packedTask) : filter (\(id, _) -> id /= taskId) oldPackMap

      -- 更新状态并返回
      M.continue $ st & notes.tempTodoNote.tasks .~ updatedTasks
                     & notes.taskPackMap .~ newPackMap
                     & updateSelectedTaskIndex True (const idx)
    else do
      -- 无子任务，检查是否有匹配的打包信息
      case existingPackInfo of
        Nothing ->
          -- 没有子任务也没有匹配的打包信息，不执行操作
          M.continue st
        Just packedTask ->
          -- 有匹配的打包信息，仅将任务标记为已打包
          let packedTaskOnly = task & packed .~ True
              updatedTasks = take idx tasksList ++ [packedTaskOnly] ++ drop (idx + 1) tasksList
          in M.continue $ st & notes.tempTodoNote.tasks .~ updatedTasks
                            & updateSelectedTaskIndex True (const idx)

-- 修改 handleUnpackTask 函数，解包后保留打包信息
handleUnpackTask :: AppState e Name -> Int -> EventM Name (Next (AppState e Name))
handleUnpackTask st idx = do
  let tasksList = st^.notes.tempTodoNote.tasks
      packMap = st^.notes.taskPackMap

  -- 确保当前选中的是已打包任务
  if idx < 0 || idx >= length tasksList || not (_packed (tasksList !! idx))
  then M.continue st
  else
    -- 从任务包映射中获取打包信息
    let currentTask = tasksList !! idx
        taskId = generateTaskId currentTask
        packedTaskInfo = lookup taskId packMap
    in case packedTaskInfo of
      Nothing ->
        -- 找不到打包信息，只需将任务标记为未打包
        let unpackedTask = currentTask & packed .~ False
            updatedTasks = take idx tasksList ++ [unpackedTask] ++ drop (idx + 1) tasksList
        in M.continue $ st & notes.tempTodoNote.tasks .~ updatedTasks
                          & updateSelectedTaskIndex True (const idx)

      Just packedTasks -> do
        -- 使用解包函数处理，将子任务插回原列表
        let unpackedTasks = unpackTasks packedTasks idx tasksList

        -- 关键修改：不从映射中移除打包信息
        -- 这样即使解包后，之前的打包信息仍然保留

        -- 更新状态并返回
        M.continue $ st & notes.tempTodoNote.tasks .~ unpackedTasks
                       & updateSelectedTaskIndex True (const idx)