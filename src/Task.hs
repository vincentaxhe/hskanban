module Task(
    -- 导出Task相关函数
    Task(..),
    getTask,
    getTasks,
    renderTask,
    emptyTask,
    -- 导出从Note.hs移动过来的函数
    defaultTaskFocus,
    newTaskWidget,
    todoTitleWidget,
    todoHighlightWidget,
    getTodoNote,
    createTodoNote,
    emptyEditor,
    initTasks,
    flattenTaskTree,
    rebuildTaskTree,
    packTasks,
    unpackTasks,
    toggleTaskPack,
    isTaskPacked,
    updateKanbanTaskLevels
) where

import           Brick.Types
import           Brick.Widgets.Core
import           Brick.Widgets.Edit as E (getEditContents, editor, Editor, renderEditor)
import           Data.List          as L (intercalate, length, map, findIndex, maximumBy, elemIndex)
import qualified Data.Text          as T
import           Data.Maybe         (mapMaybe)
import           Lens.Micro         ((^.), (.~), (&), (%~))
import           Types
import           Brick.Focus
import Data.Text (Text)
import KbUtils
getTask :: AppState e n -> Bool -> Task
getTask st selected = emptyTask
    & task .~ T.pack (L.intercalate "" $ E.getEditContents (st^.(notes . taskEdit)))
    & selectedTask .~ selected

getTasks :: AppState e n -> [Widget Name]
getTasks st = L.map renderTask (st^.notes . tempTodoNote . tasks)

-- 修改任务渲染函数，增加缩进支持

renderTask :: Task -> Widget Name
renderTask task =
  let
      -- 计算缩进空格
      indentation = replicate (_level task * 2) ' '

      -- 任务文本（带复选框）
      taskText = if _packed task
                 then indentation ++ "[+] " ++ T.unpack (_task task)  -- 使用[+]表示已打包任务
                 else indentation ++ (if _selectedTask task then "[X] " else "[ ] ") ++ T.unpack (_task task)

      -- 根据任务状态选择样式
      taskAttr | _status task = withAttr "taskHighlighted"
               | _packed task = withAttr "packedTask"  -- 已打包任务使用特殊样式
               | _selectedTask task = withAttr "completedTask"
               | otherwise = withAttr "normalTask"
  in
      taskAttr $ str taskText

emptyTask :: Task
emptyTask = Task{
    _status = False,
    _task = "",
    _selectedTask = False,
    _level = 0,
    _subTasks = [],
    _packed = False  -- 初始未打包
}

-- 从 Note.hs 移动过来的任务相关函数
defaultTaskFocus :: FocusRing Name
defaultTaskFocus = focusRing [TaskTitle, TaskEdit, Tasks, Checkbox]

newTaskWidget :: AppState e Name -> Widget Name
newTaskWidget st = txt (mconcat [st^.notes.taskEditLabel,": "]) <+> hLimit 30 (vLimit 1 $ withFocusRing (st^.(notes .focusEdit)) (renderEditor (str . unlines)) (st^.(notes . taskEdit)))

todoTitleWidget :: AppState e Name -> Widget Name
todoTitleWidget st = str "Title:    " <+> hLimit 30 (vLimit 1 $ withFocusRing (st^.(notes .focusEdit)) (renderEditor (str . unlines)) (st^.(notes . taskTitle)))

todoHighlightWidget :: AppState e Name -> Widget Name
todoHighlightWidget st = padBottom (Pad 1) $ padTop (Pad 1) render
  where render  | st^.notes.tempTodoNote.highlighted = highlightAttribute $ str "[X] Important ?"
                | otherwise = highlightAttribute $ str "[ ] Important ?"
        highlightAttribute  | st^.notes.tempTodoNote.checkBoxSelected = withAttr "taskHighlighted"
                            | otherwise = withAttr "importantOption"  -- 使用新属性名

getTodoNote :: Text -> [Task] -> Note
getTodoNote title tsk = Note{
                  _title = title,
                  _content = "",
                  _selected = False,
                  _checkBoxSelected = False,
                  _highlighted=False,
                  _tasks = tsk,
                  _selectedTaskIndex = -1,
                  _mode = TodoList
                }

createTodoNote :: AppState e Name -> Note
createTodoNote st = updatedTempNote ^. (notes . tempTodoNote)
  where updatedTempNote = st & (notes . tempTodoNote . title) .~ getTodoTitle st
        getTodoTitle st = T.pack $ unlines $ getEditContents (st^.notes.taskTitle)

emptyEditor :: Name -> Editor String Name
emptyEditor name = editor name Nothing ""

-- 修改函数签名，接收 KanbanData 和 fileInfos 参-- 修改函数签名，接收 KanbanData 和 fileInfos 参数
initTasks :: [Note] -> KanbanData -> [FileInfo] -> Notes
initTasks notes kanbanData fileInfos =
  let initialNotes = Notes{
    _taskEdit = editor TaskEdit Nothing "",
    _taskEditLabel = "New Task",
    _taskTitle = editor TaskTitle Nothing "",
    _focusEdit = defaultTaskFocus,
    _totalNotes = L.length notes,
    _taskEditMode = False,
    _noteData = notes,
    _tempTodoNote = getTodoNote "" [],
    _kanbanData = kanbanData,
    _currentKanban = 0,
    _insertTaskIndex = Nothing,
    _currentLocation = KanbanLocation {
      _kanbanFileIndex = 0,  -- 默认使用第一个文件
      _kanbanLocalIndex = 0, -- 默认选择该文件的第一个看板
      _kanbanLengthList = L.map _kanbanCount fileInfos  -- 从fileInfos获取每个文件的看板数量
    },
    _displayMode = ScrollDisplay,
    _taskPackMap = []
  }
  -- 直接返回初始化的笔记对象，不使用未定义的syncedCurrentKanbanIndex
  in initialNotes

-- | 将嵌套任务树展平为线性列表，同时设置正确的级别
flattenTaskTree :: [Task] -> [Task]
flattenTaskTree = concatMap (flattenTask 0)
  where
    -- 递归处理单个任务及其子任务
    flattenTask :: Int -> Task -> [Task]
    flattenTask currentLevel task =
      -- 当前任务（更新级别，清空子任务）
      (task & level .~ currentLevel & subTasks .~ []) :
      -- 递归处理所有子任务，增加级别
      concatMap (flattenTask (currentLevel + 1)) (_subTasks task)

-- | 重建任务树，更加健壮地处理层级关系
rebuildTaskTree :: [Task] -> [Task]
rebuildTaskTree [] = []
rebuildTaskTree tasks =
  let
    -- 第一步：规范化所有任务的层级（确保层级是连续的整数，从0开始）
    normalizedTasks = normalizeLevels tasks

    -- 第二步：使用规范化后的任务构建树
    rootTasks = buildTaskTree normalizedTasks
  in
    rootTasks

-- | 规范化任务层级，确保任务树结构正确
normalizeLevels :: [Task] -> [Task]
normalizeLevels [] = []
normalizeLevels tasks =
  let
    -- 使用有序映射记录每个缩进位置对应的规范化层级
    -- 结构是 [(实际缩进, 规范化层级)]，保持有序
    processWithIndents :: [Task] -> [(Int, Int)] -> [Task]
    processWithIndents [] _ = []
    processWithIndents (task:restTasks) indents =
      let
        currentIndent = _level task

        -- 找到所有小于当前缩进的有效缩进
        validIndents = filter (\(indent, _) -> indent < currentIndent) indents

        -- 确定新任务的规范化层级
        (normalizedLevel, updatedIndents)
          | null indents = (0, [(currentIndent, 0)])
          | currentIndent == 0 = (0, [(0, 0)])
          | null validIndents = (1, (currentIndent, 1) : indents)
          | otherwise
          = let
              (parentIndent, parentLevel)
                = maximumBy (\ (i1, _) (i2, _) -> compare i1 i2) validIndents
              newLevel = parentLevel + 1
              cleanedIndents
                = filter (\ (indent, _) -> indent < currentIndent) indents
              newIndents = (currentIndent, newLevel) : cleanedIndents
            in (newLevel, newIndents)

        -- 更新当前任务的层级
        updatedTask = task & level .~ normalizedLevel

        -- 递归处理剩余任务
        processedRest = processWithIndents restTasks updatedIndents
      in
        updatedTask : processedRest
  in
    processWithIndents tasks []

-- | 基于规范化层级构建任务树
buildTaskTree :: [Task] -> [Task]
buildTaskTree [] = []
buildTaskTree tasks =
  let
    -- 找出所有根任务（0级任务）
    rootTasks = filter (\t -> _level t == 0) tasks

    -- 为任务构建子树
    buildWithChildren :: Task -> Task
    buildWithChildren task =
      -- 获取当前任务在列表中的索引
      let taskIndex = case elemIndex task tasks of
            Just idx -> idx
            Nothing -> error "Task not found in list (should never happen)"

          -- 获取当前任务之后的所有任务
          tasksAfter = drop (taskIndex + 1) tasks

          -- 找到下一个同级或更高级任务的位置
          nextSameLevelOrHigherIndex = findIndex (\t -> _level t <= _level task) tasksAfter

          -- 确定可能包含子任务的范围
          potentialChildren = case nextSameLevelOrHigherIndex of
            Just idx -> take idx tasksAfter
            Nothing -> tasksAfter  -- 如果没找到，则所有后续任务都可能是子任务

          -- 筛选出直接子任务（层级恰好比当前任务高1）
          directChildren = filter (\t -> _level t == _level task + 1) potentialChildren

          -- 递归处理每个直接子任务
          processedChildren = map buildWithChildren directChildren
      in
        task & subTasks .~ processedChildren
  in
    map buildWithChildren rootTasks


-- | 根据任务的嵌套结构更新所有任务的级别
updateTaskLevels :: [Task] -> [Task]
updateTaskLevels = updateTaskLevelsHelper 0

-- | 递归辅助函数，处理特定级别的任务
updateTaskLevelsHelper :: Int -> [Task] -> [Task]
updateTaskLevelsHelper currentLevel = map (\task ->
    -- 更新当前任务的级别
    let updatedTask = task & level .~ currentLevel
        -- 递归更新子任务的级别
        updatedSubTasks = updateTaskLevelsHelper (currentLevel + 1) (task^.subTasks)
    in
        -- 返回完整更新的任务
        updatedTask & subTasks .~ updatedSubTasks
  )

-- 新增函数：判断任务是否已打包
isTaskPacked :: Task -> Bool
isTaskPacked = _packed

-- 新增函数：切换任务的打包状态
toggleTaskPack :: Task -> Task
toggleTaskPack task = task & packed %~ not

-- | 打包任务及其子任务，从任务列表中移除子任务
packTasks :: [Task] -> Int -> (PackedTasks, [Task])
packTasks tasks parentIdx
  | parentIdx < 0 || parentIdx >= length tasks =
      -- 索引无效时创建一个空的包和不变的任务列表
      (PackedTasks emptyTask [], tasks)
  | otherwise =
      let parentTask = tasks !! parentIdx
          parentLevel = _level parentTask

          -- 保存任务ID用于解包查找
          taskId = generateTaskId parentTask

          -- 查找所有子任务的索引（缩进级别大于父任务的连续任务）
          childIndices = takeWhile
                          (\i -> i < length tasks && _level (tasks !! i) > parentLevel)
                          [parentIdx + 1..]

          -- 收集子任务和它们相对于父任务的缩进级别
          childTasks = [(_level (tasks !! i) - parentLevel, tasks !! i) | i <- childIndices]

          -- 标记父任务为已打包
          packedParent = parentTask & packed .~ True

          -- 创建打包任务对象
          packedTasks = PackedTasks packedParent childTasks

          -- 更新任务列表：
          -- 1. 保留父任务前的所有任务
          -- 2. 插入更新后的父任务（已标记为打包）
          -- 3. 跳过所有子任务（从列表中移除）
          updatedTasks = take parentIdx tasks ++
                         [packedParent] ++
                         drop (parentIdx + 1 + length childIndices) tasks
      in
          (packedTasks, updatedTasks)

-- | 解包任务，将子任务插入回任务列表
unpackTasks :: PackedTasks -> Int -> [Task] -> [Task]
unpackTasks (PackedTasks parent children) insertIdx tasks =
  if insertIdx < 0 || insertIdx > length tasks
  then tasks  -- 插入位置无效
  else
    let
      -- 获取插入位置的缩进级别
      baseLevel = if insertIdx < length tasks
                  then _level (tasks !! insertIdx)
                  else 0

      -- 移除父任务的打包标记
      unpackedParent = parent & packed .~ False & level .~ baseLevel

      -- 调整子任务的缩进级别
      adjustedChildren = [child & level .~ (baseLevel + relLevel) | (relLevel, child) <- children]

      -- 替换父任务
      tasksWithParent = take insertIdx tasks ++ [unpackedParent] ++ drop (insertIdx + 1) tasks

      -- 在父任务之后插入所有子任务
      finalTasks = take (insertIdx + 1) tasksWithParent ++
                   adjustedChildren ++
                   drop (insertIdx + 1) tasksWithParent
    in finalTasks

-- 更新看板数据中所有任务的级别信息
updateKanbanTaskLevels :: KanbanData -> KanbanData
updateKanbanTaskLevels kanbanData =
  kanbanData & kanbans %~ map updateKanbanChildTasks

-- 更新单个看板中所有笔记的任务级别
updateKanbanChildTasks :: Kanban -> Kanban
updateKanbanChildTasks kanban =
  kanban & childs %~ map updateNoteTaskLevels

-- 更新单个笔记中的任务级别
updateNoteTaskLevels :: Note -> Note
updateNoteTaskLevels note =
  if note^.mode == TodoList
  then note & tasks %~ updateTasksWithLevel
  else note

-- 递归更新任务及其子任务的级别
updateTasksWithLevel :: [Task] -> [Task]
updateTasksWithLevel = updateTasksWithLevelHelper 0

-- 递归辅助函数，处理特定级别的任务
updateTasksWithLevelHelper :: Int -> [Task] -> [Task]
updateTasksWithLevelHelper currentLevel = map (\task ->
    -- 更新当前任务的级别
    let updatedTask = task & (level .~ currentLevel)
        -- 递归更新子任务的级别
        updatedSubTasks = updateTasksWithLevelHelper (currentLevel + 1) (task^.subTasks)
    in
        -- 返回完整更新的任务
        updatedTask & subTasks .~ updatedSubTasks
  )

