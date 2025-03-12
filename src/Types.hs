{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module Types where

import Brick.Focus ( FocusRing )
import Brick.Forms ( Form, focusedFormInputAttr )
import Brick.Widgets.Dialog ( Dialog )
import Brick.Widgets.Edit ( Editor )
import Data.Aeson
    ( (.!=),
      (.:),
      (.:?),
      withObject,
      withText,
      object,
      FromJSON(parseJSON),
      Value(String),
      KeyValue((.=)),
      ToJSON(toJSON) )
import           Data.Maybe (catMaybes)
import           Data.Text as T ( null, unpack, Text )
import           GHC.Generics ( Generic )
import           Lens.Micro.TH        (makeLenses)
import Brick.AttrMap ( AttrMap, attrMap, attrName )
import qualified Graphics.Vty               as V
import           Brick.Util                 (fg, on)
import qualified Brick.Widgets.Edit         as E (editAttr, editFocusedAttr)


data Name = TaskEdit
          | TaskTitle
          | Tasks
          | Checkbox
          | MainViewPort
          | KanbanViewPort
          | KanbanTitle
          | KanbanListViewPort
          | NoteContentViewPort Int
          | TodoListViewPort Int
          | TitleField
          | ContentField
          | HighlightField
          | KanbanTitleField
          | TaskListVP   -- 新增：任务列表视口标识
          | None
  deriving (Eq, Ord, Show)

newtype InputType = FileInput [FilePath] deriving (Show, Eq)

newtype CmdOptions = CmdOptions {
  inputType :: InputType
}

data Choice = Save | Cancel deriving (Show)

data NoteMode = TodoList | FreeNote | InvalidMode deriving (Show,Eq,Generic,Ord)

-- 跟踪文件及其包含的看板数量
data FileInfo = FileInfo {
  _filePath :: FilePath,
  _kanbanCount :: Int
} deriving (Show, Eq)

-- 为新类型创建镜头
makeLenses ''FileInfo

-- 添加这两个实例
instance ToJSON NoteMode where
    toJSON TodoList = String "TodoList"
    toJSON FreeNote = String "FreeNote"
    toJSON InvalidMode = String "InvalidMode"

instance FromJSON NoteMode where
    parseJSON = withText "NoteMode" $ \case
        "TodoList" -> return TodoList
        "FreeNote" -> return FreeNote
        "InvalidMode" -> return InvalidMode
        _ -> fail "Invalid NoteMode value"

data DialogMode = TodoCreate | NoteCreate | TodoEdit | NoteEdit | ChoiceCreate | KanbanCreate
  deriving (Show,Eq,Generic,Ord)

data NotesPersist = PersistFile FilePath | PersistNone

---------------------------------------------------
-- 修改 Task 数据类型，添加 _packed 字段但不序列化
data Task = Task{
  _status       :: Bool,     -- 用于标记任务是否选中（光标位置）
  _selectedTask :: Bool,     -- 用于标记任务完成状态（空格键切换）
  _task         :: T.Text,
  _subTasks     :: [Task],   -- 子任务列表
  _level        :: Int,      -- 运行时缩进级别，不保存到JSON
  _packed       :: Bool      -- 新增：标记任务是否已打包（运行时属性，不保存）
} deriving (Generic, Eq)

-- 修改 Show 实例，考虑打包状态
instance Show Task where
  show task = replicate (_level task * 2) ' ' ++ display ++ " " ++ T.unpack (_task task)
    where
      display | _packed task = "[+]"  -- 打包的任务显示为 [+]
              | _selectedTask task = "[X]"
              | otherwise = "[ ]"

-- JSON 实例保持不变，不序列化 _packed
instance ToJSON Task where
    toJSON t = object $
      [ "_task"         .= _task t
      , "_selectedTask" .= _selectedTask t
      ] ++
      -- 只有当子任务非空时才包含这个字段
      (["_subTasks" .= _subTasks t | not (Prelude.null (_subTasks t))])

-- 在反序列化时，_packed 始终设置为 false
instance FromJSON Task where
    parseJSON = withObject "Task" $ \o -> do
         taskText   <- o .: "_task"
         sel        <- o .: "_selectedTask"
         subTasks   <- o .:? "_subTasks" .!= []
         -- 设置初始级别为0，子任务级别在加载后计算
         return Task { _task = taskText,
                      _selectedTask = sel,
                      _status = False,
                      _subTasks = subTasks,
                      _level = 0,
                      _packed = False }

-- 包含父任务和所有子任务（带相对级别）
data PackedTasks = PackedTasks {
  _parentTask    :: Task,              -- 父任务
  _childrenTasks :: [(Int, Task)]      -- (相对级别, 子任务) 列表
} deriving (Generic, Eq, Show)

-- 创建 PackedTasks 的镜头
makeLenses ''PackedTasks

type TaskPackMap = [(Int, PackedTasks)]

-- 默认未打包
---------------------------------------------------
-- 对 Note 的自定义 JSON 实现
data Note = Note {
  _title             :: Text,
  _content           :: Text,
  _selected          :: Bool,
  _tasks             :: [Task],
  _selectedTaskIndex :: Int,
  _mode              :: NoteMode,
  _highlighted       :: Bool,
  _checkBoxSelected  :: Bool
} deriving (Show,Generic,Eq)

instance ToJSON Note where
  toJSON n = object $ catMaybes
    [ Just ("_title" .= _title n)
    , if T.null (_content n) then Nothing else Just ("_content" .= _content n)
    , Just ("_mode" .= _mode n)
    , Just ("_highlighted" .= _highlighted n)
    , if Prelude.null (_tasks n) then Nothing else Just ("_tasks" .= _tasks n)
    -- 不保存 _selected, _selectedTaskIndex, _checkBoxSelected
    ]

instance FromJSON Note where
  parseJSON = withObject "Note" $ \o -> do
    title       <- o .: "_title"
    content     <- o .:? "_content" .!= ""
    mode        <- o .: "_mode"
    highlighted <- o .: "_highlighted"
    tasks       <- o .:? "_tasks" .!= []
    return Note { _title             = title
                , _content           = content
                , _selected          = False
                , _tasks             = tasks
                , _selectedTaskIndex = -1
                , _mode              = mode
                , _highlighted       = highlighted
                , _checkBoxSelected  = False
                }

makeLenses ''Note

---------------------------------------------------
-- 修改看板数据类型部分，将冲突的字段重命名

-- 看板数据类型
data Kanban = Kanban {
  _kanbanTitle   :: T.Text,
  _childs        :: [Note],
  _kanbanSelected :: Bool   -- 不使用 _selected 避免冲突
} deriving (Show, Generic, Eq)

instance ToJSON Kanban where
  toJSON k = object
    [ "title"   .= _kanbanTitle k
    , "childs"  .= _childs k
    -- 不保存 _kanbanSelected
    ]

instance FromJSON Kanban where
  parseJSON = withObject "Kanban" $ \o -> do
    title <- o .: "title"
    notes <- o .: "childs"
    return Kanban { _kanbanTitle = title
                  , _childs      = notes
                  , _kanbanSelected = False
                  }

makeLenses ''Kanban

-- 表示整个应用程序数据的类型
newtype KanbanData = KanbanData {
  _kanbans :: [Kanban]
} deriving (Show, Generic, Eq)


makeLenses ''KanbanData
-- 修改 KanbanData 的 ToJSON 和 FromJSON 实例，直接使用数组而非对象

instance ToJSON KanbanData where
  toJSON :: KanbanData -> Value
  toJSON kd = toJSON (_kanbans kd)  -- 直接将看板数组转为 JSON 数组

instance FromJSON KanbanData where
  parseJSON v = do
    kanbans <- parseJSON v  -- 直接从 JSON 数组解析看板数组
    return KanbanData { _kanbans = kanbans }


---------------------------------------------------
-- 定义看板位置类型，记录文件和看板索引
data KanbanLocation = KanbanLocation {
  _kanbanFileIndex :: Int,  -- 文件在 fileInfos 中的索引
  _kanbanLocalIndex :: Int,  -- 看板在文件内的索引
  _kanbanLengthList :: [Int]
} deriving (Show, Eq, Generic)

makeLenses ''KanbanLocation

data DisplayMode = FullDisplay | ScrollDisplay
  deriving (Show, Eq)

-- 修改 Notes 中的相关字段
data Notes = Notes{
  _totalNotes    :: Int,
  _noteData      :: [Note],
  _tempTodoNote  :: Note,
  _taskEdit      :: Editor String Name,
  _taskEditLabel :: T.Text,
  _taskTitle     :: Editor String Name,
  _taskEditMode  :: Bool,
  _focusEdit     :: FocusRing Name,
  _kanbanData    :: KanbanData,
  _currentKanban :: Int,
  _currentLocation :: KanbanLocation,
  _insertTaskIndex :: Maybe Int,
  _displayMode :: DisplayMode,
  _taskPackMap  :: TaskPackMap
}

makeLenses ''Notes

-- | 表示笔记在网格中的位置
data GridPosition = GridPosition {
  _row :: Int,  -- 行索引
  _col :: Int   -- 列索引
} deriving (Show, Eq)

makeLenses ''GridPosition

-- | 布局计算结果
data LayoutResult = LayoutResult {
  _gridPositions :: [(Int, GridPosition)],  -- 笔记索引到网格位置的映射
  _maxRows :: Int,                          -- 总行数
  _maxCols :: Int                           -- 总列数
} deriving (Show)

makeLenses ''LayoutResult

-- 在 AppState 定义中添加剪贴板字段
data AppState e n = AppState {
  _notes           :: Notes,
  _selectedIndex   :: Int,
  _showDialog      :: Bool,
  _dialogMode      :: DialogMode,
  _dialogSelect    :: DialogMode,
  _showHelp        :: Bool,
  _form            :: Form Note e n,
  _editMode        :: Bool,
  _lastDeletedNote :: Maybe Note,           -- 最近删除的笔记
  _lastDeletedKanbanIndex :: Maybe Int,     -- 删除笔记所在的看板索引
  _fileInfos       :: [FileInfo],
  _dlg             :: Dialog Choice,
  _kanbanTitleEditor :: Editor String Name,
  _kanbanFocus :: FocusRing Name,
  _clipboard       :: [Note],  -- 新增：剪贴板字段存储剪切的笔记
  _showKanbanList  :: Bool,  -- 跟踪是否显示看板列表
  _expandKanbanNotes :: Bool,  -- 新增：是否展开看板下的笔记
  _showKanbanManager :: Bool,           -- 是否显示看板管理界面
  _kanbanManagerFocus :: Int,           -- 在看板管理界面选中的看板索引
  _editingKanbanTitle :: Bool,          -- 是否正在编辑看板标题
  _kanbanEditor :: Editor Text n,       -- 看板标题编辑器
  _kanbanFocusMap :: [(Int, Int)]
}

-- makeLenses ''AppState 将自动为 _expandKanbanNotes 创建透镜

-- 确保更新 makeLenses
makeLenses ''AppState
makeLenses ''Task

-- makeLenses ''AppState 已经为 _showKanbanList 创建了相应的透镜

-- 修改 MarkerStyle 数据类型
data MarkerStyle = MarkerStyle {
    markerSymbol :: Char,           -- 标记符号
    markerDesc   :: String,         -- 标记描述
    markerName   :: String,         -- 对应的属性名称字符串(不是AttrName)
    markerColor  :: V.Attr          -- 直接存储颜色属性
}

-- 修改 markerStyles 列表，使用字符串而非AttrName
markerStyles :: [MarkerStyle]
markerStyles = [
    MarkerStyle '!' "反对的" "opposingIdea" (fg V.red),                    -- 反对的，红色
    MarkerStyle '&' "支持的" "supportingIdea" (fg V.green),                -- 支持的，绿色
    MarkerStyle '?' "疑惑的" "questioningIdea" (fg V.yellow),              -- 疑惑的，黄色
    MarkerStyle '+' "开放的" "openIdea" (fg V.blue),                       -- 开放的，蓝色
    MarkerStyle '-' "现实的" "realisticIdea" (fg V.cyan),                 -- 现实的，白色
    MarkerStyle '=' "现实路径" "pathIdea" (fg V.magenta),                  -- 现实路径，紫色
    MarkerStyle '>' "验证后的" "validatedIdea" (fg V.brightMagenta),        -- 验证后的，粉色
    MarkerStyle '@' "优先级" "priorityIdea" (fg V.red `V.withStyle` V.bold) -- 优先级，深红色加粗
    ]

theMap :: AttrMap
theMap = attrMap V.defAttr (baseAttrs ++ markerAttrs)
  where
    baseAttrs =
      [ (E.editAttr, V.white `on` V.black),
        (E.editFocusedAttr, V.white `on` V.black),
        ("highlightedNote", V.black `on` V.yellow),      -- 重要笔记的黄色背景
        ("highlightedNoteText", V.black `on` V.yellow),  -- 高亮笔记文本样式
        ("selectedNoteText", fg V.brightWhite),          -- 选中笔记的文本样式
        ("normalNoteText", fg V.white),                  -- 普通笔记的文本样式
        ("noteTitle", fg V.brightCyan),                  -- 笔记标题样式
        ("taskHighlighted", fg V.yellow),
        (focusedFormInputAttr, fg V.yellow),
        ("normalTask", fg V.white),
        ("kanbanHeader", fg V.cyan),
        ("activeKanban", V.black `on` V.cyan),           -- 当前看板使用反转色
        ("kanbanTitle", fg V.blue),
        ("clipboardInfo", fg V.magenta),
        ("activeNote", V.brightWhite `on` V.red),        -- 当前选中的笔记使用红色背景
        ("activeNoteBorder", fg V.red),
        ("normalNote", fg V.white),                      -- 当前看板中的普通笔记
        ("inactiveNote", fg V.brightBlack),               -- 非当前看板中的笔记
        ("completedTask", fg V.brightBlack `V.withStyle` V.strikethrough)  -- 已完成任务使用灰色和删除线
      ]
    markerAttrs = [(attrName (markerName style), markerColor style) | style <- markerStyles]

-- 在 makeLenses ''AppState 前添加以下函数和类型类实例
