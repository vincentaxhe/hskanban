module Help (
    helpWidget,
    mainHelpText,
    kanbanManagerHelpText,
    dialogHelpText,
    kanbanEditingHelpText,
    noteEditHelpText,  -- 导出新增的帮助文本
    todoEditHelpText   -- 导出新增的帮助文本
) where

import Brick.Types
import Brick.Widgets.Border (borderWithLabel)
import Brick.Widgets.Core
import Data.List as L (map)
import Types
import Lens.Micro ((^.))

-- 主界面模式的帮助文本
mainHelpText :: [String]
mainHelpText = [ 
  "F1          : Show/Hide Help",
  "F8          : Expand/Collapse Notes in Kanban List",
  "F9          : Show/Hide Kanban List", 
  "F10         : Show Kanban Manager",
  "Arrows      : Select Note",
  "C-Up/Down   : Scroll",
  "Enter       : Edit Selected",
  "C-Left/Right: Swap Note",
  "Space       : Toggle Checkbox",
  "PgUp/PgDn   : Switch Kanban", 
  "j/k         : Scroll note window",
  "f           : Toggle full display",
  "Del         : Delete Note",
  "C-n         : Open Create Dialog",
  "C-s         : Save All Kanbans", 
  "C-c         : Clone selected note",
  "C-x         : Cut selected note",
  "C-v         : Paste note",
  "C-z         : Undo last delete",
  "C-t         : Convert to Todo Note",
  "C-q/Esc     : Save State and Exit"
  ]

-- 看板管理界面模式的帮助文本
kanbanManagerHelpText :: [String]
kanbanManagerHelpText = 
  [ "F1          : Show/Hide Help"
  , "F10/Esc     : Exit Manager"
  , "Up/Down     : Select Kanban"
  , "Enter       : Edit/Confirm Title"
  , "C-Up/Down   : Move Kanban"
  , "Del         : Delete Kanban"
  , "PageUp/Down : Switch Kanban"
  ]

-- 对话框模式的帮助文本
dialogHelpText :: [String]
dialogHelpText = [
  "Tab         : Move Between Fields",
  "Shift+Tab   : Move to Previous Field",
  "Enter       : Confirm",
  "Esc         : Cancel Dialog"
  ]

-- 笔记编辑模式帮助文本（修正名称首字母为小写）
noteEditHelpText :: [String]
noteEditHelpText = [
  "Tab         : Move Between Fields",
  "Shift+Tab   : Move to Previous Field",
  "C-s/Esc     : Save Changes"
  ]

-- 待办事项编辑模式帮助文本（修正名称首字母为小写）
todoEditHelpText :: [String]
todoEditHelpText = [
  "Tab         : Move Between Fields",
  "Enter       : Edit/Confirm Task",
  "C-u         : fold/unfold Task",
  "Shift+Tab   : Move to Previous Field",
  "C-s/Esc     : Save Changes",
  "Insert      : Insert Task Below",
  "Delete      : Delete Task",
  "C-Up/down   : Move Task up or down"
  ]

-- 看板标题编辑模式的帮助文本
kanbanEditingHelpText :: [String]
kanbanEditingHelpText = [
  "Enter       : Edit or Save Title",
  "Esc         : Cancel Editing"
  ]

-- 根据当前应用状态选择帮助文本
helpTextForState :: AppState e Name -> [String]
helpTextForState st
  | st^.showKanbanManager && st^.editingKanbanTitle = kanbanEditingHelpText
  | st^.showKanbanManager = kanbanManagerHelpText
  | st^.dialogMode == NoteCreate || st^.dialogMode == NoteEdit = noteEditHelpText  -- 笔记创建/编辑模式
  | st^.dialogMode == TodoCreate || st^.dialogMode == TodoEdit = todoEditHelpText  -- 待办事项创建/编辑模式
  | st^.showDialog = dialogHelpText  -- 其他对话框模式
  | otherwise = mainHelpText

-- 根据应用状态选择帮助标题
helpTitle :: AppState e Name -> String
helpTitle st 
  | st^.showKanbanManager && st^.editingKanbanTitle = "Kanban Title Editing Help"
  | st^.showKanbanManager = "Kanban Manager Help"
  | st^.dialogMode == NoteCreate = "Note Creation Help"  -- 笔记创建模式
  | st^.dialogMode == NoteEdit = "Note Editing Help"     -- 笔记编辑模式
  | st^.dialogMode == TodoCreate = "Todo Creation Help"  -- 待办事项创建模式
  | st^.dialogMode == TodoEdit = "Todo Editing Help"     -- 待办事项编辑模式  
  | st^.showDialog = "Dialog Mode Help"
  | otherwise = "Main Interface Help"

-- 根据应用状态绘制帮助窗口
helpWidget :: AppState e Name -> Widget Name
helpWidget st = 
  borderWithLabel (str (helpTitle st)) $
  hLimit 60 $      -- 宽度足够显示所有帮助文本
  vLimit 20 $      -- 高度足够显示所有可能的帮助条目
  padAll 1 $       -- 四周添加内边距
  vBox $ L.map str (helpTextForState st)