module Dialog where

import           Brick.Types
import           Brick.Widgets.Border
import           Brick.Widgets.Border.Style
import           Brick.Widgets.Center       as C (hCenter)
import           Brick.Widgets.Core
import           Brick.Widgets.Dialog       as D
import           Brick.Focus               (withFocusRing)
import qualified Brick.Widgets.Edit        as E (renderEditor)
import           Lens.Micro                 ((^.))
import           Task
import           Types
import           Form

getDialog :: D.Dialog Choice
getDialog = D.dialog (Just "Note") Nothing 45

noteDialog :: AppState e Name -> Widget Name
noteDialog st = padLeft (Pad 0) $ padRight Max $ padBottom Max dlgContent
    where nDialog = st^.dlg
          dlgContent  | st^.dialogMode  == ChoiceCreate = renderDialog nDialog $ choiceDialog st
                      | st^.dialogMode  `elem` [TodoCreate,TodoEdit] = renderDialog nDialog $ vBox $ 
                          [todoTitleWidget st,             -- 标题输入框
                           str "\n",                       -- 空行
                           newTaskWidget st,               -- 新任务输入框
                           str "\n"] ++                    -- 空行
                           [viewport TaskListVP Vertical $ vBox (getTasks st)] ++                  -- 任务列表
                           [todoHighlightWidget st]        -- 重要性复选框
                      | st^.dialogMode  `elem` [NoteCreate,NoteEdit] = renderDialog nDialog $ getForm $ st^.form
                      | st^.dialogMode  == KanbanCreate = renderDialog nDialog $ kanbanEditForm st
                      | otherwise = renderDialog nDialog $ str "Invalid Mode"

choiceDialog :: AppState e n1 -> Widget n2
choiceDialog st =  C.hCenter (
  nHighlightAttr (withBorderStyle unicode (border $ str "Note")) <+> 
  tHighlightAttr (withBorderStyle unicode (border $ str "Todo")) <+> 
  kHighlightAttr (withBorderStyle unicode (border $ str "Kanban")))
  where nHighlightAttr | st^.dialogSelect == NoteCreate = withAttr "highlightedNote"
                       | otherwise = withAttr "normalNote"
        tHighlightAttr | st^.dialogSelect == TodoCreate = withAttr "highlightedNote" 
                       | otherwise = withAttr "normalNote"
        kHighlightAttr | st^.dialogSelect == KanbanCreate = withAttr "highlightedNote"
                       | otherwise = withAttr "normalNote"

-- 创建看板标题输入小部件
kanbanTitleWidget :: AppState e Name -> Widget Name
kanbanTitleWidget st = 
  withBorderStyle unicode $
  borderWithLabel (str "Kanban Title") $
  padBottom (Pad 1) $
  vLimit 3 $
  hLimit 40 $
  withFocusRing (st^.kanbanFocus) (E.renderEditor (str . unlines)) (st^.kanbanTitleEditor)

-- 创建完整的看板编辑表单
kanbanEditForm :: AppState e Name -> Widget Name
kanbanEditForm st =
  vBox [
    kanbanTitleWidget st
  ]
