module Widgets(app) where


import           Brick.Focus                (focusRingCursor)
import           Brick.Forms                (formFocus)
import qualified Brick.Main                 as M
import           Brick.Types

import           Brick.Widgets.Border       (borderWithLabel, border, hBorderWithLabel, vBorder)
import           Brick.Widgets.Border.Style (unicode)
import           Brick.Widgets.Center       (center)
import           Brick.Widgets.Core
import qualified Brick.Widgets.Edit         as E (renderEditor, handleEditorEvent)
import           Data.List                  as L (elem, intersperse, map, length)
import           Data.Text                  as T hiding (center, null)
import           FileIO -- 添加这个导入
import qualified Graphics.Vty               as V
import           System.FilePath            (takeFileName)
import           Lens.Micro
import           Note
import           Types
import           Actions
import           Dialog
import           Help
import           AppState


-- 简化 currentFileKanbanInfo 函数，移除不必要的边界检查
currentFileKanbanInfo :: AppState e Name -> String
currentFileKanbanInfo st =
  let files = st ^. fileInfos
      fileIdx = st ^. notes . currentLocation . kanbanFileIndex

      -- 获取看板标题
      currentKanbanIdx = getCurrentKanbanIndex (st^.notes.currentLocation)
      kanbansList = st ^. notes . kanbanData . to _kanbans
      ktitle = T.unpack $ _kanbanTitle (kanbansList !! currentKanbanIdx)

      -- 如果文件数量超过1个，才显示文件名 + 看板名的格式
      displayText = if L.length files > 1
                    then
                      let fname = takeFileName $ (files !! fileIdx) ^. filePath
                      in fname ++ " - " ++ ktitle
                    else ktitle  -- 只有一个文件时只显示看板名
  in displayText

drawLayer :: AppState e Name -> Widget Name
drawLayer st = widget
  where widget  | st^.showDialog = noteDialog st
                | not (null (st^.(notes . noteData))) = viewport MainViewPort Vertical $ scrollableNoteWidget st
                | otherwise = welcomeWidget

welcomeWidget :: Widget Name
welcomeWidget = center (txt "Welcome to the Kanban App")

-- 在 appEvent 函数中添加处理 Ctrl+Del 的逻辑
appEvent :: AppState e Name -> BrickEvent Name e -> EventM Name (Next (AppState e Name))

appEvent st ev
  -- 看板管理界面的事件处理
  | st^.showKanbanManager = case ev of
      -- 帮助切换
      VtyEvent (V.EvKey (V.KFun 1) [])   -> M.continue $ st & showHelp %~ not
      VtyEvent e | st^.editingKanbanTitle ->
        case e of
          V.EvKey V.KEnter [] -> saveKanbanTitle st >>= M.continue
          _ -> handleEventLensed st kanbanEditor E.handleEditorEvent e >>= M.continue
      -- 导航事件
      VtyEvent (V.EvKey V.KUp []) | not (st^.editingKanbanTitle) -> kanbanManagerSelect st (-1)
      VtyEvent (V.EvKey V.KDown []) | not (st^.editingKanbanTitle) -> kanbanManagerSelect st 1
      VtyEvent (V.EvKey V.KPageUp [])   -> switchKanban st (-1)
      VtyEvent (V.EvKey V.KPageDown []) -> switchKanban st 1

      -- 编辑看板标题
      VtyEvent (V.EvKey V.KEnter [])
        | not (st^.editingKanbanTitle) -> M.continue $ st & editingKanbanTitle .~ True
                                              & kanbanEditor .~ initKanbanEditor (currentKanbanTitle st) KanbanTitle

      -- 看板位置交换
      VtyEvent (V.EvKey V.KUp [V.MCtrl])   -> swapKanbanUp st
      VtyEvent (V.EvKey V.KDown [V.MCtrl]) -> swapKanbanDown st

      -- 退出看板管理
      VtyEvent (V.EvKey (V.KFun 10) []) -> exitKanbanManager st  -- 修改这行，使用exitKanbanManager
      VtyEvent (V.EvKey V.KEsc [])      -> exitKanbanManager st

      -- 删除看板
      VtyEvent (V.EvKey V.KDel [])
        | not (st^.editingKanbanTitle) -> deleteKanbanAtIndex st (st^.kanbanManagerFocus)

      -- 默认情况
      _ -> M.continue st

  -- 对话框模式
  | st^.showDialog = case ev of
      -- 帮助切换
      VtyEvent (V.EvKey (V.KFun 1) [])   -> M.continue $ st & showHelp %~ not

      -- 对话框操作
      VtyEvent (V.EvKey V.KEnter [])              -> handleEnter st ev

      -- Tab导航
      VtyEvent (V.EvKey (V.KChar '\t') []) -> handleTab st ev
      VtyEvent (V.EvKey V.KBackTab [])     -> handleShiftTab st ev

      -- 保存操作
      VtyEvent (V.EvKey (V.KChar 's') [V.MCtrl]) -> onSave st ev >>= M.continue

      VtyEvent (V.EvKey V.KEsc []) ->
        case st^.dialogMode of
          -- ChoiceCreate模式下，简单关闭对话框
          ChoiceCreate -> M.continue $ st & showDialog .~ False
          -- 默认情况，不处理
          _ -> onSave st ev >>= M.continue

      -- 处理输入事件
      VtyEvent e -> handleTypingEvents st ev e

      -- 默认情况
      _ -> M.continue st

  | otherwise = case ev of
      -- 帮助和视图切换
      VtyEvent (V.EvKey (V.KFun 1) [])   -> M.continue $ st & showHelp %~ not
      VtyEvent (V.EvKey (V.KFun 8) [])   -> M.continue $ st & expandKanbanNotes %~ not
      VtyEvent (V.EvKey (V.KFun 9) [])   -> M.continue $ st & showKanbanList %~ not
      -- 在打开看板管理器的地方(例如F10按键)使用enterKanbanManager
      VtyEvent (V.EvKey (V.KFun 10) []) -> enterKanbanManager st  -- 而不是直接设置showKanbanManager

      -- 笔记操作
      VtyEvent (V.EvKey (V.KChar 'c') [V.MCtrl]) -> cloneNote st
      VtyEvent (V.EvKey (V.KChar 'n') [V.MCtrl]) -> M.continue $ st & showDialog .~ True & dialogMode .~ ChoiceCreate
      VtyEvent (V.EvKey V.KEnter [])              -> handleEnter st ev
      VtyEvent (V.EvKey (V.KChar 'z') [V.MCtrl]) -> undoLastDelete st
      VtyEvent (V.EvKey (V.KChar 'x') [V.MCtrl]) -> cutNote st
      VtyEvent (V.EvKey (V.KChar 'v') [V.MCtrl]) -> pasteNote st
      VtyEvent (V.EvKey (V.KChar 't') [V.MCtrl]) -> convertNoteToTodo st
      VtyEvent (V.EvKey V.KDel [])     -> remove st
      VtyEvent (V.EvKey (V.KChar 's') [V.MCtrl]) -> saveAndUpdateState st >>= M.continue

      -- 导航
      VtyEvent (V.EvKey V.KDown []) -> navigateGrid st V.KDown
      VtyEvent (V.EvKey V.KUp []) -> navigateGrid st V.KUp
      VtyEvent (V.EvKey V.KLeft []) -> navigateGrid st V.KLeft
      VtyEvent (V.EvKey V.KRight []) -> navigateGrid st V.KRight

      -- 笔记小窗滚动
      VtyEvent (V.EvKey (V.KChar 'j') []) -> scrollNoteContent st 1

      VtyEvent (V.EvKey (V.KChar 'k') []) -> scrollNoteContent st (-1)

      -- 切换展示模式
      VtyEvent (V.EvKey (V.KChar 'f') []) -> toggleNoteDisplayMode st

      -- 上下视口滚动
      VtyEvent (V.EvKey V.KUp [V.MCtrl]) -> M.vScrollBy (M.viewportScroll MainViewPort) (-1) >> M.continue st
      VtyEvent (V.EvKey V.KDown [V.MCtrl]) -> M.vScrollBy (M.viewportScroll MainViewPort) 1 >> M.continue st

      -- 交换笔记
      VtyEvent (V.EvKey V.KLeft [V.MCtrl]) -> swapNoteLeft st
      VtyEvent (V.EvKey V.KRight [V.MCtrl]) -> swapNoteRight st

      -- 切换看板
      VtyEvent (V.EvKey V.KPageUp [])   -> switchKanban st (-1)
      VtyEvent (V.EvKey V.KPageDown []) -> switchKanban st 1

      -- 保存并退出
      VtyEvent e | e == V.EvKey (V.KChar 'q') [V.MCtrl] || e == V.EvKey V.KEsc [] -> saveAndExit st

      -- 默认情况
      _ -> M.continue st

-- 添加渲染看板列表的函数
kanbanListWidget :: AppState e Name -> Widget Name
kanbanListWidget st =
  if not (st^.showKanbanList)
    then emptyWidget
    else borderWithLabel (str "Kanbans") $
         hLimitPercent 20 $
         vLimitPercent 100 $
         padRight Max $
         viewport KanbanListViewPort Vertical $
         vBox (renderKanbanList st)

-- 修改 renderKanbanList 函数以支持显示看板内的笔记
renderKanbanList :: AppState e Name -> [Widget Name]
renderKanbanList st =
  let kanbansList = st^.notes.kanbanData.kanbans
      currentKanbanIndex = st^.notes.currentKanban
   in Prelude.concatMap (renderKanbanWithNotes st currentKanbanIndex) (Prelude.zip [0..] kanbansList)

-- 渲染看板项及其下属笔记
renderKanbanWithNotes :: AppState e Name -> Int -> (Int, Kanban) -> [Widget Name]
renderKanbanWithNotes st currentIndex (index, kanban) =
  let title = T.unpack $ _kanbanTitle kanban
      attr = if index == currentIndex
             then withAttr "activeKanban"
             else withAttr "kanbanTitle"
      kanbanWidget = attr $ padRight Max $ str $ show (index + 1) ++ ". " ++ title

      -- 如果展开状态打开，则同时渲染看板内的笔记
      noteWidgets = if st^.expandKanbanNotes
                    then Prelude.zipWith (curry (renderNoteInKanbanList index currentIndex)) [0..] (_childs kanban)
                    else []
   in kanbanWidget : noteWidgets

-- 渲染看板列表内的单个笔记
renderNoteInKanbanList :: Int -> Int -> (Int, Note) -> Widget Name
renderNoteInKanbanList kanbanIndex currentKanbanIndex (noteIndex, note) =
  let title = T.unpack $ _title note
      -- 根据笔记类型选择前缀符号
      prefix = case _mode note of
                 TodoList -> "- " -- TodoList 使用短横线
                 FreeNote -> "> " -- FreeNote 使用大于号
                 _ -> "* "        -- 其他类型使用星号

      -- 判断是否为当前看板中的笔记
      attr = if kanbanIndex == currentKanbanIndex
             then if _selected note
                  then withAttr "activeNote"     -- 当前选中的笔记
                  else withAttr "normalNote"     -- 当前看板中的普通笔记
             else withAttr "inactiveNote"        -- 非当前看板中的笔记
      visibilityControl = if kanbanIndex == currentKanbanIndex && _selected note
                          then visible
                          else id
  in padLeft (Pad 1) $ -- 缩进显示
      -- 缩进显示
     visibilityControl $ attr $ padRight Max $ str $ prefix ++ title


drawUi :: AppState e Name ->  [Widget Name]
drawUi st
  | st^.showKanbanManager = [drawKanbanManager st]
  | otherwise =
  [ withBorderStyle unicode $
    hLimitPercent 100 $
    vLimitPercent 100 $
    vBox [
          hBox [
             kanbanListWidget st,
             borderWithLabel (str $ currentFileKanbanInfo st) 
               (if st^.showHelp
                 then center $ helpWidget st
                 else drawLayer st)
           ]
         , statusBar st  -- 底部状态栏可保持原样
         ]
  ]
-- 绘制看板管理界面
drawKanbanManager :: AppState e Name -> Widget Name
drawKanbanManager st =
  withBorderStyle unicode $
  hLimitPercent 100 $
  vLimitPercent 100 $
  vBox [
    borderWithLabel (str "Kanban Manager") 
    (if st^.showHelp
     then center $ helpWidget st  -- 显示上下文相关帮助
     else viewport KanbanViewPort Vertical $
          vBox (renderKanbanManagerList st)),
    statusBar st  -- 底部状态栏显示剪切板信息
  ]

-- 渲染看板管理列表
renderKanbanManagerList :: AppState e Name -> [Widget Name]
renderKanbanManagerList st =
  let kanbans = _kanbans (st^.notes.kanbanData)
      focusedIdx = st^.kanbanManagerFocus
      isEditing = st^.editingKanbanTitle
  in Prelude.zipWith (renderKanbanItem st focusedIdx isEditing) [0..] kanbans

-- 渲染单个看板项
renderKanbanItem :: AppState e Name -> Int -> Bool -> Int -> Kanban -> Widget Name
renderKanbanItem st focusedIdx isEditing idx kanban =
  let isFocused = idx == focusedIdx
      title = T.unpack $ _kanbanTitle kanban
      noteCount = L.length (_childs kanban)

      -- 基础样式
      attr = if isFocused
             then withAttr "activeKanban"
             else withAttr "kanbanTitle"

      -- 确定看板所在的文件
      (fileIdx, _) = globalToLocalIndex idx (st^.notes.currentLocation.kanbanLengthList)
      files = st^.fileInfos

      -- 构建显示文本
      displayPrefix =
        if L.length files > 1
        then takeFileName ((files !! fileIdx)^.filePath) ++ " - "
        else ""

      -- 内容显示
      contentWidget =
        if isFocused && isEditing
          then -- 编辑状态显示编辑器
               hLimit 40 $ -- 限制编辑器宽度
                -- 限制编辑器宽度
               vLimit 1 $  -- 限制为单行
                 -- 限制为单行
               visible $
               E.renderEditor (txt . T.unlines) True (st^.kanbanEditor)
          else -- 非编辑状态显示带样式文本及文件信息
               (if isFocused then visible else id) $
               attr $ padRight Max $
               str $ show (idx + 1) ++ ". " ++ displayPrefix ++ title ++
                     " (" ++ show noteCount ++ " notes)"
  in padLeft (Pad 1) contentWidget

statusBar :: AppState e Name -> Widget Name
statusBar st =
  case st^.clipboard of
    [] -> emptyWidget  -- 剪贴板为空时不显示
    notes ->
      withAttr "clipboardInfo" $
      vLimit 4 $
      vBox [
        -- 左右两边添加垂直边框，上部无边框
        hBox [
          vBorder,  -- 左侧垂直边框
          center $ padLeftRight 1 $  -- 让内容居中并添加左右边距
              -- 让内容居中并添加左右边距
            hBox $ L.intersperse (str " ") $ L.map renderClipboardNote notes,
          vBorder   -- 右侧垂直边框
        ],

        -- 底部带标签的边框
        hBorderWithLabel (withAttr "clipboardHeader" $ str "Clipboard")
      ]
  where
    -- 每个笔记项用边框包围
    renderClipboardNote note =
          let titleText = _title note
              -- 控制每个单独项的宽度
              displayText = T.unpack titleText
          in withAttr "clipboardItem" $
             hLimit 20 $ -- 限制每个单独项的宽度
              -- 限制每个单独项的宽度
             border $    -- 使用标准的边框组件
                 -- 使用标准的边框组件
             padLeftRight 1 $ -- 内部填充，使文本不紧贴边框
             str displayText

appCursor :: AppState e Name -> [CursorLocation Name] -> Maybe (CursorLocation Name)
appCursor st  | st^.dialogMode `L.elem` [NoteCreate,NoteEdit] = focusRingCursor formFocus (st^.form)
              | st^.dialogMode `L.elem` [TodoCreate,TodoEdit] = focusRingCursor (^.notes.focusEdit) st
              | otherwise = M.showFirstCursor st
app :: M.App (AppState e Name) e Name
app =
    M.App { M.appDraw = drawUi
          , M.appStartEvent = return  -- 简单返回原始状态，首次导航时会计算布局
          , M.appHandleEvent = appEvent
          , M.appAttrMap = const theMap
          , M.appChooseCursor = appCursor
          }
