module Note(
    -- 导出Note相关
    Note(..),
    scrollableNoteWidget,
    noteWidgets,
    noteWithWidth,
    getFreeNote,
    emptyNote,
    getMode
) where
-- 修改导入部分，添加必要的Lens组件
import           Brick.Forms
import           Brick.Types
import           Brick.Widgets.Border       (borderWithLabel)
import           Brick.Widgets.Border.Style (unicode, unicodeBold)
import           Brick.Widgets.Core
import           Data.List                  as L (find)
import           Data.Maybe                 (catMaybes, isJust)
import           Data.Text                  as T hiding (map, unlines)
import           TextWrap                   (renderWrappedText)
import           Types
import           Lens.Micro                 (ix, non, (^.), (^?))  -- 添加 & 和 .~ 运算符
import           Brick.AttrMap              ( attrName )
import           KbUtils
import           Layout

noteWidgets :: Int -> AppState e Name -> (Widget Name, AppState e Name)
noteWidgets width st =
    -- 创建一个显式不依赖 EventM 上下文的纯布局
    let -- 使用纯函数直接计算布局
        (notesPerRow, noteWidth, _, updatedSt) = calculateLayoutPure st width

        -- 获取笔记数据
        notesData = updatedSt^.(notes . noteData)

        -- 创建多列布局 - 每列包含多个笔记，垂直排列
        widgetLayout = if notesPerRow <= 1
                      then vBox $ Prelude.zipWith (\i n -> noteWithWidth updatedSt i noteWidth n) [0..] notesData -- 单列布局
                      else createColumnLayout notesData notesPerRow noteWidth renderNote

        -- 构建最终组件
        finalWidget = padLeft (Pad 0) $ padRight Max $ padBottom Max $
                    withBorderStyle unicode widgetLayout

        -- 创建笔记渲染函数 - 用于传递给 createColumnLayout
        renderNote = noteWithWidth updatedSt
    in
        (finalWidget, updatedSt) -- 返回布局组件和更新后的状态

-- 然后修改 scrollableNoteWidget 函数来使用新的接口
scrollableNoteWidget :: AppState e Name -> Widget Name
scrollableNoteWidget s =
    Widget Fixed Fixed $ do
        ctx <- getContext
        let totalWidth = ctx^.availWidthL
            (widget, _) = noteWidgets totalWidth s
        render widget

combineAttrs :: [Widget Name -> Widget Name] -> (Widget Name -> Widget Name)
combineAttrs attrs w = Prelude.foldr ($) w attrs

-- 修改 calculateLayout 函数，扩展其功能

getFreeNote :: Form Note e Name -> AppState e Name -> Bool -> Note
getFreeNote f' st sel = Note{
                  _title = formState f'^.title,
                  _content = formState f'^.content,
                  _selected = sel,
                  _highlighted=formState f'^.highlighted,
                  _checkBoxSelected = False,
                  _tasks = [],
                  _selectedTaskIndex = -1,
                  _mode = FreeNote
                }

emptyNote :: Note
emptyNote = Note{
                  _title = "",
                  _selectedTaskIndex = -1,
                  _content = "",
                  _selected = False,
                  _checkBoxSelected = False,
                  _highlighted=False,
                  _mode = InvalidMode,
                  _tasks = []
            }

-- 明确使用 Types.Mode 类型
getMode :: AppState e Name -> Types.NoteMode  -- 改为 NoteMode 而不是 Mode
getMode st = (st^.(notes . noteData) ^? ix (st^.selectedIndex)) ^. (non emptyNote . mode)

-- 将 renderContentWithStyles 提取为顶层函数以便复用
renderNoteWithStyles :: Text -> Int -> Widget Name
renderNoteWithStyles content width =
    let ls = T.splitOn "\n" content
        renderLine line =
            let (attr, processedLine) = applyMarkerStyle defaultStyle line
            in attr $ renderWrappedText processedLine width

        -- 默认样式和辅助函数
        defaultStyle = withAttr "normalNoteText"

        applyMarkerStyle :: (Widget Name -> Widget Name) -> Text -> (Widget Name -> Widget Name, Text)
        applyMarkerStyle defaultStyle line = case T.uncons line of
            Just (c, rest) -> case findMarkerStyle c of
                Just ms -> (withAttr (attrName (markerName ms)), T.dropWhile (== ' ') rest)
                Nothing -> (defaultStyle, line)
            Nothing -> (defaultStyle, line)

        findMarkerStyle :: Char -> Maybe MarkerStyle
        findMarkerStyle c = L.find (\ms -> markerSymbol ms == c) markerStyles
    in vBox (map renderLine ls)

-- 重构后的 noteWithWidth 函数，精简核心逻辑
noteWithWidth :: AppState e Name -> Int -> Int -> Note -> Widget Name
noteWithWidth st index width n =
    let
        -- 获取当前显示模式
        dispMode = st^.(notes.displayMode)

        -- 根据显示模式选择是否应用高度限制
        heightLimitedWidget =
            withBorderStyle borderStyle $
            borderAttr $
            hLimit width $
            (if dispMode == ScrollDisplay then vLimit 10 else id)
            noteOrTodo
    in
        heightLimitedWidget
  where
        borderStyle
          | n^.selected = unicodeBold
          | otherwise   = unicode

        borderAttr = combineAttrs $ catMaybes
            [ if n^.selected then Just (withDefAttr "activeNoteBorder") else Nothing
            , if n^.highlighted then Just (withDefAttr "highlightedNote") else Nothing
            ]

        contentWidth = width - 2

        -- 根据笔记类型选择不同的渲染方式
        noteOrTodo
         | n^.mode == FreeNote =
            borderWithLabel (withAttr "noteTitle" $ txt $ n^.title)
              (drawFreeNoteContent st n index contentWidth)

         | n^.mode == TodoList =
            borderWithLabel (withAttr "noteTitle" $ txt $ n^.title)
              (drawTodoNoteContent st n index contentWidth)

         | otherwise = emptyWidget

-- 为任务列表创建专门的渲染函数
renderTodoWithStyles :: [Task] -> Int -> Widget Name
renderTodoWithStyles tasks _ =
    vBox $ map renderTask tasks
  where
    renderTask :: Task -> Widget Name
    renderTask task =
        let taskText = show task  -- 使用 Task 类型的 Show 实例，已经包含了复选框显示
            -- 判断任务是否完成（根据 _selectedTask 字段）
            taskWidget = if task^.selectedTask
                         then withAttr "completedTask" $ str taskText --不使用strWrap，因为会在标志处折行
                         else str taskText
        in taskWidget
-- 通用的笔记内容渲染函数，接收原始内容和渲染函数，同时增加 width 参数
drawNoteContentCommon :: AppState e Name -> Note -> Int -> Int -> (a -> Int -> Widget Name) -> a -> (a -> Bool) -> Widget Name
drawNoteContentCommon st n index width renderFunc contentData isEmptyCheck =
    let contentStyle = if n^.highlighted
                       then withAttr "highlightedNote"
                       else withAttr "normalNote"

        textStyle = combineAttrs $ catMaybes
            [ if n^.selected then Just (withAttr "selectedNoteText") else Nothing
            , if n^.highlighted then Just (withAttr "highlightedNoteText") else Nothing
            , Just (withAttr "normalNoteText")
            ]

        -- 使用视口实现滚动功能
        allNotes = st^.(notes.noteData)
        duplicateIdx = findDuplicateNoteIndex allNotes index

        -- 从应用状态获取显示模式
        disMode = st^.(notes.displayMode)

        -- 应用文本样式到内容（传入 width 参数）
        contentWidget = textStyle $ renderFunc contentData width

        -- 最终组合
        finalContentWidget =
            if isEmptyCheck contentData
                then padBottom (Pad 1) $ textStyle $ str " "  -- 显示一行空白
                else case disMode of
                        ScrollDisplay -> viewport viewportName Vertical contentWidget
                        FullDisplay -> contentWidget

        -- 视口名称根据笔记类型提供
        (baseViewportId, viewportName) =
            if n^.mode == FreeNote
                then let bvpId = generateViewportId n
                     in (bvpId, NoteContentViewPort (if isJust duplicateIdx then bvpId * 31 + index else bvpId))
                else let bvpId = generateTodoViewportId n index
                     in (bvpId, TodoListViewPort (if isJust duplicateIdx then bvpId * 31 + index else bvpId))
    in
        padTop (Pad 0) $ contentStyle finalContentWidget

-- 自由笔记内容渲染函数
drawFreeNoteContent :: AppState e Name -> Note -> Int -> Int -> Widget Name
drawFreeNoteContent st n index width =
    drawNoteContentCommon st n index width renderNoteWithStyles (n^.content) T.null

-- 待办列表笔记内容渲染函数  
drawTodoNoteContent :: AppState e Name -> Note -> Int -> Int -> Widget Name
drawTodoNoteContent st n index width =
    drawNoteContentCommon st n index width renderTodoWithStyles (n^.tasks) Prelude.null
