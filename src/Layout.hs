module Layout (
    createColumnLayout,
    GridPosition(..),
    calculateLayoutPure
) where

import Brick.Types
import Brick.Widgets.Core
import Data.List as L
import Lens.Micro
import Types

-- | 提供布局计算的共享配置
layoutConfig :: (Int, Int)
layoutConfig = (25, 50)  -- (最小宽度, 最大宽度)

-- | 创建列式布局，将笔记分布在多列中
createColumnLayout :: [Note] -> Int -> Int -> (Int -> Int -> Note -> Widget Name) -> Widget Name
createColumnLayout notes columns noteWidth noteRenderer =
    let
        totalNotes = L.length notes
        -- 计算每列笔记数量
        baseNotesPerCol = totalNotes `div` columns
        extraNotes = totalNotes `mod` columns
        
        -- 分配笔记到各列
        distributeNotes :: [Note] -> Int -> [[Note]]
        distributeNotes [] _ = []
        distributeNotes remainingNotes colIdx
            | colIdx >= columns = []
            | otherwise =
                let 
                    -- 当前列的笔记数 (前extraNotes列每列多一个笔记)
                    colSize = baseNotesPerCol + (if colIdx < extraNotes then 1 else 0)
                    -- 为当前列提取笔记
                    (colNotes, rest) = L.splitAt colSize remainingNotes
                in
                    colNotes : distributeNotes rest (colIdx + 1)
        
        -- 将笔记分配到各列
        noteColumns = distributeNotes notes 0
        
        -- 为每列创建Widget
        columnWidgets = zipWith createColumnWidget [0..] noteColumns
        
        -- 计算实际的笔记起始索引，考虑之前列的笔记数量
        getStartIndex colIdx = 
            sum $ map (\i -> baseNotesPerCol + (if i < extraNotes then 1 else 0)) [0..(colIdx-1)]
        
        createColumnWidget colIdx colNotes = 
            let startIdx = getStartIndex colIdx
            in vBox $ zipWith (\i n -> noteRenderer (startIdx + i) noteWidth n) 
                      [0..] colNotes
    in
        -- 水平排列各列，并添加分隔
        hBox $ L.intersperse (padLeft (Pad 2) emptyWidget) columnWidgets

-- | 计算基本布局 - 确定每行显示的笔记数量和每个笔记的宽度
calculateLayout :: Int -> Int -> Int -> Int -> (Int, Int)
calculateLayout width noteCount minWidth maxWidth =
    -- 确定每行显示的笔记数量和每个笔记的宽度
    let 
        -- 如果只有一个笔记或窗口太窄，则使用单列布局
        tryLayout n
            | n <= 1 || width < minWidth * 2 = (1, min maxWidth width)
            -- 计算平均宽度，减去列间间隔(每列间隔1个字符)
            | otherwise = 
                let availWidth = width - (n - 1)  -- 减去列间隔的宽度
                    avgWidth = availWidth `div` n
                in 
                    -- 如果平均宽度在限制范围内，接受这个布局
                    if avgWidth >= minWidth && avgWidth <= maxWidth
                       then (n, avgWidth)
                       -- 如果平均宽度小于最小宽度，减少每行笔记数
                       else if avgWidth < minWidth
                            then tryLayout (n - 1)
                            -- 如果平均宽度大于最大宽度，使用最大宽度但保持列数
                            else (n, maxWidth)
        
        -- 从可能的最大数量开始尝试
        maxPossible = min noteCount (width `div` minWidth)
    in
        -- 如果窗口太窄，将强制使用一个笔记并适应可用宽度
        if width < minWidth
           then (1, width)
           else tryLayout maxPossible

-- | 按列分配网格位置的辅助函数
buildColumnBasedGrid :: Int -> Int -> [(Int, GridPosition)]
buildColumnBasedGrid noteCount notesPerRow =
    -- 如果只有一列，简单地按顺序分配网格位置
    if notesPerRow <= 1 
    then [(i, GridPosition i 0) | i <- [0..noteCount-1]]
    else
        let
            -- 计算每列笔记数量
            baseNotesPerCol = noteCount `div` notesPerRow
            extraNotes = noteCount `mod` notesPerRow
            
            -- 逐列构建网格位置
            buildGridPositions :: Int -> Int -> [(Int, GridPosition)]
            buildGridPositions colIdx globalIdx
                | colIdx >= notesPerRow = []
                | otherwise =
                    let 
                        colSize = baseNotesPerCol + (if colIdx < extraNotes then 1 else 0)
                        thisColPositions = [(globalIdx + i, GridPosition i colIdx) 
                                          | i <- [0..colSize-1]]
                    in thisColPositions ++ buildGridPositions (colIdx+1) (globalIdx+colSize)
        in
            buildGridPositions 0 0

calculateLayoutPure :: AppState e Name -> Int -> (Int, Int, [(Int, GridPosition)], AppState e Name)
calculateLayoutPure s w = 
    let 
        -- 获取笔记数据和数量
        notesData = s^.(notes . noteData)
        noteCount = Prelude.length notesData
        
        -- 应用布局配置 (从 Layout 模块引入的常量)
        (minNoteWidth, maxNoteWidth) = layoutConfig  -- layoutConfig
        
        -- 计算每行的列数和宽度
        (notesPerRow, noteWidth) = calculateLayout w noteCount minNoteWidth maxNoteWidth
        
        -- 构建网格位置
        gridPositions = buildColumnBasedGrid noteCount notesPerRow
        
    in
        (notesPerRow, noteWidth, gridPositions, s)

