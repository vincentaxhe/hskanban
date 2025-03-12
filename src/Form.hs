module Form(getForm,emptyForm,setForm) where

import           Brick.Forms
import           Brick.Types
import           Brick.Widgets.Border (border, hBorder)
import           Brick.Widgets.Core
import           Types

getForm :: Form Note e Name -> Widget Name
getForm  =  border . padTop (Pad 1) . hLimit 50 . renderForm

setForm :: Maybe Note -> Form Note e Name
setForm Nothing  = emptyForm
setForm (Just n) = mkForm n

emptyForm :: Form Note e Name
emptyForm = mkForm Note{
  _title="",
  _tasks = [],
  _mode = FreeNote,
  _selectedTaskIndex = -1,
  _content="",
  _selected=False,
  _checkBoxSelected = False,
  _highlighted=False
}

-- 修改表单字段的创建方式，使用标准样式并确保初始焦点正确
mkForm :: Note -> Form Note e Name
mkForm note =
    let formField name field = 
          field <=> (if name == "title" then hBorder else emptyWidget)
        form = newForm [ formField "title" @@=
                  editTextField title TitleField (Just 1)
               , formField "content" @@=  
                  editTextField content ContentField Nothing
               , formField "highlighted" @@=
                  checkboxField highlighted HighlightField "Important?"  -- 使用标准复选框
               ] note
    in setFormFocus TitleField form  -- 设置初始焦点在标题字段



