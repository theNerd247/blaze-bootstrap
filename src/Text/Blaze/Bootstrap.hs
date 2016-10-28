{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Text.Blaze.Bootstrap where

import Control.Monad
#if MIN_VERSION_base(4,8,0)
#else
import Data.Monoid
#endif

import Text.Blaze.Html ((!),(!?))
import Text.Blaze.Html.Renderer.String
import qualified Data.Text as T
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

stylesheet :: H.AttributeValue -> H.Html
stylesheet path = H.link ! A.href path ! A.rel "stylesheet" ! A.type_ "text/css"

javascript :: H.AttributeValue -> H.Html
javascript path = H.script ! A.src path $ mempty

container :: H.Html -> H.Html
container x = H.div ! A.class_ "container" $ x

row :: H.Html -> H.Html
row x = H.div ! A.class_ "row" $ x

col :: String -- ^ The Bootstrap column size (e.g: xs-7)
  -> H.Html -- ^ The body HTML of the column
  -> H.Html
col s = H.div ! A.class_ (H.toValue $ "col-"++s)

data FillPadding = NoPadding | PrePadding | PostPadding

-- | Creates repeated columns of equal width to fill the given column width
fillCols :: 
  Int  -- ^ The column width to fill
  -> Int  -- ^ The number of columns to use
  -> FillPadding
  -> H.Html  -- ^ The html to fill each column with
  -> H.Html
fillCols w n pad = mconcat . makePadCols pad . replicate sz . (col $ "xs-" ++ (show sz))
  where
    (sz,rm) = w `quotRem` n
    makePadCols NoPadding = id
    makePadCols PrePadding = (paddingCols rm :)
    makePadCols PostPadding = (++ [paddingCols rm])

-- | Creates a padding column that contains a single space.
paddingCols :: Int -> H.Html
paddingCols n = col ("xs-" ++ (show n)) $ H.toHtml (" " :: String)

dataToggle :: H.AttributeValue -> H.Attribute
dataToggle = H.dataAttribute "toggle"

dataTarget :: H.AttributeValue -> H.Attribute
dataTarget = H.dataAttribute "target"

dataDismiss :: H.AttributeValue -> H.Attribute
dataDismiss = H.dataAttribute "dismiss"

ariaHidden :: Bool -> H.Attribute
ariaHidden bool =
    H.customAttribute  "aria-hidden" (if bool then "true" else "false")

role :: H.AttributeValue -> H.Attribute
role = H.customAttribute "role"

data BootAlertType
   = BootAlertDanger
   | BootAlertWarn
   | BootAlertInfo
   | BootAlertSuccess

alertBox :: BootAlertType -> H.Html -> H.Html
alertBox alertType alertVal =
    H.div ! A.class_ (H.toValue $ T.concat ["alert alert-dismissable ", t]) $
    do H.button ! A.type_ "button" ! A.class_ "close" ! dataDismiss "alert" ! ariaHidden True $ (H.unsafeByteString "&times;")
       alertVal
    where
      t =
          case alertType of
            BootAlertDanger -> "alert-danger"
            BootAlertWarn -> "alert-warning"
            BootAlertInfo -> "alert-info"
            BootAlertSuccess -> "alert-success"

mainNavigation :: H.AttributeValue
               -> H.Html -> [(H.AttributeValue, H.Html)] -> H.Html
mainNavigation indexPath pageTitle navPoints =
    H.nav ! A.class_ "navbar navbar-default navbar-fixed-top" $
     container $
      do H.div ! A.class_ "navbar-header page-scroll" $
          do H.button ! A.type_ "button" ! A.class_ "navbar-toggle" ! dataToggle "collapse" ! dataTarget "#main-nav" $
              do H.span ! A.class_ "sr-only" $ "Toggle navigation"
                 H.span ! A.class_ "icon-bar" $ mempty
                 H.span ! A.class_ "icon-bar" $ mempty
                 H.span ! A.class_ "icon-bar" $ mempty
             H.a ! A.class_ "navbar-brand" ! A.href indexPath $ pageTitle
         H.div ! A.class_ "collapse navbar-collapse" ! A.id "main-nav" $
           H.ul ! A.class_ "nav navbar-nav navbar-right" $
              forM_ navPoints $ \(url, val) ->
                H.li (H.a ! A.href url $ val)

formGroup :: H.Html -> H.Html
formGroup = H.form . (H.div ! A.class_ "form-group")

formInput :: (H.ToValue a, H.ToValue b) => a -> b -> H.Html
formInput tp name = H.input ! A.class_ "form-control" ! A.type_ (H.toValue tp) ! A.name (H.toValue name)

formSelect :: (Eq k, H.ToValue k, H.ToMarkup v)
           => T.Text -> H.AttributeValue -> [(k, v)] -> Maybe k -> H.Html
formSelect selLabel selName keyValues selectedV =
    do H.label ! A.for selName $ (H.toHtml selLabel)
       H.select ! A.name selName ! A.class_ "form-control" $
        forM_ keyValues $ \(k, v) ->
          H.option ! A.value (H.toValue k) !? (Just k == selectedV, A.selected "selected") $ H.toMarkup v

formSubmit :: H.Html -> H.Html
formSubmit buttonVal =
    H.button ! A.type_ "submit" ! A.class_ "btn btn-lg btn-success btn-block" $ buttonVal

tableResponsive :: H.Html -> H.Html -> H.Html
tableResponsive tblHead tblBody =
    H.div ! A.class_ "table-responsive" $
    H.table ! A.class_ "table table-striped table-bordered table-hover" $
          do H.thead tblHead
             H.tbody tblBody

-- | Creates a glyphicon. 
-- @glyphicon name ~ <span class="glyphicon glyphicon-name"></span>@ 
glyphicon :: String -> H.Html
glyphicon n = H.span ! A.class_ (H.toValue $ "glyphicon glyphicon-" ++ n) $ mempty

panel :: String -> H.Html -> H.Html
panel t = (H.div ! A.class_ (H.toValue $ "panel panel-"++t)) . (H.div ! A.class_ "panel-body")

panelDefault :: H.Html -> H.Html
panelDefault = panel "default"

panelPrimay :: H.Html -> H.Html
panelPrimay = panel "primary"

panelSuccess :: H.Html -> H.Html
panelSuccess = panel "success"

panelInfo :: H.Html -> H.Html
panelInfo = panel "info"

panelWarning :: H.Html -> H.Html
panelWarning = panel "warning"

panelDanger :: H.Html -> H.Html
panelDanger = panel "danger"

badge :: H.Html -> H.Html
badge = H.span ! A.class_ "badge"

addPopOver = addPopOverIndex "0"

addPopOverIndex :: 
  H.AttributeValue -- ^ the tabindex to use (this is for nested popovers)
  -> H.AttributeValue -- ^ Title string of popover
  -> H.Html -- ^ The html to make a popover
  -> H.Html -- ^ The content HTML of the popover
  -> H.Html
addPopOverIndex ind tle bdy popOverContent = H.a 
  ! H.customAttribute "tabIndex" ind
  ! role "button" 
  ! H.dataAttribute "toggle" "popover" 
  ! H.dataAttribute "trigger" "click" 
  ! A.title tle
  ! H.dataAttribute "placement" "auto" 
  ! H.dataAttribute "html" "true" 
  ! H.dataAttribute "content" (H.toValue $ renderHtml popOverContent)
  $ bdy

jumbotron :: H.Html -> H.Html -> H.Html
jumbotron title body = H.div ! A.class_ "jumbotron" $
  do H.h1 title
     H.p body

-- | Create an accordion of panels with pairs of titles and contents
accordion :: String -> [(H.Html,H.Html)] -> H.Html
accordion name panels = (H.div ! A.class_ "panel-group" ! ariaMultiSel "true" ! role "tablist" ! A.id (H.toValue aName)) $ snd $ foldr mkAccordion (0,mempty) panels
  where
    mkAccordion (title,body) (i,h) = ((,)(i+1)) . (h`mappend`) $  H.div ! A.class_ "panel panel-default" $
      do H.div ! A.class_ "panel-heading" ! A.id (headName i) ! role "tab" $ H.h2 ! A.class_ "panel-title" $ toggleLink i title
         H.div ! A.id (cName i) ! A.class_ "panel-collapse collapse" ! role "tabpanel" ! ariaLabel (headName i) $ 
           H.div ! A.class_ "panel-body" $ body
    toggleLink i = H.a ! A.href (H.toValue $ cLink i) ! dataToggle "collapse" ! dataParent (H.toValue $ "#" ++ aName) ! ariaControls (cName i)
    cLink = H.toValue . ("#collapse"++) . show
    cName = H.toValue . ("collapse"++) . show
    headName = H.toValue . ("heading"++) . show
    aName = "accordion" ++ name

ariaLabel :: H.AttributeValue -> H.Attribute
ariaLabel = H.customAttribute "aria-labelledby"

ariaMultiSel :: H.AttributeValue -> H.Attribute
ariaMultiSel = H.customAttribute "aria-multiselectable"

dataParent :: H.AttributeValue -> H.Attribute
dataParent = H.customAttribute "data-parent"

ariaControls :: H.AttributeValue -> H.Attribute
ariaControls = H.customAttribute "aria-controls"
