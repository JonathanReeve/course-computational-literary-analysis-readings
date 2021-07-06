{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Clay ((?), Css, em, pc, px, sym)
import qualified Clay as C
import Control.Monad
import Data.Aeson (FromJSON, fromJSON)
import qualified Data.Aeson as Aeson
import qualified Data.Text as T
import Data.Text (Text)
import Development.Shake
import GHC.Generics (Generic)
import Lucid
import Main.Utf8
import Rib (IsRoute, Pandoc)
import qualified Rib
import System.FilePath

import PandocSidenote (usingSideNotes)
import PandocUtils

-- | Route corresponding to each generated static page.
--
-- The `a` parameter specifies the data (typically Markdown document) used to
-- generate the final page text.
data Route a where
  Route_Index :: Route [(Route Pandoc, Pandoc)]
  Route_Article :: FilePath -> Route Pandoc

-- | The `IsRoute` instance allows us to determine the target .html path for
-- each route. This affects what `routeUrl` will return.
instance IsRoute Route where
  routeFile = \case
    Route_Index ->
      pure "index2.html"
    Route_Article srcPath ->
      pure $ srcPath -<.> ".html"

-- | Main entry point to our generator.
--
-- `Rib.run` handles CLI arguments, and takes three parameters here.
--
-- 1. Directory `content`, from which static files will be read.
-- 2. Directory `dest`, under which target files will be generated.
-- 3. Shake action to run.
--
-- In the shake action you would expect to use the utility functions
-- provided by Rib to do the actual generation of your static site.
main :: IO ()
main = withUtf8 $ do
  Rib.run "content" "dest" generateSite

-- | Shake action for generating the static site
generateSite :: Action ()
generateSite = do
  -- Copy over the static files
  Rib.buildStaticFiles ["static/**"]
  let writeHtmlRoute :: Route a -> a -> Action ()
      writeHtmlRoute r = Rib.writeRoute r . Lucid.renderText . renderPage r
  -- Build individual sources, generating .html for each.
  articles <-
    Rib.forEvery ["*.md", "texts/*.md"] $ \srcPath -> do
      let r = Route_Article srcPath
      doc <- PandocUtils.parse PandocUtils.readMarkdown srcPath
      writeHtmlRoute r doc
      pure (r, doc)
  writeHtmlRoute Route_Index articles

-- | Define your site HTML here
renderPage :: Route a -> a -> Html ()
renderPage route val = html_ [lang_ "en"] $ do
  head_ $ do
    meta_ [httpEquiv_ "Content-Type", content_ "text/html; charset=utf-8"]
    title_ routeTitle
    link_ [rel_ "stylesheet", href_ "https://cdnjs.cloudflare.com/ajax/libs/tufte-css/1.7.2/tufte.min.css"]
    style_ [type_ "text/css"] $ C.render pageStyle
  body_ $ do
    header_ [class_ "header"] $ do
      nav_ $ do
        a_ [href_ "/"] "Introduction to Computational Literary Analysis"
        " // "
        a_ [href_ "https://icla2021.zulipchat.com"] "Chat"
        " // "
        a_ [href_ "/texts/moonstone.html"] "The Moonstone"
        -- " // "
        -- a_ [id_ "thisweek", href_ "#"] "This Week"
    h1_ routeTitle
    case route of
      Route_Index -> do
        p_ mempty
      Route_Article _ -> do
        article_ $ do
          details_ [] $ do
            summary_ [] "Table of Contents"
            section_ [] (PandocUtils.getToC val)
          section_ [] $ PandocUtils.render $ usingSideNotes val
        footer_ $ do
          p_ $ do
            "This course material is released under "
            a_ [href_ "https://creativecommons.org/licenses/by-sa/2.0/"] "the CC-BY-SA license."
          script_ [src_ "https://hypothes.is/embed.js"] T.empty
 where
    routeTitle :: Html ()
    routeTitle = case route of
      Route_Index -> "Introduction to Computational Literary Analysis"
      Route_Article _ -> toHtml $ title $ getMeta val

renderMarkdown :: Text -> Html ()
renderMarkdown =
  PandocUtils.render . usingSideNotes . PandocUtils.parsePure PandocUtils.readMarkdown

-- | Define your site CSS here
pageStyle :: Css
pageStyle = C.body ? do
  -- C.margin (em 4) (pc 20) (em 1) (pc 20)
  "details > summary" ? C.fontSize (em 1.5)
  ".header" ? do
    C.marginBottom $ em 2
  "li.pages" ? do
    -- C.listStyleType C.none
    C.marginTop $ em 1
    "b" ? C.fontSize (em 1.2)
    "p" ? sym C.margin (px 0)

-- | Metadata in our markdown sources
data SrcMeta
  = SrcMeta
      { title :: Text,
        -- | Description is optional, hence `Maybe`
        description :: Maybe Text
      }
  deriving (Show, Eq, Generic, FromJSON)

-- | Get metadata from Markdown's YAML block
getMeta :: Pandoc -> SrcMeta

getMeta src = case PandocUtils.extractMeta src of
  Nothing -> error "No YAML metadata"
  Just (Left e) -> error $ T.unpack e
  Just (Right val) -> case fromJSON val of
    Aeson.Error e -> error $ "JSON error: " <> e
    Aeson.Success v -> v
