{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE QuasiQuotes                 #-}
{-# LANGUAGE ScopedTypeVariables         #-}
{-# LANGUAGE ViewPatterns                #-}

{-# OPTIONS_GHC -Wall -Werror #-}

-- | source: https://github.com/google/caja/
module Data.Markdown.HtmlWhiteLists
    ( html5Element
    , html5Attribute
    , css3Property

      -- (more for testing, really)
    , Html5Elements(..), html5Elements
    , Html5Attributes(..), html5Attributes
    , Css3Properties(..), css3Properties
    )
where

import Control.Applicative ((<|>))
import Control.Monad ((>=>))
import Data.Aeson
import Data.Aeson.QQ (aesonQQ)
import Data.Aeson.Types
import Data.String.Conversions
import Data.Typeable

import qualified Data.Text as ST
import qualified Data.Vector as Vector


unsafeFromJSON :: (FromJSON a, Typeable a) => Value -> a
unsafeFromJSON v = case fromJSON v of
    Success s -> s
    Error   e -> error $ "usafeFromJSON: " <> show e

newtype Html5Elements = Html5Elements [ST]
  deriving (Eq, Ord, Show, Read)

instance FromJSON Html5Elements where
    parseJSON v = do
        els0 :: Value   <- withObject "html5 elem whitelist" (.: "allowed") v
        els1 :: [Value] <- withArray "element list" (pure . Vector.toList) els0
        Html5Elements <$> (withText "element" (pure . cs) `mapM` els1)


-- | every attribute consists of a maybe-element and an attribute.  if the element is nothing, the
-- attribute is allowed in all elements.
newtype Html5Attributes = Html5Attributes [(Maybe ST, ST)]
  deriving (Eq, Ord, Show, Read)

instance FromJSON Html5Attributes where
    parseJSON v = do
        els0 :: Value   <- withObject "html5 elem whitelist" (.: "allowed") v
        els1 :: [Value] <- withArray "element list" (pure . Vector.toList) els0

        let parseAttr, o, p :: Value -> Parser (Maybe ST, ST)
            parseAttr s = o s <|> p s

            o = withObject "key with comment" (.: "key") >=> p
            p = withText "attr" $ \case (ST.splitOn "::" -> [el, attr]) -> pure (f el, attr)
                                        bad -> fail ("no element constraint: " <> show bad)
            f "*" = Nothing
            f el  = Just el

        Html5Attributes <$> parseAttr `mapM` els1

newtype Css3Properties = Css3Properties [ST]
  deriving (Eq, Ord, Show, Read)

instance FromJSON Css3Properties where
    parseJSON v = do
        els0 :: Value   <- withObject "html5 elem whitelist" (.: "allowed") v
        els1 :: [Value] <- withArray "element list" (pure . Vector.toList) els0
        Css3Properties <$> (withText "element" (pure . cs) `mapM` els1)


html5Element :: ST -> Bool
html5Element el = case html5Elements of
    Html5Elements els -> el `elem` els

html5Attribute :: ST -> ST -> Bool
html5Attribute el attr = case html5Attributes of
    Html5Attributes attrs -> any (`elem` attrs) [(Nothing, attr), (Just el, attr)]

css3Property :: ST -> Bool
css3Property prop = case css3Properties of
    Css3Properties props -> prop `elem` props


-- /src/com/google/caja/lang/html/html5-elements-whitelist.json
html5Elements :: Html5Elements
html5Elements = unsafeFromJSON [aesonQQ|
{
  "allowed": [
    "ARTICLE",
    "ASIDE",
    "AUDIO",
    "BDI",
    "CANVAS",
    "COMMAND",
    "DATA",
    "DATALIST",
    "DETAILS",
    "FIGCAPTION",
    "FIGURE",
    "FOOTER",
    "HEADER",
    "HGROUP",
    "MARK",
    "METER",
    "NAV",
    "NOBR",
    "OUTPUT",
    "PROGRESS",
    "SECTION",
    "SOURCE",
    "SUMMARY",
    "TIME",
    "TRACK",
    "VIDEO",
    "WBR"
  ],
  
  "denied": [
    { "key": "DIALOG",
      "reason": "can be displayed as a full-page modal dialog; needs further review" },
    { "key": "KEYGEN",
      "reason": "needs further review" },
    { "key": "NOEMBED",
      "reason": "rawtext elements are hazardous and should be avoided" }
  ]
}
|]

-- /src/com/google/caja/lang/html/html5-attributes-whitelist.json
html5Attributes :: Html5Attributes
html5Attributes = unsafeFromJSON [aesonQQ|
{
  "allowed": [
      { "key": "FORM::AUTOCOMPLETE",
        "comment": "forced to off by value criterion" },
      { "key": "INPUT::AUTOCOMPLETE",
        "comment": "forced to off by value criterion" },
      { "key": "SELECT::AUTOCOMPLETE",
        "comment": "forced to off by value criterion" },
      { "key": "TEXTAREA::AUTOCOMPLETE",
        "comment": "forced to off by value criterion" },
      "COMMAND::CHECKED",
      "COMMAND::COMMAND",
      "AUDIO::CONTROLS",
      "VIDEO::CONTROLS",
      "TRACK::DEFAULT",
      "INPUT::DIRNAME",
      "COMMAND::DISABLED",
      "FIELDSET::DISABLED",
      "KEYGEN::DISABLED",
      { "key": "*::DRAGGABLE",
        "comment": "TODO(kpreid): write matching script interfaces" },
      "OUTPUT::FOR",
      "CANVAS::HEIGHT",
      "VIDEO::HEIGHT",
      "*::HIDDEN",
      "METER::HIGH",
      "COMMAND::ICON",
      "*::INERT",
      "INPUT::INPUTMODE",
      "TEXTAREA::INPUTMODE",
      "*::ITEMPROP",
      "*::ITEMREF",
      "*::ITEMSCOPE",
      "KEYGEN::KEYTYPE",
      "TRACK::KIND",
      "COMMAND::LABEL",
      "MENU::LABEL",
      "TRACK::LABEL",
      "INPUT::LIST",
      "AUDIO::LOOP",
      "VIDEO::LOOP",
      "METER::LOW",
      "INPUT::MAX",
      "METER::MAX",
      "PROGRESS::MAX",
      "AUDIO::MEDIAGROUP",
      "VIDEO::MEDIAGROUP",
      "INPUT::MIN",
      "METER::MIN",
      "PROGRESS::MIN",
      "INPUT::MULTIPLE",
      "AUDIO::MUTED",
      "VIDEO::MUTED",
      "KEYGEN::NAME",
      "OUTPUT::NAME",
      "FORM::NOVALIDATE",
      "*::ONBLUR",
      "*::ONCHANGE",
      "*::ONERROR",
      "*::ONFOCUS",
      "*::ONLOAD",
      "*::ONRESET",
      "*::ONSELECT",
      "*::ONSUBMIT",
      "*::ONUNLOAD",
      "DETAILS::OPEN",
      "METER::OPTIMUM",
      "INPUT::PATTERN",
      "INPUT::PLACEHOLDER",
      "TEXTAREA::PLACEHOLDER",
      "VIDEO::POSTER",
      "AUDIO::PRELOAD",
      "VIDEO::PRELOAD",
      "COMMAND::RADIOGROUP",
      "INPUT::REQUIRED",
      "SELECT::REQUIRED",
      "TEXTAREA::REQUIRED",
      "OL::REVERSED",
      "*::SPELLCHECK",
      "LINK::SIZES",
      "AUDIO::SRC",
      "VIDEO::SRC",
      "TRACK::SRCLANG",
      "INPUT::STEP",
      "*::TRANSLATE",
      "COMMAND::TYPE",
      "SOURCE::TYPE",
      "MENU::TYPE",
      { "key": "OBJECT::TYPEMUSTMATCH",
        "comment": "TODO(kpreid): Can we make use of this attribute?" },
      "DATA::VALUE",
      "METER::VALUE",
      "PROGRESS::VALUE",
      "CANVAS::WIDTH",
      "VIDEO::WIDTH",
      "TEXTAREA::WRAP"
      ],

  "denied": [
      { "key": "SCRIPT::ASYNC",
        "comment": "TODO(kpreid): further review" },
      { "key": "BUTTON::AUTOFOCUS",
        "comment": "allows stealing focus from host page" },
      { "key": "INPUT::AUTOFOCUS",
        "comment": "allows stealing focus from host page" },
      { "key": "KEYGEN::AUTOFOCUS",
        "comment": "allows stealing focus from host page" },
      { "key": "SELECT::AUTOFOCUS",
        "comment": "allows stealing focus from host page" },
      { "key": "TEXTAREA::AUTOFOCUS",
        "comment": "allows stealing focus from host page" },
      { "key": "BUTTON::AUTOFOCUS",
        "comment": "allows stealing focus from host page" },
      { "key": "AUDIO::AUTOPLAY",
        "comment": "audio can't be isolated; safe to enable if wanted" },
      { "key": "VIDEO::AUTOPLAY",
        "comment": "audio can't be isolated; safe to enable if wanted" },
      { "key": "KEYGEN::CHALLENGE",
        "comment": "TODO(kpreid): further review" },
      { "key": "*::CONTENTEDITABLE",
        "comment": "TODO(kpreid): further review" },
      { "key": "*::CONTEXTMENU",
        "comment": "TODO(kpreid): further review" },
      { "key": "AUDIO::CROSSORIGIN",
        "comment": [
            "TODO(kpreid): further review",
            "(how does this interact with our policy?)"
          ] },
      { "key": "IMG::CROSSORIGIN",
        "comment": [
            "TODO(kpreid): further review",
            "(how does this interact with our policy?)"
          ] },
      { "key": "VIDEO::CROSSORIGIN",
        "comment": [
            "TODO(kpreid): further review",
            "(how does this interact with our policy?)"
          ] },
      { "key": "INPUT::DIRNAME",
        "comment": [
            "TODO(kpreid): further review",
            "(is adding new form submit pairs OK?)"
          ] },
      { "key": "TEXTAREA::DIRNAME",
        "comment": [
            "TODO(kpreid): further review",
            "(is adding new form submit pairs OK?)"
          ] },
      { "key": "A::DOWNLOAD",
        "comment": "TODO(kpreid): further review" },
      { "key": "AREA::DOWNLOAD",
        "comment": "TODO(kpreid): further review" },
      { "key": "*::DROPZONE",
        "comment": [
            "TODO(kpreid): further review",
            "(enables new information sources)"
          ] },
      { "key": "BUTTON::FORM",
        "comment": [
            "TODO(kpreid): further review",
            "(breaks containment assumption, if we care)"
          ] },
      { "key": "FIELDSET::FORM",
        "comment": [
            "TODO(kpreid): further review",
            "(breaks containment assumption, if we care)"
          ] },
      { "key": "INPUT::FORM",
        "comment": [
            "TODO(kpreid): further review",
            "(breaks containment assumption, if we care)"
          ] },
      { "key": "KEYGEN::FORM",
        "comment": [
            "TODO(kpreid): further review",
            "(breaks containment assumption, if we care)"
          ] },
      { "key": "LABEL::FORM",
        "comment": [
            "TODO(kpreid): further review",
            "(breaks containment assumption, if we care)"
          ] },
      { "key": "OBJECT::FORM",
        "comment": [
            "TODO(kpreid): further review",
            "(breaks containment assumption, if we care)"
          ] },
      { "key": "OUTPUT::FORM",
        "comment": [
            "TODO(kpreid): further review",
            "(breaks containment assumption, if we care)"
          ] },
      { "key": "SELECT::FORM",
        "comment": [
            "TODO(kpreid): further review",
            "(breaks containment assumption, if we care)"
          ] },
      { "key": "TEXTAREA::FORM",
        "comment": [
            "TODO(kpreid): further review",
            "(breaks containment assumption, if we care)"
          ] },
      { "key": "BUTTON::FORMACTION",
        "comment": "TODO(kpreid): further review" },
      { "key": "INPUT::FORMACTION",
        "comment": "TODO(kpreid): further review" },
      { "key": "BUTTON::FORMENCTYPE",
        "comment": "TODO(kpreid): further review" },
      { "key": "INPUT::FORMENCTYPE",
        "comment": "TODO(kpreid): further review" },
      { "key": "BUTTON::FORMMETHOD",
        "comment": "TODO(kpreid): further review" },
      { "key": "INPUT::FORMMETHOD",
        "comment": "TODO(kpreid): further review" },
      { "key": "BUTTON::FORMNOVALIDATE",
        "comment": "TODO(kpreid): further review" },
      { "key": "INPUT::FORMNOVALIDATE",
        "comment": "TODO(kpreid): further review" },
      { "key": "BUTTON::FORMTARGET",
        "comment": "TODO(kpreid): further review" },
      { "key": "INPUT::FORMTARGET",
        "comment": "TODO(kpreid): further review" },
      { "key": "*::ITEMID",
        "comment": "TODO(kpreid): need to be non-rewritten URIs" },
      { "key": "*::ITEMTYPE",
        "comment": "TODO(kpreid): need to be non-rewritten URIs" },
      { "key": "HTML::MANIFEST",
        "comment": "TODO(kpreid): further review" },
      { "key": "A::MEDIA",
        "comment":
            "TODO(kpreid): Implement MEDIA_QUERY atype sanitization FOR THESE 3"
        },
      "AREA::MEDIA",
      "SOURCE::MEDIA",
      { "key": "FIELDSET::NAME",
        "comment": "TODO(kpreid): further review" },
      { "key": "DIALOG::OPEN",
        "comment":
          "TODO(kpreid): further review (can this steal page focus/pop out?)" },
      { "key": "A::PING",
        "comment": "TODO(kpreid): introduce multiple-URI type for this" },
      { "key": "AREA::PING",
        "comment": "TODO(kpreid): introduce multiple-URI type for this" },
      { "key": "AREA::REL",
        "reason": [
            "Can make an assertion about the entire page.",
            "TODO(kpreid): Allow filtering rels to include e.g. 'nofollow'"
          ] },
      { "key": "IFRAME::SANDBOX",
        "comment": "TODO(kpreid): further review" },
      { "key": "STYLE::SCOPED",
        "comment":
          "TODO(kpreid): Stop migrating style elements so that this works" },
      { "key": "IFRAME::SEAMLESS",
        "comment": "TODO(kpreid): further review" },
      { "key": "IFRAME::SRCDOC",
        "comment": "TODO(kpreid): Implement HTML atype sanitization" }
      ]
}
|]

-- /src/com/google/caja/lang/css/css3-whitelist.json
css3Properties :: Css3Properties
css3Properties = unsafeFromJSON [aesonQQ|
{
  "description": "Set of CSS properties allowed.",

  "allowed": [
    "-moz-border-radius-bottomleft",
    "-moz-border-radius-bottomright",
    "-moz-border-radius-topleft",
    "-moz-border-radius-topright",
    "animation",
    "animation-delay",
    "animation-direction",
    "animation-duration",
    "animation-fill-mode",
    "animation-iteration-count",
    "animation-name",
    "animation-play-state",
    "animation-timing-function",
    "appearance",
    "azimuth",
    "backface-visibility",
    "background",
    "background-attachment",
    "background-color",
    "background-image",
    "background-position",
    "background-repeat",
    "background-size",
    "border",
    "border-bottom",
    "border-bottom-color",
    "border-bottom-left-radius",
    "border-bottom-right-radius",
    "border-bottom-style",
    "border-bottom-width",
    "border-collapse",
    "border-color",
    "border-left",
    "border-left-color",
    "border-left-style",
    "border-left-width",
    "border-radius",
    "border-right",
    "border-right-color",
    "border-right-style",
    "border-right-width",
    "border-spacing",
    "border-style",
    "border-top",
    "border-top-color",
    "border-top-left-radius",
    "border-top-right-radius",
    "border-top-style",
    "border-top-width",
    "border-width",
    "bottom",
    "box",
    "box-shadow",
    "box-sizing",
    "caption-side",
    "clear",
    "clip",
    "color",
    "content",
    "cue",
    "cue-after",
    "cue-before",
    "cursor",
    "direction",
    "display",
    "display-extras",
    "display-inside",
    "display-outside",
    "elevation",
    "empty-cells",
    "filter",
    "float",
    "font",
    "font-family",
    "font-size",
    "font-stretch",
    "font-style",
    "font-variant",
    "font-weight",
    "height",
    "left",
    "letter-spacing",
    "line-height",
    "list-style",
    "list-style-image",
    "list-style-position",
    "list-style-type",
    "margin",
    "margin-bottom",
    "margin-left",
    "margin-right",
    "margin-top",
    "max-height",
    "max-width",
    "min-height",
    "min-width",
    "opacity",
    "outline",
    "outline-color",
    "outline-style",
    "outline-width",
    "overflow",
    "overflow-wrap",
    "overflow-x",
    "overflow-y",
    "padding",
    "padding-bottom",
    "padding-left",
    "padding-right",
    "padding-top",
    "page-break-after",
    "page-break-before",
    "page-break-inside",
    "pause",
    "pause-after",
    "pause-before",
    "perspective",
    "perspective-origin",
    "pitch",
    "pitch-range",
    "play-during",
    "position",
    "quotes",
    "resize",
    "richness",
    "right",
    "speak",
    "speak-header",
    "speak-numeral",
    "speak-punctuation",
    "speech-rate",
    "stress",
    "table-layout",
    "text-align",
    "text-decoration",
    "text-indent",
    "text-overflow",
    "text-shadow",
    "text-transform",
    "text-wrap",
    "top",
    "transform",
    "transform-origin",
    "transform-style",
    "transition",
    "transition-delay",
    "transition-duration",
    "transition-property",
    "transition-timing-function",
    "unicode-bidi",
    "vertical-align",
    "visibility",
    "voice-family",
    "volume",
    "white-space",
    "widows",
    "width",
    "word-break",
    "word-spacing",
    "word-wrap",
    "z-index",
    "zoom"
  ],

  "denied": [
    { "key": "orphans",
      "reason": "Allows manipulation of page outside clipping region" },

    { "key": "widows",
      "reason": "Allows manipulation of page outside clipping region" }
  ]
}
|]
