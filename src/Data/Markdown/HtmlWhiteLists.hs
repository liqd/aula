{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE QuasiQuotes                 #-}
{-# LANGUAGE ScopedTypeVariables         #-}
{-# LANGUAGE ViewPatterns                #-}

{-# OPTIONS_GHC -Wall -Werror #-}

-- | source: https://github.com/google/caja/
module Data.Markdown.HtmlWhiteLists
    ( HtmlElements(..), htmlElements
    , HtmlAttributes(..), htmlAttributes
    , Css3Properties(..), css3Properties
    )
where

import Control.Applicative ((<|>))
import Control.Monad ((>=>))
import Data.Aeson
import Data.Aeson.QQ (aesonQQ)
import Data.CaseInsensitive
import Data.String.Conversions
import Data.Typeable

import qualified Data.Text as ST
import qualified Data.Vector as Vector


unsafeFromJSON :: (FromJSON a, Typeable a) => Value -> a
unsafeFromJSON v = case fromJSON v of
    Success s -> s
    Error   e -> error $ "usafeFromJSON: " <> show e


newtype HtmlElements = HtmlElements [CI ST]
  deriving (Eq, Ord, Show, Read, Monoid)

instance FromJSON HtmlElements where
    parseJSON v = do
        els0 :: Value   <- withObject "html elem whitelist" (.: "allowed") v
        els1 :: [Value] <- withArray "element list" (pure . Vector.toList) els0
        HtmlElements <$> (withText "element" (pure . mk) `mapM` els1)


-- | every attribute consists of a maybe-element and an attribute.  if the element is nothing, the
-- attribute is allowed in all elements.
newtype HtmlAttributes = HtmlAttributes [(Maybe (CI ST), (CI ST))]
  deriving (Eq, Ord, Show, Read, Monoid)

instance FromJSON HtmlAttributes where
    parseJSON v = do
        els0 :: Value   <- withObject "html attribute whitelist" (.: "allowed") v
        els1 :: [Value] <- withArray "element list" (pure . Vector.toList) els0

        let parseAttr s = o s <|> p s

            o = withObject "key with comment" (.: "key") >=> p
            p = withText "attr" $ \case (ST.splitOn "::" -> [el, attr]) -> pure (mk <$> f el, mk attr)
                                        bad -> fail ("no element constraint: " <> show bad)
            f "*" = Nothing
            f el  = Just $ el

        HtmlAttributes <$> parseAttr `mapM` els1

newtype Css3Properties = Css3Properties [CI ST]
  deriving (Eq, Ord, Show, Read)

instance FromJSON Css3Properties where
    parseJSON v = do
        els0 :: Value   <- withObject "css elem whitelist" (.: "allowed") v
        els1 :: [Value] <- withArray "element list" (pure . Vector.toList) els0
        Css3Properties <$> (withText "element" (pure . mk) `mapM` els1)


htmlElements :: HtmlElements
htmlElements = html4Elements <> html5Elements

-- /src/com/google/caja/lang/html/html4-elements-whitelist.json
html4Elements :: HtmlElements
html4Elements = unsafeFromJSON [aesonQQ|
{
  "description": [
      "See http://code.google.com/p/google-caja/wiki/CajaWhitelists",
      "The denied is not necessary but lets us document why they're denied."
      ],

  "allowed": [
      "A",
      "ABBR",
      "ACRONYM",
      "ADDRESS",
      "AREA",
      "B",
      "BDO",
      "BIG",
      "BLOCKQUOTE",
      "BR",
      "BUTTON",
      "CAPTION",
      "CENTER",
      "CITE",
      "CODE",
      "COL",
      "COLGROUP",
      "DD",
      "DEL",
      "DFN",
      "DIR",
      "DIV",
      "DL",
      "DT",
      "EM",
      "FIELDSET",
      "FONT",
      "FORM",
      "H1",
      "H2",
      "H3",
      "H4",
      "H5",
      "H6",
      "HR",
      "I",
      "IFRAME",
      "IMG",
      "INPUT",
      "INS",
      "KBD",
      "LABEL",
      "LEGEND",
      "LI",
      "MAP",
      "MENU",
      "OL",
      "OPTGROUP",
      "OPTION",
      "P",
      "PRE",
      "Q",
      "S",
      "SAMP",
      "SELECT",
      "SMALL",
      "SPAN",
      "STRIKE",
      "STRONG",
      "SUB",
      "SUP",
      "TABLE",
      "TBODY",
      "TD",
      "TEXTAREA",
      "TFOOT",
      "TH",
      "THEAD",
      "TR",
      "TT",
      "U",
      "UL",
      "VAR"
      ],

  "denied": [
      { "key": "APPLET",
        "reason": "disallow because allows scripting" },
      { "key": "BASE",
        "reason":
            "affects global state and could be used to redirect requests" },
      { "key": "BASEFONT",
        "reason": "affects global state" },
      { "key": "BODY",
        "reason": "a global level tag" },
      { "key": "FRAME",
        "reason": "can be used to cause javascript execution" },
      { "key": "FRAMESET",
        "reason": "only useful with banned elements" },
      { "key": "HEAD",
        "reason": "a global level tag" },
      { "key": "HTML",
        "reason": "a global level tag" },
      { "key": "ISINDEX",
        "reason": "can be used to change page location" },
      { "key": "LINK",
        "reason": "can be used to load other javascript, e.g. on print" },
      { "key": "META",
        "reason": "can be used to cause page reloads" },
      { "key": "NOFRAMES",
        "reason": "useless since frames can't be used" },
      { "key": "NOSCRIPT",
        "reason": "useless since javascript must be loaded" },
      { "key": "OBJECT",
        "reason": "allows scripting" },
      { "key": "PARAM",
        "reason": "useless since applet and object banned" },
      { "key": "SCRIPT",
        "reason": "allows execution of arbitrary script" },
      { "key": "STYLE",
        "reason": "allows global definition of styles." },
      { "key": "TITLE",
        "reason": "a global level tag" }
      ]
}
|]

-- /src/com/google/caja/lang/html/html5-elements-whitelist.json
html5Elements :: HtmlElements
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

htmlAttributes :: HtmlAttributes
htmlAttributes = html4Attributes <> html5Attributes

-- /src/com/google/caja/lang/html/html4-attributes-whitelist.json
html4Attributes :: HtmlAttributes
html4Attributes = unsafeFromJSON [aesonQQ|
{
  "description":
      "A whitelist of allowed attributes by element and attribute name.",
  "allowed": [
      "TD::ABBR",
      "TH::ABBR",
      "FORM::ACCEPT",
      "INPUT::ACCEPT",
      "A::ACCESSKEY",
      "AREA::ACCESSKEY",
      "BUTTON::ACCESSKEY",
      "INPUT::ACCESSKEY",
      "LABEL::ACCESSKEY",
      "LEGEND::ACCESSKEY",
      "TEXTAREA::ACCESSKEY",
      "FORM::ACTION",
      "CAPTION::ALIGN",
      "IFRAME::ALIGN",
      "IMG::ALIGN",
      "INPUT::ALIGN",
      "LEGEND::ALIGN",
      "TABLE::ALIGN",
      "HR::ALIGN",
      "DIV::ALIGN",
      "H1::ALIGN",
      "H2::ALIGN",
      "H3::ALIGN",
      "H4::ALIGN",
      "H5::ALIGN",
      "H6::ALIGN",
      "P::ALIGN",
      "COL::ALIGN",
      "COLGROUP::ALIGN",
      "TBODY::ALIGN",
      "TD::ALIGN",
      "TFOOT::ALIGN",
      "TH::ALIGN",
      "THEAD::ALIGN",
      "TR::ALIGN",
      "BODY::ALINK",
      "AREA::ALT",
      "IMG::ALT",
      "INPUT::ALT",
      "TD::AXIS",
      "TH::AXIS",
      "BODY::BACKGROUND",
      "TABLE::BGCOLOR",
      "TR::BGCOLOR",
      "TD::BGCOLOR",
      "TH::BGCOLOR",
      "BODY::BGCOLOR",
      "TABLE::BORDER",
      "IMG::BORDER",
      "TABLE::CELLPADDING",
      "TABLE::CELLSPACING",
      "COL::CHAR",
      "COLGROUP::CHAR",
      "TBODY::CHAR",
      "TD::CHAR",
      "TFOOT::CHAR",
      "TH::CHAR",
      "THEAD::CHAR",
      "TR::CHAR",
      "COL::CHAROFF",
      "COLGROUP::CHAROFF",
      "TBODY::CHAROFF",
      "TD::CHAROFF",
      "TFOOT::CHAROFF",
      "TH::CHAROFF",
      "THEAD::CHAROFF",
      "TR::CHAROFF",
      "INPUT::CHECKED",
      "BLOCKQUOTE::CITE",
      "Q::CITE",
      "DEL::CITE",
      "INS::CITE",
      "*::CLASS",
      "BR::CLEAR",
      "FONT::COLOR",
      "TEXTAREA::COLS",
      "TD::COLSPAN",
      "TH::COLSPAN",
      "DIR::COMPACT",
      "DL::COMPACT",
      "MENU::COMPACT",
      "OL::COMPACT",
      "UL::COMPACT",
      "AREA::COORDS",
      "A::COORDS",
      "DEL::DATETIME",
      "INS::DATETIME",
      "*::DIR",
      "BDO::DIR",
      "BUTTON::DISABLED",
      "INPUT::DISABLED",
      "OPTGROUP::DISABLED",
      "OPTION::DISABLED",
      "SELECT::DISABLED",
      "TEXTAREA::DISABLED",
      "FORM::ENCTYPE",
      "FONT::FACE",
      "LABEL::FOR",
      "TABLE::FRAME",
      "IFRAME::FRAMEBORDER",
      "TD::HEADERS",
      "TH::HEADERS",
      "IFRAME::HEIGHT",
      "TD::HEIGHT",
      "TH::HEIGHT",
      "IMG::HEIGHT",
      "A::HREF",
      "AREA::HREF",
      "A::HREFLANG",
      "IMG::HSPACE",
      "*::ID",
      "IMG::ISMAP",
      "INPUT::ISMAP",
      "OPTION::LABEL",
      "OPTGROUP::LABEL",
      "*::LANG",
      "BODY::LINK",
      "IFRAME::MARGINHEIGHT",
      "IFRAME::MARGINWIDTH",
      "INPUT::MAXLENGTH",
      "FORM::METHOD",
      "SELECT::MULTIPLE",
      "BUTTON::NAME",
      "TEXTAREA::NAME",
      "SELECT::NAME",
      "FORM::NAME",
      "FRAME::NAME",
      "IMG::NAME",
      "A::NAME",
      "INPUT::NAME",
      "MAP::NAME",
      "AREA::NOHREF",
      "HR::NOSHADE",
      "TD::NOWRAP",
      "TH::NOWRAP",
      "A::ONBLUR",
      "AREA::ONBLUR",
      "BUTTON::ONBLUR",
      "INPUT::ONBLUR",
      "LABEL::ONBLUR",
      "SELECT::ONBLUR",
      "TEXTAREA::ONBLUR",
      "INPUT::ONCHANGE",
      "SELECT::ONCHANGE",
      "TEXTAREA::ONCHANGE",
      "*::ONCLICK",
      "*::ONDBLCLICK",
      "A::ONFOCUS",
      "AREA::ONFOCUS",
      "BUTTON::ONFOCUS",
      "INPUT::ONFOCUS",
      "LABEL::ONFOCUS",
      "SELECT::ONFOCUS",
      "TEXTAREA::ONFOCUS",
      "*::ONKEYDOWN",
      "*::ONKEYPRESS",
      "*::ONKEYUP",
      "BODY::ONLOAD",
      "*::ONMOUSEDOWN",
      "*::ONMOUSEMOVE",
      "*::ONMOUSEOUT",
      "*::ONMOUSEOVER",
      "*::ONMOUSEUP",
      "FORM::ONRESET",
      "*::ONSCROLL",
      "INPUT::ONSELECT",
      "TEXTAREA::ONSELECT",
      "FORM::ONSUBMIT",
      "BODY::ONUNLOAD",
      "TEXTAREA::READONLY",
      "INPUT::READONLY",
      "TEXTAREA::ROWS",
      "TD::ROWSPAN",
      "TH::ROWSPAN",
      "TABLE::RULES",
      "TD::SCOPE",
      "TH::SCOPE",
      "OPTION::SELECTED",
      "AREA::SHAPE",
      "A::SHAPE",
      "HR::SIZE",
      "FONT::SIZE",
      "INPUT::SIZE",
      "SELECT::SIZE",
      "COL::SPAN",
      "COLGROUP::SPAN",
      "INPUT::SRC",
      "IMG::SRC",
      "OL::START",
      "*::STYLE",
      "TABLE::SUMMARY",
      "*::TABINDEX",
      "A::TARGET",
      "AREA::TARGET",
      "FORM::TARGET",
      "BODY::TEXT",
      "*::TITLE",
      "A::TYPE",
      "INPUT::TYPE",
      "LI::TYPE",
      "OL::TYPE",
      "UL::TYPE",
      "BUTTON::TYPE",
      "IMG::USEMAP",
      "INPUT::USEMAP",
      "COL::VALIGN",
      "COLGROUP::VALIGN",
      "TBODY::VALIGN",
      "TD::VALIGN",
      "TFOOT::VALIGN",
      "TH::VALIGN",
      "THEAD::VALIGN",
      "TR::VALIGN",
      "INPUT::VALUE",
      "OPTION::VALUE",
      "BUTTON::VALUE",
      "LI::VALUE",
      "HTML::VERSION",
      "BODY::VLINK",
      "IMG::VSPACE",
      "COL::WIDTH",
      "COLGROUP::WIDTH",
      "HR::WIDTH",
      "IFRAME::WIDTH",
      "IMG::WIDTH",
      "PRE::WIDTH",
      "TABLE::WIDTH",
      "TD::WIDTH",
      "TH::WIDTH"
    ],
  "denied": [
      { "key": "FORM::ACCEPT-CHARSET",
        "reason": [
            "Per bug 585, this is an infrequently used and poorly",
            "understood attribute that could lead to mismatched encoding",
            "attacks.  Could be used to sneak content through a proxy in a",
            "wrong encoding?"
          ] },
      { "key": "A::CHARSET",
        "reason": [
            "Per bug 585:  Charset is disallowed since it allows overriding",
            "of Content-type headers.  A server might specify UTF-8 via the",
            "header Content-type:text/javascript;charset=UTF-8, but an",
            "embedding page might cause that file to be interpreted as UTF-7.",
            "According to http://www.w3schools.com/TAGS/att_a_charset.asp: ",
            "The charset attribute is not supported in any of the major browsers."
          ] },
      { "key": "A::REL",
        "reason": [
            "Can make an assertion about the entire page.",
            "TODO(kpreid): Allow filtering rels to include e.g. 'nofollow'"
          ] },
      { "key": "A::REV",
        "reason": [
            "Can make an assertion about the entire page.",
            "TODO(kpreid): Allow filtering rels to include e.g. 'nofollow'"
          ] },
      "LINK::CHARSET",
      "SCRIPT::CHARSET",
      { "key": "IMG::LONGDESC",
        "reason": "Not supported by any major browser" },
      { "key": "IFRAME::LONGDESC",
        "reason": "Not supported by any major browser" }
    ],
  "types": [
      { "key": "IFRAME::ID",
        "type": "ID", "optional": true,
        "reason": [
            "We allow a restricted set of attributes on IFRAMEs to allow them ",
            "to be used as shims to work around IE layout bugs.",
            "But we do not allow either NAME or ID since those are not ",
            "required for shims and affect publicly visible browser global ",
            "state like the frame graph."
        ] }
    ]
}
|]

-- /src/com/google/caja/lang/html/html5-attributes-whitelist.json
html5Attributes :: HtmlAttributes
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
