{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Example.QQ where

import Language.Haskell.TH (ExpQ, reify, pprint, runIO)
import Language.Haskell.TH.Syntax (Name, Exp(LitE))
import Language.Haskell.TH.Quote (QuasiQuoter(..))
import qualified Language.Haskell.Exts as Exts
-- For parsing the haskell strings to template-haskell AST
import Language.Haskell.Meta (parseExp, parseResultToEither, toExp)
import Language.Haskell.Meta.Utils (eitherQ)

import Data.Char (isUpper, isAlpha, isAlphaNum, isNumber)

import Language.Haskell.HsColour.Classify (tokenise)
import Language.Haskell.HsColour.CSS (renderToken)

import Control.Monad (void)

import Reflex.Dom.SemanticUI hiding (parseType)
import Data.Text (Text)
import qualified Data.Text as T

mymode = Exts.defaultParseMode
    { Exts.baseLanguage = Exts.Haskell2010
    , Exts.extensions = Exts.EnableExtension <$>
      [ Exts.ExistentialQuantification
      , Exts.TypeFamilies
      , Exts.ExplicitForAll
      , Exts.DataKinds
      , Exts.GADTs
      , Exts.MultiParamTypeClasses
      , Exts.RecordWildCards
      , Exts.RecursiveDo
      , Exts.ScopedTypeVariables
      ]
    }
-- | Pretty print the definition of a haskell type
printDefinition :: (String -> String) -> Name -> ExpQ
printDefinition preproc name = do
  info <- reify name
  let
    mode = Exts.defaultParseMode
      { Exts.baseLanguage = Exts.Haskell2010
      , Exts.extensions = Exts.EnableExtension <$>
        [Exts.ExistentialQuantification, Exts.TypeFamilies, Exts.ExplicitForAll, Exts.DataKinds, Exts.GADTs, Exts.MultiParamTypeClasses, Exts.RecordWildCards, Exts.RecursiveDo]
      }
    style' = Exts.style { Exts.lineLength = 600, Exts.ribbonsPerLine = 1 }
    parse = Exts.parseDeclWithMode mode . stripForAll . stripTypes . stripNumbers . stripModules . preproc . pprint
    prettyPrint = newlines . Exts.prettyPrintStyleMode style' Exts.defaultMode
    pp = prettyPrint . Exts.fromParseResult . parse $ info
  case parse info of
    Exts.ParseOk _ -> [|hscode pp|]
    Exts.ParseFailed loc str -> do
      runIO $ print loc
      runIO $ putStrLn $ stripModules $ pprint info
      fail str

hscode :: MonadWidget t m => String -> m ()
hscode = void . elAttr "code" ("class" =: "haskell")
       . elDynHtml' "pre" . constDyn . hscolour

hsCodeInline :: MonadWidget t m => String -> m ()
hsCodeInline = void . elAttr "code" ("class" =: "haskell inline")
       . elDynHtml' "pre" . constDyn . hscolour

hscodeInline :: MonadWidget t m => String -> m ()
hscodeInline = void . elAttr "code" ("class" =: "inline haskell")
             . elDynHtml' "pre" . constDyn . hscolour

hscolour :: String -> Text
hscolour = T.strip . T.pack . concatMap renderToken . tokenise . unindent

newlines :: String -> String
newlines "" = ""
newlines ('{':' ':rest) = "\n  { " ++ newlines rest
newlines ('{':rest) = "\n  { " ++ newlines rest
newlines ('}':rest) = "\n  }" ++ newlines rest
newlines (',':rest) = "\n  ," ++ newlines rest
newlines (x:rest) = x : newlines rest

stripForAll :: String -> String
stripForAll "" = ""
stripForAll ('f':'o':'r':'a':'l':'l':rest) = stripForAll $ drop 1 $ dropWhile (/= '.') rest
stripForAll (x:rest) = x : stripForAll rest

-- Breaks stuff
stripParens :: String -> String
stripParens "" = ""
stripParens ('-':'>':' ':'(':rest) = '-':'>':' ': stripParens rest
stripParens ('=':'>':' ':'(':rest) = '=':'>':' ': stripParens rest
stripParens (':':':':' ':'(':rest) = ':':':':' ': stripParens rest
stripParens (')':',':rest) = ',': stripParens rest
stripParens (')':'}':rest) = '}': stripParens rest
stripParens (')':' ':'-':'>':rest) = stripParens $ ' ':'-':'>': rest
stripParens (x:rest) = x : stripParens rest

stripModules :: String -> String
stripModules "" = ""
stripModules (c:s)
  | isUpper c = case span isAlpha (c:s) of
    (_, '.':rest) -> stripModules rest
    (taken, rest) -> taken ++ stripModules rest
  | otherwise = c : stripModules s

stripNumbers :: String -> String
stripNumbers "" = ""
stripNumbers (x:'_':a:b:c:rest)
  | isAlphaNum x && isNumber a && isNumber b && isNumber c = x : stripNumbers rest
stripNumbers (x:'_':a:b:rest)
  | isAlphaNum x && isNumber a && isNumber b = x : stripNumbers rest
stripNumbers (x:'_':a:rest)
  | isAlphaNum x && isNumber a = x : stripNumbers rest
stripNumbers (x:rest) = x : stripNumbers rest

stripTypes :: String -> String
stripTypes "" = ""
stripTypes ('(':t:' ':':':':':' ':rest)
  | isAlpha t = t:' ': stripTypes (drop 1 $ dropWhile (/= ')') rest)
stripTypes ('(':t:a:' ':':':':':' ':rest)
  | isAlpha t && isAlphaNum a = t:a:' ': stripTypes (drop 1 $ dropWhile (/= ')') rest)
stripTypes (x:rest) = x : stripTypes rest

-- | Strips indentation spaces from a string
unindent :: String -> String
unindent str = unlines $ map (drop $ minSpaces loc) loc
  where loc = lines str

-- | Counts the smallest number of spaces that any string in the list is
-- indented by
minSpaces :: [String] -> Int
minSpaces = minimum . map (length . takeWhile (== ' ')) . filter (/= "")

mkExample :: QuasiQuoter
mkExample = QuasiQuoter
  { quoteExp = \ex -> [|(ex, $(return $ toExp $ Exts.fromParseResult $ Exts.parseExpWithMode mymode $ "do\n" ++ ex))|]
  , quotePat = const $ error "ex: not an expression"
  , quoteType = const $ error "ex: not an expression"
  , quoteDec = const $ error "ex: not an expression"
  }

mkResetExample :: QuasiQuoter
mkResetExample = mkExample
  { quoteExp = \ex -> [|(ex, $(eitherQ id $ parseExp $ "\resetEvent -> do\n" ++ ex))|]
  }
