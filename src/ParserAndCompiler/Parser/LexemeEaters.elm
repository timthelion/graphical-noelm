module ParserAndCompiler.Parser.LexemeEaters where
{- base libraries -}
import String

{- external libraries -}
import open Parsers.ContinuationParser
import Parsers.ContinuationParser.LexemeEaters as LE
import Parsers.ContinuationParser.Specifics.Lexemes as Lexemes
import Parsers.CharacterClassification as CharacterClassification

{- internal modules -}
import ParserAndCompiler.Constants as Constants
import LevelizedGraphs.Graph as Graph

restOfLineAsString:  LexemeEater Char Char String
restOfLineAsString = LE.convertOutput String.fromList Lexemes.tillEndOfLine

miscLine: LexemeEater Char Char String
miscLine = restOfLineAsString

nodeNameEater: LexemeEater Char Char String
nodeNameEater = (LE.convertOutput String.fromList) <| (LE.untillMarker (String.toList Constants.nameObfuscator))

typeEater: LexemeEater Char Char String
typeEater = LE.convertOutput String.trim restOfLineAsString


-- Note: This looks ugly, but if we were to continue only untill the start of ANY comment, that would be bad.  We NEED to be more picky than that.
compiledPartEater: LexemeEater Char Char [Char]
compiledPartEater = LE.untillMarker (String.toList <| Constants.commentStart ++ Constants.languageMarker)

languageEater: LexemeEater Char Char Graph.Language
languageEater =
 (LE.convertOutputMaybe
   (\ls-> foldl
    (\l acc ->
     if | show l == ls -> Just l
        | otherwise -> acc)
     Nothing
     Graph.languages))
 <| (LE.convertOutput String.fromList)
 <| (LE.charset CharacterClassification.isLetter)

baseCodeMarkerEater: LexemeEater Char Char [Char]
baseCodeMarkerEater = LE.exactMatch (String.toList <| Constants.baseCodeMarker ++ Constants.codeQuoteOpen)

baseCodeEater: LexemeEater Char Char String
baseCodeEater =
 (LE.convertOutput String.fromList) <|
 LE.untillMarker <| String.toList Constants.codeQuoteClose

parentsMarkerEater: LexemeEater Char Char [Char]
parentsMarkerEater =
 LE.exactMatch (String.toList Constants.parentsMarker)

parentEater: LexemeEater Char Char String
parentEater
 =  LE.convertOutput String.fromList
 <| LE.charset (\c->not (c==','||c=='-'))

