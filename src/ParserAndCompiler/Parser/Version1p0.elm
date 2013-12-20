module ParserAndCompiler.Parser.Version1p0 where
{- standard libraries -}
import String

{- external libraries -}
import open Parsers.ContinuationParser
import Parsers.ContinuationParser.PositionMarking as PM
import Parsers.ContinuationParser.Specifics.Lexemes as Lexemes
import Parsers.ContinuationParser.LexemeEaters as LE

{- internal modules -}
import ParserAndCompiler.Types as PT
import ParserAndCompiler.Constants as Constants
import open ParserAndCompiler.Parser.LexemeEaters
import State.EditorState as ES
import LevelizedGraphs.Graph as Graph

version         = {major=1,minor=0}


t = PM.standardTaker

parseSavedGraph: Parser (PM.PositionMarked Char) ES.EditorState
parseSavedGraph =
 let
  defaultEditorState = ES.defaultEditorState
 in
 takeMisc <| \ misc _ ->
 takeGraph <| \ graph _ ->
 return <| Parsed
            {defaultEditorState|
             misc<-misc
            ,graph<-graph}

takeMisc: ContinuationParser (PM.PositionMarked Char) String Char ES.EditorState
takeMisc continuation =
 t.take (LE.exactMatch (String.toList <| Constants.startMisc++"\n")) <| \ _ _ ->
 takeMisc' [] continuation

takeMisc': [String] -> ContinuationParser (PM.PositionMarked Char) String Char ES.EditorState
takeMisc' acc continuation =
 t.take miscLine <| \ line transition ->
  if | line == Constants.endMisc -> continuation (String.join "\n" acc) transition
     | otherwise ->
        fastforward 1 <|
        takeMisc' (acc ++ [line]) continuation

takeGraph: ContinuationParser (PM.PositionMarked Char) Graph.Graph Char ES.EditorState
takeGraph = takeGraph' []

takeGraph': [Graph.Node] -> ContinuationParser (PM.PositionMarked Char) Graph.Graph Char ES.EditorState
takeGraph' acc continuation =
 t.takeWithFallbackValue Lexemes.whitespace (continuation acc ' ' []) <| \ _ _ -> -- << ugly passing the space there
 takeNode <| \ node _ ->
 takeGraph' (acc++[node]) continuation

{-
Take a node in the format:
main=(\analogClockFace digitalClockFace -> (\acf dcf->flow down [acf,dcf])<~analogClockFace~digitalClockFace)analogClockFacePrivateDon'tUse digitalClockFacePrivateDon'tUse{-_language_ElmLang_base_code_#####$#(\acf dcf->flow down [acf,dcf])<~)#$#####_parents_analogClockFace,digitalClockFace-}
-}
takeNode: ContinuationParser (PM.PositionMarked Char) Graph.Node () ES.EditorState
takeNode continuation =
 t.take nodeNameEater <| \ nodeName _ ->
 (takeNode' nodeName)
  `PM.markEndOfInputAsErrorAt`
 ("Error in node named:"++nodeName++" End of input, did not finish parsing.")
 <| continuation

takeNode': String -> ContinuationParser (PM.PositionMarked Char) Graph.Node () ES.EditorState
takeNode' nodeName continuation =
 t.take Lexemes.whitespace <| \ _ transition ->
  if | transition == ':' ->
       fastforward 1 <|
       t.take typeEater <| \ ntype _ ->
       t.take Lexemes.whitespace <| \ _ _ ->
       t.take nodeNameEater <| \ _ _ ->
       t.take Lexemes.whitespace <| \ _ transition ->
       takeNodeCode nodeName (Just ntype) transition continuation
     | otherwise ->
       takeNodeCode nodeName Nothing transition continuation

takeNodeCode: String -> Maybe String -> Char -> ContinuationParser (PM.PositionMarked Char) Graph.Node () ES.EditorState
takeNodeCode nodeName ntype transition continuation input =
 (\allIsGood-> if allIsGood
 then
  t.take compiledPartEater (\ _ _ ->
  t.take languageEater <| \ language _ ->
  takeBaseCode
   `PM.markEndOfInputAsErrorAt` "Close quote not found in code block."
  <| \ baseCode _ ->
  takeParents <| \ parents _ ->
  continuation {parents=parents
               ,name=nodeName
               ,value={language=language
                      ,ntype=ntype
                      ,code=baseCode}} ()) input
 else
  PM.parseErrorAts ("Unexpected input:" ++ show transition) input) (transition == '=')

takeBaseCode: ContinuationParser (PM.PositionMarked Char) String Char ES.EditorState
takeBaseCode continuation =
 t.take baseCodeMarkerEater <| \ _ _ ->
 t.take baseCodeEater continuation

takeParents: ContinuationParser (PM.PositionMarked Char) [String] Char ES.EditorState
takeParents continuation =
 t.take parentsMarkerEater <| \ _ _ ->
 takeParents' [] continuation

takeParents': [String] -> ContinuationParser (PM.PositionMarked Char) [String] Char ES.EditorState
takeParents' acc continuation =
 t.take parentEater <| \ parent transition ->
 if | transition == ',' ->
       fastforward 1 <|
       takeParents' (acc++[String.trim parent]) continuation
    | transition == '-' ->
       lookAhead 2 <| \ future' _ ->
       (\cont->map .char future' |> cont) <| \ future -> 
       if | future == ['-','}'] ->
            fastforward 2 <|
            if | String.trim parent == "" ->
                  continuation (acc) '}'
               | otherwise ->
                  continuation (acc++[parent]) '}'
          | otherwise ->
            PM.parseErrorAts <| "Unexpected input:" ++ show future

{-
Graphical ELM - A program for editing graphs as graphs.
Visually, Architecturally

    Copyright (C) 2013  Timothy Hobbs <timothyhobbs@seznam.cz> thobbs.cz

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU Affero General Public License as
    published by the Free Software Foundation, either version 3 of the
    License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Affero General Public License for more details.

    You should have received a copy of the GNU Affero General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
-}
