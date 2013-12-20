module ParserAndCompiler.Types where

{-| The version of the file format. Minor versions add new syntaxes whereas major versions break old syntaxes. This means, that a parser version 1.2 can parse code of version 1.0, whereas a parser of version 2.0 cannot.-}
type Version = {major:Int,minor:Int}

showVersion: Version -> String
showVersion version = (show version.major)++"."++(show version.minor)