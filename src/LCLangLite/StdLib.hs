module LCLangLite.StdLib where

import LCLangLite.LanguageAst
import Gfx.GfxAst


box :: Maybe Block -> InterpreterProcess m Value
box block = 
