module QQ where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Prettyprinter
import QQ.Compiler
import QQ.Eval
import QQ.Pretty
import System.FilePath
import ZM.Parser.Exp
import ZM.Parser.Types (unAnn)

{-
>>> tt "rec"
"VCon \"S\" [VCon \"S\" [VCon \"Z\" []]]"

>>> tt "parser"
"17:5:\n   |\n17 |  c1 =\n   |     ^\nunexpected '='\nexpecting '\"', '&', ''', '(', '+', '-', '?', '[', '{', digit, integer, lowercase letter, or uppercase letter\n"

>>> tt "x"
unsupported compilation of VCase [Lambda [("addIntInt",VPrim "addIntInt (  )"),("subIntInt",VPrim "subIntInt (  )")] (VCon "F" []) (Ann 12 (Con "T")),Lambda [("addIntInt",VPrim "addIntInt (  )"),("subIntInt",VPrim "subIntInt (  )")] (VCon "T" []) (Ann 22 (Con "F"))]
-}
tt mdlName = testMdl $ concat ["../qq/qq-src/", mdlName, ".qq"]

-- testMdl :: FilePath -> IO ()
testMdl :: FilePath -> IO Text
testMdl fileName = do
    src <- T.readFile fileName
    case testPretty src of
        Left e -> return $ T.pack e
        Right prettySrc -> do
            T.writeFile fileName prettySrc
            let Right exp = parseMdl src
            let result = stdEval exp
            let jsFile = takeDirectory (takeDirectory fileName) </> "js" </> replaceExtension (takeFileName fileName) "js"
            compileToFile jsFile exp
            return (T.pack . show $ result)

-- return $ T.pack jsFile

-- case stdEval exp of
--     Left e -> return e
--     Right result -> do
--         T.writeFile (replaceExtension fileName "js") $ stdCompile exp
--         return result

-- Test that the pretty printed version has the same semantic as the original
testPretty :: Text -> Either String Text
testPretty src =
    case parseMdl src of
        Left e -> Left e
        Right syntax ->
            let src2 = show . pretty . unAnn $ syntax
                syntax1 = unAnn syntax
             in case parseMdlF $ T.pack src2 of
                    Left e -> Left e
                    Right syntax2
                        | syntax1 == syntax2 -> Right $ T.pack src2
                        | otherwise -> Left (unlines ["bad pretty: ", src2, "semantic was", show syntax1, T.unpack src, "now is", show syntax2, src2])
