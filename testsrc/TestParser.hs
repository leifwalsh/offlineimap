{-
Copyright (C) 2002-2008 John Goerzen <jgoerzen@complete.org>

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
-}
module TestParser where
import qualified Data.Map as Map
import Data.List
import qualified Test.HUnit as HU
import Test.HUnit.Utils
import Data.Word
import Test.QuickCheck
import TestInfrastructure

import Network.IMAP.Parser
import Network.IMAP.Connection
import Network.IMAP.Types

import TestInfrastructure
import TestConnection(expectedString, noCR)
import TestParserPrim(isValidText, isValidAtom)

prop_getFullLine_basic :: [String] -> Property
prop_getFullLine_basic s =
    (null s || not ("}" `isSuffixOf` (head s))) && noCR s ==>
        runLinesConnection s (getFullLine []) @?= 
            if null s
               then Left "EOF in input in readLine"
               else Right (head s, (expectedString (tail s), []))

prop_getFullLine_count :: [String] -> Property
prop_getFullLine_count s =
    length s >= 2 && noCR s && (length s < 3 || not ("}" `isSuffixOf` (s !! 2)))
           ==> 
           runLinesConnection lenS (getFullLine []) @?=
                              Right (expectedResult, (expectedRemain, []))
    where lenS = [braceString] ++ [(head . tail $ s)] ++ drop 2 s
          braceString = head s ++ "{" ++ show (length (s !! 1)) ++ "}"
          expectedResult = braceString ++ "\r\n" ++ (s !! 1)
          expectedRemain = expectedString (drop 2 s)

prop_rfr_basic :: [String] -> Property
prop_rfr_basic s =
    let testlist = 
            case length s of
              0 -> []
              1 -> ["TAG " ++ head s]
              _ -> map ("* " ++) (init s) ++
                   ["TAG " ++ last s]
        resultstr = expectedString testlist
    in noCR s && noBrace s ==>
       runLinesConnection testlist readFullResponse @?=
             if null s
                then Left "EOF in input in readLine"
                else Right (resultstr, ([], []))

noBrace s = and (map (not . isSuffixOf "}") s)

prop_respTextSimple :: String -> Result
prop_respTextSimple s =
    p respText s @?= 
      if isValidText s && (head s /= '[')
         then Just (RespText Nothing s)
         else Nothing

prop_respTextAtom :: String -> Property
prop_respTextAtom s2 =
    isValidAtom s2 && isValidText s1 && ']' `notElem` s1 ==>
    p respText ("[" ++ s2 ++ "] " ++ s1) @?=
      Just (RespText (Just s2) s1)
    where s1 = reverse s2 -- Gen manually to avoid test exhaustion
          
prop_respTextAtomOpt :: String -> String -> String -> Property
prop_respTextAtomOpt codeatom codedesc text =
    isValidAtom codeatom && isValidText codedesc && isValidText text &&
                ']' `notElem` (codeatom ++ codedesc) &&
                (head text) /= '[' ==>
    p respText ("[" ++ codeatom ++ " " ++ codedesc ++ "] " ++ text) @?=
      Just (RespText (Just (codeatom ++ " " ++ codedesc)) text)

prop_greeting_bye :: String -> Property
prop_greeting_bye s =
    isValidAtom s && head s /= '[' ==>
    p greeting ("* BYE " ++ s) @?=
      (Just $ Left $ RespText Nothing s)

prop_greeting_auth :: String -> Property
prop_greeting_auth s =
    isValidAtom s && head s /= '[' ==>
    p' greeting ("* OK " ++ s) @?=
      (Right $ Right $ (AUTHOK, RespText Nothing s))

prop_greeting_courier :: Result
prop_greeting_courier =
    p greeting courierStr @?=
      (Just $ Right $ (AUTHOK, RespText (Just code) text))
    where courierStr = "* OK [CAPABILITY IMAP4rev1 UIDPLUS CHILDREN NAMESPACE THREAD=ORDEREDSUBJECT THREAD=REFERENCES SORT QUOTA IDLE ACL ACL2=UNION] Courier-IMAP ready. See COPYING for distribution information."
          code = "CAPABILITY IMAP4rev1 UIDPLUS CHILDREN NAMESPACE THREAD=ORDEREDSUBJECT THREAD=REFERENCES SORT QUOTA IDLE ACL ACL2=UNION"
          text = "Courier-IMAP ready. See COPYING for distribution information."

allt = [q "getFullLine_basic" prop_getFullLine_basic,
        q "getFullLine_count" prop_getFullLine_count,
        q "readFullResponse_basic" prop_rfr_basic,
        q "respText simple" prop_respTextSimple,
        q "respText atom" prop_respTextAtom,
        q "respTextAtomOpt" prop_respTextAtomOpt,
        q "greeting_bye" prop_greeting_bye,
        q "greeting_auth" prop_greeting_auth,
        q "greeting_courier" prop_greeting_courier
       ]
