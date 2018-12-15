{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}

import Data.List
import Data.Maybe
import Debug.Trace

data VCard = VCard {
  name :: String,
  fullName :: String,
  org :: String,
  title :: String,
  tel :: [(String, String)],
  adr :: [(String, String)],
  lab :: [(String, String)],
  email :: [(String, String)],
  note :: String
} deriving Show

mapFst :: (a -> b) -> (a, c) -> (b, c)
mapFst f (x, y) = (f x, y)

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn x = foldr f [[]]
  where f y (ys:yss)
          | x == y    = [] : ys : yss
          | otherwise = (y:ys) : yss

splitOnFst :: Eq a => a -> [a] -> ([a],[a])
splitOnFst x [] = ([],[])
splitOnFst x (y:ys)
  | x == y    = ([],ys)
  | otherwise = mapFst (y:) (splitOnFst x ys)

cleanNewlines :: String -> String
cleanNewlines  []            = []
cleanNewlines ('\r':'\n':xs) = '\n' : cleanNewlines xs
cleanNewlines (x:xs)         = x : cleanNewlines xs

stripFromStart :: Eq a => [a] -> [a] -> [a]
stripFromStart xs ys = fromMaybe ys (f xs ys)
  where f :: Eq a => [a] -> [a] -> Maybe [a]
        f  []     ys  = Just ys
        f  _      []  = Nothing
        f (x:xs) (y:ys)
          | x == y    = f xs ys
          | otherwise = Nothing

emptyCard = VCard "" "" "" "" [] [] [] [] ""

readVCards :: [String] -> [VCard]
readVCards = foldl (flip processLine) []
  where processLine :: String -> [VCard] -> [VCard]
        processLine s =
          let (header,value) = splitOnFst ':' . stripFromStart "item2." . stripFromStart "item1." $ s in
          let (field:params) = splitOn ';' header in
          if value == "" then id
          else processField (field, map (splitOnFst '=') params, value) 
        processField :: (String,[(String, String)],String) -> [VCard] -> [VCard]
        processField ("BEGIN"          ,[]                              ,"VCARD") = (emptyCard :)
        processField ("VERSION"        ,[]                              ,_      ) = id
        processField ("PRODID"         ,[]                              ,_      ) = id
        processField ("N"              ,[]                              ,n      ) = \(x:xs) -> x { name     = n }                : xs
        processField ("FN"             ,[]                              ,n      ) = \(x:xs) -> x { fullName = n }                : xs
        processField ("ORG"            ,[]                              ,o      ) = \(x:xs) -> x { org      = o }                : xs
        processField ("TITLE"          ,[]                              ,t      ) = \(x:xs) -> x { title    = t }                : xs
        processField ("TEL"            ,("type",t):_                    ,n      ) = \(x:xs) -> x { tel      = (t ,n) : tel   x } : xs
        processField ("TEL"            ,_                               ,n      ) = \(x:xs) -> x { tel      = ("",n) : tel   x } : xs
        processField ("ADR"            ,("type",t):_                    ,a      ) = \(x:xs) -> x { adr      = (t ,a) : adr   x } : xs
        processField ("ADR"            ,_                               ,a      ) = \(x:xs) -> x { adr      = ("",a) : adr   x } : xs
        processField ("LABEL"          ,("type",t):_                    ,a      ) = \(x:xs) -> x { lab      = (t ,a) : lab   x } : xs
        processField ("LABEL"          ,_                               ,a      ) = \(x:xs) -> x { lab      = ("",a) : lab   x } : xs
        processField ("EMAIL"          ,("type","INTERNET"):("type",t):_,e      ) = \(x:xs) -> x { email    = (t ,e) : email x } : xs
        processField ("EMAIL"          ,_                               ,e      ) = \(x:xs) -> x { email    = ("",e) : email x } : xs
        processField ("NOTE"           ,[]                              ,n      ) = \(x:xs) -> x { note     = n }                : xs
        processField ("CATEGORIES"     ,[]                              ,_      ) = id
        processField ("UID"            ,[]                              ,_      ) = id
        processField ("URL"            ,[]                              ,_      ) = id
        processField ("END"            ,[]                              ,"VCARD") = id
        processField ("X-ABUID"        ,[]                              ,_      ) = id
        processField ("X-SOCIALPROFILE",_                               ,_      ) = id
        processField ("X-ABADR"        ,[]                              ,_      ) = id
        processField ("X-ABLabel"      ,[]                              ,_      ) = id
        processField ("X-Jabber"       ,[]                              ,_      ) = id
        processField ("IMPP"           ,_                               ,_      ) = id
        processField ("BDAY"           ,_                               ,_      ) = id
        processField ("PHOTO"          ,_                               ,_      ) = id
        processField (field,args,value) = id -- error ("Unrecognised field " ++ field ++ " with value " ++ value ++ " and args " ++ show args)

headerRow :: [String]
headerRow = ["Full Name", "Surname", "Title", "Company", "Addresses"]

showVCard :: VCard -> [String]
showVCard card = [replaceWithNewlines . intercalate " " . filter (not . null) $ otherNames, surname, title card, org card] ++ concat [[t,cleanBlankLines . replaceWithNewlines $ a] | (t,a) <- adr card]
  where (surname:otherNames) = splitOn ';' . name $ card
        cleanBlankLines :: String -> String
        cleanBlankLines  ""        = ""
        cleanBlankLines (';':xs)  = cleanBlankLines xs
        cleanBlankLines (x:';':xs) = x : ';' : cleanBlankLines xs
        cleanBlankLines (x:xs)     = x : cleanBlankLines xs
        replaceWithNewlines :: String -> String
        replaceWithNewlines  ""               = ""
        replaceWithNewlines (';':xs)          = ';' : replaceWithNewlines xs
        replaceWithNewlines ('\\':',':' ':xs) = ';' : replaceWithNewlines xs
        replaceWithNewlines ('\\':',':xs)     = ';' : replaceWithNewlines xs
        replaceWithNewlines (',':xs)     = ';' : replaceWithNewlines xs
        replaceWithNewlines ('\\':'n':xs)     = ';' : replaceWithNewlines xs
        replaceWithNewlines (x:xs)            = x : replaceWithNewlines xs

main = putStrLn =<< (intercalate "\n" . map (intercalate ",") . (headerRow :) . map showVCard . readVCards . splitOn '\n' . cleanNewlines <$> getContents)

