{- |
   Module      : Text.Pandoc.Readers.Confluence
   Copyright   : Copyright (C) 2011 ??
   License     : GNU GPL, version 2 or above 

   Maintainer  : ??
   Stability   : alpha 
   Portability : portable

Conversion of Confluence export xml format Wiki  to 'Pandoc' document.
-}
module Text.Pandoc.Readers.Confluence ( readConfluence
                                      ) where

import qualified Data.Map as Map
import Text.XML.Light.Input
import Text.XML.Light.Types
import Text.Pandoc.Builder

import Data.List (foldl')


readConfluence = undefined


-- e.g. readConfluenceFile "tests/confluence-entities.xml"
--readConfluenceFile
--  :: FilePath -> IO [(String, String, [Confluence])]
readConfluenceFile filename =
  do 
     x <- readFile (filename)
     return $ process $ parseXML x
     
-- ---------------------------------------------------------------------

isElement (Elem x) = True
isElement _ = False

-- ---------------------------------------------------------------------

data Confluence = StringData String
                | Id Integer
                | Property String [Confluence] -- name children
                | Collection String [Confluence] -- name children
                | CollectionElement String Confluence -- name id
                | Properties (Map.Map String [Confluence]) -- map of property names to values
                | TempData Element
                deriving (Show)
                         
-- ---------------------------------------------------------------------

flattenAttr :: Text.XML.Light.Types.Attr -> (String, String)
flattenAttr a =
  let
    key = qName $ attrKey a
    val = attrVal a
  in
   (key,val)

-- ---------------------------------------------------------------------
{-
Elem (Element {elName = QName {qName = "id", qURI = Nothing, qPrefix = Nothing}, elAttribs = [Attr {attrKey = QName {qName = "name", qURI = Nothing, qPrefix = Nothing}, attrVal = "id"}], elContent = [Text (CData {cdVerbatim = CDataText, cdData = "5865479", cdLine = Just 3})], elLine = Just 3})
-}

processId :: Element -> Confluence
processId c =
  let
    (Text content) = head (elContent c)
    val = cdData content
  in
    Id $ read val

-- ---------------------------------------------------------------------
-- Property always has name attr, may have class attr

processProperty :: Element -> Confluence
processProperty c = 
  let
    attribs = map flattenAttr $ elAttribs c
    content = filter noEmptyStringData $ map processElement (elContent c)
  in  
    Property (snd $ head attribs) content

-- ---------------------------------------------------------------------
-- Collection has name and class, which identifies the java class.
processCollection :: Element -> Confluence
processCollection c =
  let
    attribs = map flattenAttr $ elAttribs c
    content = filter noEmptyStringData $ map processElement (elContent c)
  in  
    Collection (snd $ head attribs) content

-- ---------------------------------------------------------------------

processElementTag :: Element -> Confluence
processElementTag c =
  let
    attribs = map flattenAttr $ elAttribs c
    content = filter noEmptyStringData $ map processElement (elContent c)
  in  
    CollectionElement (snd $ head attribs) (head content)

-- ---------------------------------------------------------------------

processElement :: Content -> Confluence
processElement (Elem c) 
  | name == "id" = processId c
  | name == "property" = processProperty c
  | name == "collection" = processCollection c
  | name == "element" = processElementTag c
  | otherwise = StringData name                 
  where                
    name = qName (elName c)
processElement (Text c) = StringData (cdData c)
processElement x = StringData $ show x
    
    
-- ---------------------------------------------------------------------
{- Note: there are also composite ids. ignoring for now.

Example:
<composite-id><property name="entityName" type="string"><![CDATA[confluence_ContentEntityObject]]></property>
<property name="entityId" type="long">6914050</property>
<property name="key" type="string"><![CDATA[socialbookmarkingurl]]></property>
</composite-id>
-}

getId :: [Confluence] -> Confluence
getId cs = 
  let
    idList = dropWhile notId cs
  in  
    idOrNone idList
  where
    notId (Id x) = False
    notId _      = True
    
    idOrNone [] = Id (-1)
    idOrNone xs = head xs
  
-- ---------------------------------------------------------------------

noEmptyStringData (StringData "\n") = False
noEmptyStringData _ = True

processObject :: Content -> (String, Confluence, String, [Confluence])
processObject (Elem c) = 
  let
    name = qName (elName c)
    attribs = attrVal $ head (elAttribs c)
    content = filter noEmptyStringData $ map processElement (elContent c)
    oid = getId content
  in  
   (name, oid, attribs, content)

--process :: [Content] -> [Content]
processXml :: [Content] -> [(String, Confluence, String, [Confluence])]
processXml cs = 
  let
    (Elem root) = head $ tail $ filter isElement cs
    objects = filter isElement $ elContent root
  in  
    map processObject objects

-- ---------------------------------------------------------------------

--process :: [Content] -> [(String, String)]
--process cs = map handleObject $ processXml cs
process cs = 
  let
    (m,pages) = foldl' buildStructure (Map.empty,[]) $ processXml cs
    pages' = filter (\oid -> livePage m oid) pages
  in  
    map (\oid -> m Map.! oid) pages'

livePage :: Map.Map Integer (String,[Confluence]) -> Integer -> Bool
livePage m oid =
  let
    (name,cs) = (m Map.! oid)
    (Properties props) = head cs
  in  
    Map.notMember "originalVersion" props 
        
-- ---------------------------------------------------------------------

buildStructure
  :: (Map.Map Integer ([Char], [Confluence]), [Integer])
     -> (t, Confluence, [Char], [Confluence])
     -> (Map.Map Integer ([Char], [Confluence]), [Integer])
buildStructure (m,pages) o = 
  let
    (name,Id idval,contents) = handleObject o
    pages' = if (name == "Page") then idval:pages else pages
  in 
   (Map.insert idval (name,contents) m, pages')

-- ---------------------------------------------------------------------

--handleObject :: (String, Confluence, String, [Confluence]) -> (String, [Confluence])
handleObject (_,oid,"Attachment",contents)              = ("Attachment",oid, collectProperties contents)
handleObject (_,oid,"BodyContent",contents)             = ("BodyContent",oid, collectProperties contents)
handleObject (_,oid,"BucketPropertySetItem",contents)   = ("BucketPropertySetItem",oid, collectProperties contents)
handleObject (_,oid,"ConfluenceBandanaRecord",contents) = ("ConfluenceBandanaRecord",oid, collectProperties contents)
handleObject (_,oid,"OutgoingLink",contents)            = ("OutgoingLink",oid, collectProperties contents)
handleObject (_,oid,"Page",contents)                    = ("Page",oid, collectProperties contents)
handleObject (_,oid,"Space",contents)                   = ("Space",oid, collectProperties contents)
handleObject (_,oid,"SpacePermission",contents)         = ("SpacePermission",oid, collectProperties contents)
handleObject (_,oid,"SpaceDescription",contents)        = ("SpaceDescription",oid, collectProperties contents)

-- ---------------------------------------------------------------------

collectProperties :: [Confluence] -> [Confluence]
collectProperties cs = 
  let
    (m,r) = foldl' gather (Map.empty,[]) cs
  in
   (Properties m):r
  where
    gather
      :: (Map.Map String [Confluence], [Confluence])
         -> Confluence
         -> (Map.Map String [Confluence], [Confluence])
    gather (m,others) (Property name rest) = (Map.insert name rest m,others) 
    gather (m,others) c                    = (m,                     others++[c])     
    

    

-- =====================================================================
-- Testing support

t = readConfluenceFile "tests/confluence-entities.xml"


_page :: [Confluence]
_page =   
  [
  Id 6914052,
  Property "position" [],
  Property "title" [StringData "Home"],
  Collection "bodyContents" [CollectionElement "BodyContent" (Id 6979588)],
  Property "version" [StringData "1"],
  Property "creatorName" [StringData "alanz"],
  Property "creationDate" [StringData "2011-03-14 20:25:24.676"],
  Property "lastModifierName" [StringData "alanz"],
  Property "lastModificationDate" [StringData "2011-03-14 20:25:24.676"],
  Property "versionComment" [StringData ""],
  Property "originalVersion" [Id 6914050],
  Property "contentStatus" [StringData "current"]
  ]
  


tXml :: [Content]
tXml = parseXML _test_str

_test_str :: [Char]
_test_str = 
  "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" ++
  "<hibernate-generic datetime=\"2011-03-07 16:45:43\">\n" ++
  "<object class=\"Page\" package=\"com.atlassian.confluence.pages\">" ++
  "<id name=\"id\">5799945</id>" ++
  "<property name=\"position\"/><property name=\"parent\" class=\"Page\" package=\"com.atlassian.confluence.pages\"><id name=\"id\">5275654</id>" ++
  "</property>" ++
  "<collection name=\"ancestors\" class=\"java.util.List\"><element class=\"Page\" package=\"com.atlassian.confluence.pages\"><id name=\"id\">5275654</id>" ++
  "</element>" ++
  "</collection>" ++
  "<property name=\"space\" class=\"Space\" package=\"com.atlassian.confluence.spaces\"><id name=\"id\">5341185</id>" ++
  "</property>" ++
  "<property name=\"title\"><![CDATA[Club]]></property>" ++
  "<collection name=\"bodyContents\" class=\"java.util.Collection\"><element class=\"BodyContent\" package=\"com.atlassian.confluence.core\"><id name=\"id\">5865479</id>" ++
  "</element>" ++
  "</collection>" ++
  "<collection name=\"outgoingLinks\" class=\"java.util.Collection\"><element class=\"OutgoingLink\" package=\"com.atlassian.confluence.links\"><id name=\"id\">6062121</id>" ++
  "</element>" ++
  "<element class=\"OutgoingLink\" package=\"com.atlassian.confluence.links\"><id name=\"id\">6062122</id>" ++
  "</element>" ++
  "<element class=\"OutgoingLink\" package=\"com.atlassian.confluence.links\"><id name=\"id\">6062123</id>" ++
  "</element>" ++
  "<element class=\"OutgoingLink\" package=\"com.atlassian.confluence.links\"><id name=\"id\">6062124</id>" ++
  "</element>" ++
  "<element class=\"OutgoingLink\" package=\"com.atlassian.confluence.links\"><id name=\"id\">6062125</id>" ++
  "</element>" ++
  "</collection>" ++
  "<property name=\"version\">4</property>" ++
  "<property name=\"creatorName\"><![CDATA[andreww]]></property>" ++
  "<property name=\"creationDate\">2010-11-26 14:26:12.990</property>" ++
  "<property name=\"lastModifierName\"><![CDATA[andreww]]></property>" ++
  "<property name=\"lastModificationDate\">2010-11-26 15:10:37.153</property>" ++
  "<property name=\"versionComment\"><![CDATA[]]></property>" ++
  "<collection name=\"historicalVersions\" class=\"java.util.Collection\"><element class=\"Page\" package=\"com.atlassian.confluence.pages\"><id name=\"id\">5799949</id>" ++
  "</element>" ++
  "<element class=\"Page\" package=\"com.atlassian.confluence.pages\"><id name=\"id\">5799946</id>" ++
  "</element>" ++
  "<element class=\"Page\" package=\"com.atlassian.confluence.pages\"><id name=\"id\">5799948</id>" ++
  "</element>" ++
  "</collection>" ++
  "<property name=\"contentStatus\"><![CDATA[current]]></property>" ++
  "<collection name=\"attachments\" class=\"java.util.Collection\"><element class=\"Attachment\" package=\"com.atlassian.confluence.pages\"><id name=\"id\">6029315</id>" ++
  "</element>" ++
  "</collection>" ++
  "</object>" ++
  "</hibernate-generic>"


-- EOF

