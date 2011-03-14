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

import Text.XML.Light.Input
import Text.XML.Light.Types
import Text.Pandoc.Builder


readConfluence = undefined

-- ---------------------------------------------------------------------

isElement (Elem x) = True
isElement _ = False

-- ---------------------------------------------------------------------

data Confluence = StringData String
                | Id Integer
                | TempData Element
                deriving (Show)
                         
-- ---------------------------------------------------------------------
{-
Elem (Element {elName = QName {qName = "id", qURI = Nothing, qPrefix = Nothing}, elAttribs = [Attr {attrKey = QName {qName = "name", qURI = Nothing, qPrefix = Nothing}, attrVal = "id"}], elContent = [Text (CData {cdVerbatim = CDataText, cdData = "5865479", cdLine = Just 3})], elLine = Just 3})
-}

processId c =
  let
    (Text content) = head (elContent c)
    val = cdData content
  in
    Id $ read val

-- ---------------------------------------------------------------------

processProperty c = TempData c

-- ---------------------------------------------------------------------

processElement (Elem c) 
  | name == "id" = processId c
  | name == "property" = processProperty c                   
  | otherwise = StringData name                 
  where                
    name = qName (elName c)
    
-- ---------------------------------------------------------------------

processObject (Elem c) = 
  let
    -- (QName qname) = elName c
    -- name = qName qname
    name = qName (elName c)
    attribs = attrVal $ head (elAttribs c)
    content = map processElement (elContent c)
  in  
   (name, attribs, content)

--process :: [Content] -> [Content]
process cs = 
  let
    (Elem root) = head $ tail $ filter isElement cs
    objects = filter isElement $ elContent root
  in  
    map processObject objects

-- =====================================================================
-- Testing support

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

