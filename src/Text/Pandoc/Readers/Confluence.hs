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

import Codec.Archive.Zip
import Control.Applicative ( (<$>) )
import Control.Monad
import Data.Foldable (foldlM)
import Data.List (foldl', sort)
import Lib.Git
import System.Directory
import System.IO
import Text.Pandoc.Builder
import Text.XML.Light.Input
import Text.XML.Light.Types
import qualified Data.ByteString.Lazy as B
import qualified Data.Map as Map

-- ---------------------------------------------------------------------

readConfluence = undefined

-- e.g. readConfluenceFile "tests/confluence-entities.xml"
readConfluenceFile
  :: FilePath
     -> IO [(VersionInfo, [(ChangeType, [Char], [Char])])]
readConfluenceFile filename =
  do 
    x <- readFile (filename)
    let changes = doProcess $ parseXML x
    makeGitVersion emptyArchive changes "/tmp/flub"    
    return changes    
     
-- e.g. readConfluenceZip "tests/CONFLUENCE.zip"
readConfluenceZip
  :: FilePath
     -> IO [(VersionInfo, [(ChangeType, [Char], [Char])])]
readConfluenceZip filename =
  do 
    exists <- doesFileExist filename
    archive <- toArchive <$> B.readFile filename
    let changes = doProcess $ parseXML $ getFileFromArchive archive "entities.xml"
    makeGitVersion archive changes "/tmp/flub"    
    return changes    

-- ---------------------------------------------------------------------

getFileFromArchive :: Archive -> FilePath -> B.ByteString
getFileFromArchive archive filepath = 
    case (findEntryByPath filepath archive) of
      Just entry -> fromEntry entry
      _          -> B.empty

-- ---------------------------------------------------------------------
      
notDoesDirectoryExist :: FilePath -> IO Bool
notDoesDirectoryExist path = do
  x <- doesDirectoryExist path
  case (x) of
    True -> return False
    False -> return True
      
makeCleanFilePath :: [Char] -> IO FilePath
makeCleanFilePath path = do liftM head $ filterM (\p -> notDoesDirectoryExist p) candidatePaths
  where
    candidatePaths = map (path++) ([""] ++ ( map ("."++) $ map show [1..10] ))
  
-- ---------------------------------------------------------------------
      

isElement :: Content -> Bool
isElement (Elem x) = True
isElement _ = False

-- ---------------------------------------------------------------------

data Confluence = StringData String
                | Id Integer
                | Property String [Confluence] -- name children
                | Collection String [Confluence] -- name children
                | CollectionElement String Confluence -- name id
                | Properties (Map.Map String [Confluence]) -- map of property names to values
                | Collections (Map.Map String [Confluence]) -- map of collection names to values
                | TempData Element
                deriving (Show)
                         
data ChangeType = ChSpace | ChPage | ChAttachment | ChOther
              deriving (Eq,Show)
                       
type Change = (ChangeType,  
               String,  -- filename
               String)  -- body

type VersionInfo = (String, -- committer
                    String, -- commit date
                    String, -- message
                    String) -- version id

-- ---------------------------------------------------------------------

unescapeStr :: String -> String
unescapeStr str = read str

-- ---------------------------------------------------------------------

--generateFile :: Config -> String -> (ChangeType, String, String) -> IO ()
generateFile :: Archive -> Config -> String -> (ChangeType, String, String) -> IO Bool
generateFile archive cfg path (ChPage,filename,body) = 
  do
    let fullfilename = path ++ "/" ++ filename ++ ".textile"
    writeFile fullfilename (unescapeStr body)
    runGit cfg (add [fullfilename])
    return True

generateFile archive cfg path (ChAttachment,filename,filepath) = 
  do
    let fullfilename = path ++ "/" ++ filename 
    let body = getFileFromArchive archive filepath   
    B.writeFile fullfilename body
    runGit cfg (add [fullfilename])
    return True

generateFile archive cfg path change = do return False
 
-- ---------------------------------------------------------------------

foldGenerateFile
  :: Archive -> Config -> String -> Bool -> (ChangeType, String, String) -> IO Bool
foldGenerateFile archive cfg path acc change =
  do
    doCommit <- generateFile archive cfg path change
    return (doCommit || acc)

-- ---------------------------------------------------------------------
    
commitIfNecessary :: (Monad m) => Bool -> m a -> m ()
commitIfNecessary True cmd   = do cmd; return ()    
commitIfNecessary False _cmd =         return ()

-- ---------------------------------------------------------------------

doOneGitChange
  :: Archive -> Config -> String -> (VersionInfo,[(ChangeType, String, String)]) -> IO [()]
doOneGitChange archive cfg path (version,changeSet) = 
  do
    doCommit <- foldlM (foldGenerateFile archive cfg path) False changeSet
    let author_email = "author@example.com"
    let (author,date,logmsg,versionid) = version    
    commitIfNecessary doCommit (runGit cfg (commit [] author author_email (logmsg++versionid) ["--date",date]))
    
    return [()]
  
-- ---------------------------------------------------------------------

makeGitVersion :: Archive -> [(VersionInfo,[Change])] -> FilePath -> IO ()
makeGitVersion archive changes path = 
  do
    createDirectoryIfMissing True path
    
    let cfg = makeConfig path Nothing
    runGit cfg (initDB False)
    
    mapM (doOneGitChange archive cfg path) changes

    return ()
    
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
  | name == "id"         = processId c
  | name == "property"   = processProperty c
  | name == "collection" = processCollection c
  | name == "element"    = processElementTag c
  | otherwise            = StringData name            
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

renderBody :: t -> (t1, [Confluence]) -> String
renderBody m (_,(Properties props:Collections collections:rest)) = 
  let
    [StringData body] = props Map.! "body"
  in
    body

-- ---------------------------------------------------------------------

renderPage
  :: Map.Map Integer (t1, [Confluence])
     -> (t, [Confluence])
     -> (String, String)
renderPage m (_,(Properties props:Collections collections:rest)) = 
  let
    [StringData title] = props Map.! "title"
    [CollectionElement _ (Id bcid)] = collections Map.! "bodyContents"
    bc = m Map.! bcid
  in
    (title,renderBody m bc)

-- ---------------------------------------------------------------------

extractDates
  :: Integer -> (t1, [Confluence]) -> (String, String, String, Integer)
extractDates (-1) _ = ("","","",-1) 
extractDates oid (_,(Properties props:Collections collections:rest)) = 
  let
    [StringData creationDate]         = Map.findWithDefault [(StringData "")] "creationDate" props
    [StringData lastModificationDate] = Map.findWithDefault [(StringData "")] "lastModificationDate" props
    [StringData lastModifierName]     = Map.findWithDefault [(StringData "")] "lastModifierName" props
  in
    (lastModificationDate,lastModifierName,creationDate,oid)

-- ---------------------------------------------------------------------

groupByChangeSet
  :: Map.Map [Char] [([Char], t, t1, t2)]
     -> ([Char], t, t1, t2)
     -> Map.Map [Char] [([Char], t, t1, t2)]
groupByChangeSet m r@("",_,_,_) = m
groupByChangeSet m r@(lastModificationDate,lastModifierName,creationDate,oid)
  | Map.member lastModificationDate m = Map.insert lastModificationDate (r:(m Map.! lastModificationDate)) m
  | otherwise                         = Map.insert lastModificationDate [r] m

-- ---------------------------------------------------------------------

getProp :: (Ord k) => k -> Map.Map k [Confluence] -> String
getProp k m = 
  let
    v = if (Map.member k m) then (m Map.! k) else [(StringData "")]
  in
    case v of
      [(StringData val)] -> val
      [(Id val)]         -> show(val)
      _                  -> show(v)
   
-- ---------------------------------------------------------------------
    
renderObject
  :: Map.Map Integer (t1, [Confluence])
     -> [Char]
     -> Map.Map [Char] [Confluence]
     -> Map.Map [Char] [Confluence]
     -> [Confluence]
     -> Change
renderObject m "SpaceDescription" props collections rest = (ChOther, "", "SpaceDescription:ignore")
{-
  [Properties (fromList 
    [
    ("creationDate",[StringData "2011-03-14 20:25:24.056"]),
    ("creatorName",[StringData "alanz"]),
    ("description",[Id 6914049]),
    ("homePage",[Id 6914050]),
    ("key",[StringData "PLAY1"]),
    ("lastModificationDate",[StringData "2011-03-14 20:25:24.056"]),
    ("lastModifierName",[StringData "alanz"]),
    ("name",[StringData "Play"]),
    ("spaceType",[StringData "global"])]),
-}
renderObject m "Space" props collections rest = 
  let
    key = (getProp "key" props)
    name = (getProp "name" props)  
  in    
    (ChSpace, key ++ ":" ++ name, "Space:" ++ key ++ ":" ++ name)

renderObject m "SpacePermission" props collections rest = (ChOther, "","SpacePermission:ignore")

renderObject m "Page" props collections rest = 
  let
    title = (getProp "title" props)

    -- "position" property? Empty list.
    -- [CollectionElement "bodyContent" (Id bodyId)] = collections Map.! "bodyContents"
    [CollectionElement _ (Id bodyId)] = collections Map.! "bodyContents"
    
    pageBody = renderBody m (m Map.! bodyId)
    
  in         
    (ChPage,title,show(pageBody))

{-
<object class="Attachment" package="com.atlassian.confluence.pages">
<id name="id">7077889</id>
<property name="fileName"><![CDATA[20091211238.jpg]]></property>
<property name="contentType"><![CDATA[image/jpeg]]></property>
<property name="content" class="Page" package="com.atlassian.confluence.pages"><id name="id">6914050</id>
</property>
<property name="creatorName"><![CDATA[alanz]]></property>
<property name="creationDate">2011-03-14 20:28:54.777</property>
<property name="lastModifierName"><![CDATA[alanz]]></property>
<property name="lastModificationDate">2011-03-14 20:28:54.777</property>
<property name="fileSize">642789</property>
<property name="comment"/><property name="attachmentVersion">1</property>
</object>
-}
renderObject m "Attachment" props collections rest = 
  let
    fileName          = (getProp "fileName" props)
    contentId         = (getProp "content" props)
    attachmentVersion = (getProp "attachmentVersion" props) 

    [(Id oid)] = rest
    
    entrypath = "attachments/"++contentId++"/"++show(oid)++"/"++attachmentVersion
    
    pageBody = entrypath
    
  in         
    (ChAttachment,fileName,pageBody)

renderObject m name props collections rest = (ChOther, name,name)

-- ---------------------------------------------------------------------
{-
  ("contentStatus",[StringData "current"]),
  ("creationDate",[StringData "2011-03-14 20:25:24.676"]),
  ("creatorName",[StringData "alanz"]),
  ("lastModificationDate",[StringData "2011-03-14 20:25:24.676"]),
  ("lastModifierName",[StringData "alanz"]),
  ("originalVersion",[Id 6914050]),
  ("version",[StringData "1"]),
  ("versionComment",[StringData ""])
-}

getVersionInfo
  :: Map.Map String [Confluence] -> VersionInfo
getVersionInfo props =
  let
    contentStatus        = getProp "contentStatus" props
    creationDate         = getProp "creationDate" props
    creatorName          = getProp "creatorName" props
    lastModificationDate = getProp "lastModificationDate" props
    lastModifierName     = getProp "lastModifierName" props
    originalVersion      = getProp "originalVersion" props
    version              = getProp "version" props
    versionComment       = getProp "versionComment" props
    user                 = getProp "user" props
    committer = if (lastModifierName /= "") then lastModifierName else user
  in    
    (committer,lastModificationDate,versionComment,lastModificationDate ++ version)
    -- (lastModifierName,"",lastModificationDate ++ version)
    
getVersionForOid
  :: (Ord k) =>
     Map.Map k (t, [Confluence]) -> k -> VersionInfo
getVersionForOid m oid =
  let
    (name, (Properties props:Collections collections:rest)) = m Map.! oid
  in  
    getVersionInfo props

-- ---------------------------------------------------------------------

renderOid
  :: Map.Map Integer ([Char], [Confluence])
     -> Integer
     -> Change
renderOid m oid = 
  let
    (name, (Properties props:Collections collections:rest)) = m Map.! oid
  in  
   --(oid,renderObject m name props collections rest)
   renderObject m name props collections rest

-- ---------------------------------------------------------------------

generateChange
  :: (Ord k) =>
     Map.Map Integer ([Char], [Confluence])
     -> Map.Map k [(t, t1, t2, Integer)]
     -> k
     -> (VersionInfo, [(ChangeType, String, String)])
generateChange m changes key =
  let
    changeset = changes Map.! key
    changeIds = map (\(_,_,_,oid) -> oid) changeset
    version = getVersionForOid m (head changeIds)
  in  
    (version,filter (\(chType,filename,body) -> filename /= "") $ map (renderOid m) changeIds)

-- ---------------------------------------------------------------------

extractPages cs = foldl' buildStructure (Map.empty,[]) $ processXml cs


doProcess :: [Content] -> [(VersionInfo,[(ChangeType, [Char], [Char])])]
doProcess cs = 
  let
    (m,pages) = extractPages cs
    pages' = filter (\oid -> livePage m oid) pages
    changes = foldl' groupByChangeSet Map.empty $ map (\(k,v) -> extractDates k v) $ Map.toList m
    res = filter (\(v,xs) -> xs /= []) $ map (generateChange m changes) $ Map.keys changes
  in  
    --map (renderPage m) $ map (\oid -> m Map.! oid) pages'
    --map (\oid -> m Map.! oid) pages'
    res
    
-- ---------------------------------------------------------------------

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
handleObject (_,oid,"Attachment",contents)              = ("Attachment",oid, collectNames contents)
handleObject (_,oid,"BodyContent",contents)             = ("BodyContent",oid, collectNames contents)
handleObject (_,oid,"BucketPropertySetItem",contents)   = ("BucketPropertySetItem",oid, collectNames contents)
handleObject (_,oid,"ConfluenceBandanaRecord",contents) = ("ConfluenceBandanaRecord",oid, collectNames contents)
handleObject (_,oid,"OutgoingLink",contents)            = ("OutgoingLink",oid, collectNames contents)
handleObject (_,oid,"Page",contents)                    = ("Page",oid, collectNames contents)
handleObject (_,oid,"Space",contents)                   = ("Space",oid, collectNames contents)
handleObject (_,oid,"SpacePermission",contents)         = ("SpacePermission",oid, collectNames contents)
handleObject (_,oid,"SpaceDescription",contents)        = ("SpaceDescription",oid, collectNames contents)
handleObject (_,oid,"Labelling",contents)               = ("Labelling",oid, collectNames contents)
handleObject (_,oid,"Label",contents)                   = ("Label",oid, collectNames contents)

handleObject (_,oid,name,contents)                      = ("***" ++ name,oid, collectNames contents)

-- ---------------------------------------------------------------------

collectNames :: [Confluence] -> [Confluence]
collectNames cs = 
  let
    (m,cm,r) = foldl' gather (Map.empty,Map.empty,[]) cs
  in
   (Properties m):(Collections cm):r
  where
    gather
      :: (Map.Map String [Confluence], Map.Map String [Confluence], [Confluence])
         -> Confluence
         -> (Map.Map String [Confluence], Map.Map String [Confluence], [Confluence])
    gather (m,cm,others) (Property name rest)   = (Map.insert name rest m,cm,others) 
    gather (m,cm,others) (Collection name rest) = (m,Map.insert name rest cm,others) 
    gather (m,cm,others) c                      = (m,                     cm,others++[c])     
    

    

-- =====================================================================
-- Testing support

parseXmlFile filename =
  do 
    x <- readFile (filename)
    let xml = parseXML x
    let (m,pages) = extractPages xml    
    return pages

x = parseXmlFile "tests/confluence-entities.xml"

t = readConfluenceFile "tests/confluence-entities.xml"

--z = readConfluenceZip "/home/alanz/tmp/PLAY1-203424-2.xml.zip"
z = readConfluenceZip "/home/alanz/tmp/PLAY1-215229-4.xml.zip"


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
tXml = parseXML _test_str2


_test_str2 :: [Char]
_test_str2 = 
  "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" ++
  "<hibernate-generic datetime=\"2011-03-07 16:45:43\">\n" ++
  "<object class=\"Attachment\" package=\"com.atlassian.confluence.pages\">\n" ++
  "<id name=\"id\">7077889</id>\n" ++
  "<property name=\"fileName\"><![CDATA[20091211238.jpg]]></property>\n" ++
  "<property name=\"contentType\"><![CDATA[image/jpeg]]></property>\n" ++
  "<property name=\"content\" class=\"Page\" package=\"com.atlassian.confluence.pages\"><id name=\"id\">6914050</id>\n" ++
  "</property>\n" ++
  "<property name=\"creatorName\"><![CDATA[alanz]]></property>\n" ++
  "<property name=\"creationDate\">2011-03-14 20:28:54.777</property>\n" ++
  "<property name=\"lastModifierName\"><![CDATA[alanz]]></property>\n" ++
  "<property name=\"lastModificationDate\">2011-03-14 20:28:54.777</property>\n" ++
  "<property name=\"fileSize\">642789</property>\n" ++
  "<property name=\"comment\"/><property name=\"attachmentVersion\">1</property>\n" ++
  "</object>\n" ++
  "</hibernate-generic>"


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

