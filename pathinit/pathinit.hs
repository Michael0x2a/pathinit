{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO
import Data.List
import Data.Char
import qualified Data.Map as Map
import Control.Applicative

import Data.Aeson
import Data.Aeson.Encode.Pretty
import GHC.Generics
import qualified Data.ByteString.Lazy as B

-- Raw strings and basic primitives

usageText :: String
usageText = "\n\
            \pathinit: Path init manager\n\
            \\n\
            \Usage:\n\
            \  pathinit add <new_path> <group>\n\
            \  pathinit remove <path> <group>\n\
            \  pathinit list all\n\
            \  pathinit list summary\n\
            \  pathinit list <group>\n\
            \  pathinit regenerate\n\
            \  pathinit help\n\
            \  pathinit -h|--help\n"

makeAbsolutePath :: FilePath -> IO FilePath
makeAbsolutePath relPath = do 
    absPath <- getExecutablePath
    return $ combine (takeDirectory absPath) relPath

-- Group Name (String wrapper)

newtype GroupName = GroupName String deriving (Ord, Eq, Generic)
instance FromJSON GroupName
instance ToJSON GroupName
instance Show GroupName where
    show (GroupName s) = show s

getGroupName :: GroupName -> String
getGroupName (GroupName s) = s


-- Path (String wrapper)

newtype Path = Path String deriving (Ord, Eq, Generic)
instance FromJSON Path
instance ToJSON Path
instance Show Path where
    show (Path s) = show s

getPath :: Path -> String
getPath (Path s) = s


-- Group JSON object

data Group =
    Group {
        name :: GroupName,
        paths :: [Path]
    } deriving (Show, Generic)

instance FromJSON Group
instance ToJSON Group


-- PathConfig JSON object

data PathConfig = 
    PathConfig {
        version :: Double,
        defaults :: [Path],
        groups :: [Group]
    } deriving (Show, Generic)

instance FromJSON PathConfig
instance ToJSON PathConfig

configPath :: IO FilePath
configPath = makeAbsolutePath "config.json"

getConfig :: IO PathConfig
getConfig = do
    extractedPath <- configPath
    fmap unwrap $ fmap decode' $ B.readFile extractedPath
    where
        unwrap :: (Maybe a) -> a
        unwrap a = case a of
            Just x -> x
            Nothing -> error "Could not parse config file"

-- Config file manipulation

getGroup :: PathConfig -> GroupName -> Group
getGroup config groupName = 
    case Map.lookup groupName groupNames of
        Just group -> group
        Nothing -> Group groupName [] 
    where
        groupNames :: Map.Map GroupName Group
        groupNames = Map.fromList $ map (\group -> (name group, group)) (groups config)  

addToGroup :: Path -> Group -> Group
addToGroup path (Group name paths) =
     Group name (path : paths)

addToConfig :: Group -> PathConfig -> PathConfig
addToConfig newGroup (PathConfig version defaults groups) = 
    PathConfig (version + 1) defaults newGroups
    where 
        newGroups :: [Group]
        newGroups = map replaceFunc groups

        replaceFunc :: Group -> Group
        replaceFunc oldGroup = 
            if (getName oldGroup) == (getName newGroup)
            then newGroup
            else oldGroup

        getName :: Group -> String
        getName group = (getGroupName (name group))

removeFromGroup :: Path -> Group -> Group
removeFromGroup path (Group name paths) =
    Group name (filter (\p -> p /= path) paths)

listGroup :: Group -> IO ()
listGroup (Group (GroupName name) paths) = do
    putStrLn $ " " ++ name ++ ":"
    sequence_ $ map (\(Path path) -> putStrLn ("     " ++ path)) paths
    putStrLn ""

listGroups :: PathConfig -> IO ()
listGroups (PathConfig version defaults groups) = do
    putStrLn $ "Version " ++ (show version)
    putStrLn ""
    sequence_ $ map listGroup groups

listSummary :: PathConfig -> IO ()
listSummary (PathConfig version defaults groups) = do
    putStrLn $ "Version " ++ (show version)
    putStrLn ""
    sequence_ $ map listNames groups
    where
        listNames :: Group -> IO ()
        listNames (Group (GroupName name) paths) = putStrLn name 


-- generate batch files

makeBatchName :: FilePath -> IO FilePath
makeBatchName name = makeAbsolutePath $ "init-" ++ name ++ ".ps1"

generateBatchFile :: Group -> IO ()
generateBatchFile (Group (GroupName name) paths) = do
    batchPath <- makeBatchName name
    writeFile batchPath contents
    where
        contents :: String
        contents = intercalate "\n" stringPaths 

        stringPaths :: [String]
        stringPaths = map formatPath paths

        formatPath :: Path -> String
        formatPath (Path path) = "$env:Path = $env:Path + \";" ++ path ++ "\"" 

generateBatchFiles :: PathConfig -> IO ()
generateBatchFiles (PathConfig version defaults groups) = do
    sequence_ $ map generateBatchFile groups


-- Loading

--loadGroup :: Group -> IO ()
--loadGroup (Group (GroupName groupName) paths) = do
--    batchName <- makeBatchName groupName
--    system batchName


-- Write out to files

bytesToString :: B.ByteString -> String
bytesToString bytes = map (chr . fromIntegral) $ B.unpack bytes

prettyPrint :: PathConfig -> String
prettyPrint config = bytesToString $ encodePretty config

writeConfigToFile :: PathConfig -> IO ()
writeConfigToFile config = do 
    extractedPath <- configPath
    -- Hack to force strict evaluation: 
    -- http://stackoverflow.com/a/2530948/646543
    length contents `seq` (writeFile extractedPath contents)
    where
        contents = prettyPrint config

modifyGroupAndSave :: PathConfig -> GroupName -> (Group -> Group) -> IO ()
modifyGroupAndSave config groupName changeFunc = do
    let group = getGroup config groupName
    let newGroup = changeFunc group
    let newConfig = addToConfig newGroup config
    writeConfigToFile newConfig
    generateBatchFiles newConfig


-- Commands

load :: PathConfig -> GroupName -> IO ()
load config group = 
    putStrLn $ "loading " ++ (getGroupName group)
    

add :: PathConfig -> GroupName -> Path -> IO ()
add config groupName newPath = do
    putStrLn $ "Adding " ++ (getPath newPath) ++ " to " ++ (getGroupName groupName)
    modifyGroupAndSave config groupName (addToGroup newPath)

remove :: PathConfig -> GroupName -> Path -> IO ()
remove config groupName path = do
    putStrLn $ "Removing " ++ (getPath path) ++ " from " ++ (getGroupName groupName)
    modifyGroupAndSave config groupName (removeFromGroup path)

list :: PathConfig -> GroupName -> IO ()
list config (GroupName "all") = listGroups config
list config (GroupName "summary") = listSummary config 
list config groupName = listGroup $ getGroup config groupName

regenerate :: PathConfig -> IO ()
regenerate config = do
    putStrLn $ "Generating ps1 files"
    generateBatchFiles config
    
usage :: IO ()
usage = putStrLn usageText

exit :: IO ()
exit = exitWith ExitSuccess


-- Parsing and execution

parse :: PathConfig -> [String] -> IO ()
--parse config ["load", group]           = load config (GroupName group)
parse config ["add", group, newPath]   = add config (GroupName group) (Path newPath)
parse config ["remove", group, path]   = remove config (GroupName group) (Path path)
parse config ["list", group]           = list config (GroupName group)
parse config ["regenerate"]            = regenerate config
parse config ["help"]                  = usage
parse config ["-h"]                    = usage
parse config ["--help"]                = usage
parse config _                         = die usageText

main :: IO ()
main = do 
    args <- getArgs
    config <- getConfig
    parse config args
    exit

