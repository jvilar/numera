{-# LANGUAGE TemplateHaskell #-}

import Control.Lens(Getting, makeLenses, set, (^.))
import Control.Monad(forM_, guard, mapM, unless, when)
import Data.Char(isDigit)
import Shh
import System.Console.JMVOptions
import System.Environment(getArgs, getProgName)
import System.Exit(exitSuccess, exitFailure)
import System.FilePath.Glob(glob)
import System.FilePath.Posix(hasExtension, dropExtension, splitExtension, takeExtension, (</>))
import System.IO(hPutStrLn, stderr)


$(loadExe Absolute "mv")

data Action = Number | Undo

data Options = Options { _help :: Bool
                       , _dry_run :: Bool
                       , _everyThing :: Bool
                       , _action :: Action
                       , _files :: [FilePath]
                       }

makeLenses ''Options

defaultOptions = Options { _help = False
                         , _dry_run = False
                         , _everyThing = False
                         , _action = Number
                         , _files = []
                         }

usageHeader :: String
usageHeader = "numera\n\
              \======\n\
              \Adds number to file names before the extension.\n\
              \\n"

showDef :: Show a => Getting a Options a -> String
showDef field = "Por defecto: " ++ show (defaultOptions ^. field) ++ "."

options :: [OptDescr (Options -> Options)]
options = processOptions $ do
  'h' ~: "help" ==> NoArg (set help True) ~: "Esta ayuda."
  'n' ~: "dry-run" ==> NoArg (set dry_run True) ~: "Solo muestra qué renombraría, pero no lo hace."
  't' ~: "todo" ==> NoArg (set everyThing True) ~: "Renombra todo. (Por defecto, solo los ficheros sin un número delante de la extensión)"
  'u' ~: "undo" ==> NoArg (set action Undo) ~: "Elimina el número del fichero (si lo tiene y no existe el fichero sin él)"

getOptions :: IO Options
getOptions = do
               args <- getArgs
               let (o, a, e) =  getOpt Permute options args
               let opt = foldl (flip id) defaultOptions o
               when (opt ^. help) $ putStrLn helpMessage >> exitSuccess
               unless (null e) $ myError $ concat e ++ helpMessage
               unless (not $ null a) $ myError "Necesito al menos un nombre de fichero"
               return $ set files a opt

helpMessage :: String
helpMessage = usageInfo usageHeader options

myError :: String -> IO a
myError m = do
              n <- getProgName
              hPutStrLn stderr $ n ++ " error: " ++ m
              exitFailure

currentNumber :: FilePath -> Int
currentNumber = read . tail . takeExtension . dropExtension

hasNumber :: FilePath -> Bool
hasNumber fp = let
  (path, ext) = splitExtension fp
  n = tail $ takeExtension path
  in hasExtension fp && hasExtension path && not (null n) && all isDigit n

mkGlob :: (FilePath, String) -> String
mkGlob (path, ext) = path ++ ".<->" ++ ext

addNumber :: Int -> (FilePath, String) -> FilePath
addNumber n (path, ext) = concat [path, ".", show n, ext]

takeOutNumber :: FilePath -> FilePath
takeOutNumber fp = let
  (fp2, ext) = splitExtension fp
  in dropExtension fp2 ++ ext

generateNumber :: FilePath -> IO FilePath
generateNumber fp = do
  let pe = splitExtension fp
  existing <- map currentNumber <$> glob (mkGlob pe)
  let n = if null existing
          then 1
          else maximum existing + 1
  return $ addNumber n pe

move :: Options -> FilePath -> FilePath -> IO ()
move opts fp fp2 = if opts ^. dry_run
    then putStrLn $ concat [fp, " -> ", fp2]
    else case opts ^. action of
           Number -> mv fp fp2
           Undo -> mv "-n" fp fp2

process :: Action -> Options -> FilePath -> IO ()
process Number opts fp = when (opts ^. everyThing || not (hasNumber fp)) $
   generateNumber fp >>= move opts fp
process Undo opts fp = when (hasNumber fp) $ move opts fp $ takeOutNumber fp

main = do
  opts <- getOptions
  forM_ (opts ^. files) $ process (opts ^. action) opts
