module Geimskell.Options
  ( Options(..)
  , OptionSoundEngine(..)
  , OptionRendererType(..)
  , getOptions
  )
where

import Control.Monad
import System.Console.GetOpt
import System.Environment

data Options = Options { optionsSoundEngine :: OptionSoundEngine
                       , optionsRendererType :: OptionRendererType
                       }

data OptionSoundEngine = OptionSoundEngineNone
                       | OptionSoundEngineCsound

data OptionRendererType = OptionRendererSoftware
                        | OptionRendererHardware

type OptionsParser = Options -> Either String Options

usage :: String
usage = usageInfo "geimskell" options

soundEngineHelp :: String
soundEngineHelp = unwords $
  [ "Valid sound engines are 'none' and 'csound'"]

parseSoundEngine :: ArgDescr OptionsParser
parseSoundEngine = ReqArg
  (\ arg opts ->
     case arg of
       "csound" ->
         Right opts { optionsSoundEngine = OptionSoundEngineCsound }
       "none" ->
         Right opts { optionsSoundEngine = OptionSoundEngineNone }
       _ -> Left . unlines $
            [ usage
            , "Invalid argument for --sound-engine"
            , soundEngineHelp
            ]
  ) "SOUND_ENGINE"

parseRenderer :: ArgDescr OptionsParser
parseRenderer = ReqArg
  (\ arg opts ->
     case arg of
       "hardware" ->
         Right opts { optionsRendererType = OptionRendererHardware }
       "software" ->
         Right opts { optionsRendererType = OptionRendererSoftware }
       _ -> Left . unlines $
            [ usage
            , "Invalid argument for --renderer"
            , rendererHelp
            ]
  ) "RENDERER"

rendererHelp :: String
rendererHelp = "Valid Renderers are 'hardware' and 'software'"

defaultOptions :: Options
defaultOptions =
  Options { optionsSoundEngine = OptionSoundEngineCsound
          , optionsRendererType = OptionRendererSoftware
          }

options :: [OptDescr OptionsParser]
options =
  [ Option "s" ["sound-engine"] parseSoundEngine soundEngineHelp
  , Option "r" ["renderer"] parseRenderer rendererHelp
  ]

getOptions :: IO (Either String Options)
getOptions = do
  (args, _, errorMessages) <- getOpt RequireOrder options <$> getArgs
  if not $ null errorMessages
    then return . Left . unlines $ usage:errorMessages
    else return $ foldM applyTransformation defaultOptions args
  where
    applyTransformation = flip ($)
