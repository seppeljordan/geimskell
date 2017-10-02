module Geimskell.Options where

import Control.Monad
import Control.Monad.Trans.Except
import System.Console.GetOpt
import System.Environment

data OptionSoundEngine = OptionSoundEngineNone
                       | OptionSoundEngineCsound

data OptionRendererType = OptionRendererSoftware
                        | OptionRendererHardware

usage = usageInfo "geimskell" options

soundEngineHelp = unwords $
  [ "Valid sound engines are 'none' and 'csound'"]

parseSoundEngine ::
  ArgDescr (Options -> ExceptT String IO Options)
parseSoundEngine = ReqArg
  (\ arg opts ->
     case arg of
       "csound" ->
         return opts { optionsSoundEngine = OptionSoundEngineCsound }
       "none" ->
         return opts { optionsSoundEngine = OptionSoundEngineNone }
       _ -> throwE . unlines $
            [ usage
            , "Invalid argument for --sound-engine"
            , soundEngineHelp
            ]
  ) "SOUND_ENGINE"

parseRenderer ::
  ArgDescr (Options -> ExceptT String IO Options)
parseRenderer = ReqArg
  (\ arg opts ->
     case arg of
       "hardware" ->
         return opts { optionsRendererType = OptionRendererHardware }
       "software" ->
         return opts { optionsRendererType = OptionRendererSoftware }
       _ -> throwE . unlines $
            [ usage
            , "Invalid argument for --renderer"
            , rendererHelp
            ]
  ) "RENDERER"

rendererHelp = "Valid Renderers are 'hardware' and 'software'"

data Options = Options { optionsSoundEngine :: OptionSoundEngine
                       , optionsRendererType :: OptionRendererType
                       }

defaultOptions =
  Options { optionsSoundEngine = OptionSoundEngineCsound
          , optionsRendererType = OptionRendererSoftware
          }

options =
  [ Option "s" ["sound-engine"] parseSoundEngine soundEngineHelp
  , Option "r" ["renderer"] parseRenderer rendererHelp
  ]

getOptions :: IO (Either String Options)
getOptions = do
  (args, _, errorMessages) <- getOpt RequireOrder options <$> getArgs
  if not $ null errorMessages
    then return . Left . unlines $ usage:errorMessages
    else runExceptT $ foldM applyTransformation defaultOptions args
  where
    applyTransformation :: Options
                        -> (Options -> ExceptT String IO Options)
                        -> ExceptT String IO Options
    applyTransformation = flip ($)
