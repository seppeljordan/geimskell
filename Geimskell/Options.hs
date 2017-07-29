module Geimskell.Options where

import Control.Monad
import Control.Monad.Trans.Except
import System.Console.GetOpt
import System.Environment

data OptionSoundEngine = OptionSoundEngineNone
                       | OptionSoundEngineCsound

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

data Options = Options { optionsSoundEngine :: OptionSoundEngine }

defaultOptions =
  Options { optionsSoundEngine = OptionSoundEngineCsound }

options =
  [ Option "s" ["sound-engine"] parseSoundEngine soundEngineHelp ]

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
