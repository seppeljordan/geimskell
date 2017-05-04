module Sound where

import System.Process

shootSound = callCommand "csound -odac osc_send.csd"
