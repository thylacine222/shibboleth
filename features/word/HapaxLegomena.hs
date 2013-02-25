--- Words.hs
--- Generate word stylometric features

import Features (HapaxLegomena)
import Data.Char (isAlphaNum)
import Control.Monad (=<<)
import qualified Data.Set as Set


generateHapaxLegomena :: String -> Set Feature
generateHapaxLegomena text = Set.fromList $
                                map HapaxLegomena $ 
                                singletons $ 
                                (=<<) words $
                                lines $
                                filter (isAlphaNum) $ 
                                map toLower text
    where
        singletons list = concat $
                            filter (\x -> length x == 1) $
                            group $
                            sort list
