module Types where

import           Universum

data Package = Package
    { name       :: String
    , directDeps :: [String]
    }
