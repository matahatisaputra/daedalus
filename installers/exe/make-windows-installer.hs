import           Universum hiding (FilePath)
import           Turtle hiding ((<>))
import qualified Data.Text                        as T
import qualified Filesystem.Path.CurrentOS        as FP

import           Types
import           Config
import           WindowsInstaller

main :: IO ()
main = do
  buildNumber <- getAppVeyorBuildNumber
  options' <- options "Daedalus Windows installer generator" $ optionsParser Win64 buildNumber
  clusters <- getClusters
  forM_ clusters $ \cluster -> do
    putStrLn (banner cluster)
    export "NETWORK" (clusterNetwork cluster)
    makeWindowsInstaller (withCluster cluster options')
    cp "launcher-config.yaml" (win64yaml "launcher-config" cluster)
    cp "wallet-topology.yaml" (win64yaml "wallet-topology" cluster)

win64yaml :: Text -> Cluster -> FilePath
win64yaml name cluster = FP.fromText (format (s%"-"%s%".win64.yaml") name (lshowText cluster))

getAppVeyorBuildNumber :: IO (Maybe BuildJob)
getAppVeyorBuildNumber = parse <$> need "APPVEYOR_BUILD_NUMBER"
  where
    parse Nothing   = Nothing
    parse (Just "") = Nothing
    parse (Just n)  = Just . BuildJob $ n

getClusters :: IO [Cluster]
getClusters = maybe [] parse <$> need "CLUSTERS"
  where parse = catMaybes . map (diagReadCaseInsensitive . T.unpack) . T.splitOn " "

withCluster :: Cluster -> Options -> Options
withCluster cluster o = o { oCluster = cluster
                          , oAppName = clusterAppName cluster
                          }

clusterAppName :: Cluster -> AppName
clusterAppName Mainnet = "Daedalus"
clusterAppName Staging = "DaedalusStaging"
clusterAppName Testnet = "DaedalusTestnet"

banner :: Cluster -> Text
banner c = unlines
  [ "##############################################################################"
  , "###"
  , "### Building for cluster " <> show c
  , "###"
  , "##############################################################################"
  ]
