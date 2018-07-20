import           Universum hiding (FilePath)
import           Turtle hiding ((<>))

import           Types
import           Config
import           WindowsInstaller

main :: IO ()
main = do
  -- buildNumber <- getAppVeyorBuildNumber
  cmd <- options "Daedalus Windows installer generator" windowsInstallerParser
  windowsInstallerMain cmd

windowsInstallerParser :: Parser WindowsInstaller
windowsInstallerParser =
  subcommand "make-nsi" "Generate NullSoft Installer scripts" (MakeNSI <$> makeNSI)
  <|> subcommand "build" "Build and sign installer from scripts" (BuildInstaller <$> buildInstaller)
  where
    makeNSI = MakeNSIOptions <$> optPath "output-dir" 'o' "Output directoy"
              <*> optPath "dhall-dir" 'd' "Dhall config location"
              <*> optPath "filename" 'n' "Installer filename"
              <*> (Version <$> optText "version" 'v' "Application version")
              <*> clusterParser
    buildInstaller = BuildInstallerOptions <$> optPath "filename" 'n' "Output filename"

{-
withCluster :: Cluster -> Options -> Options
withCluster cluster o = o { oCluster = cluster
                          , oAppName = clusterAppName cluster
                          }

clusterAppName :: Cluster -> AppName
clusterAppName Mainnet = "Daedalus"
clusterAppName Staging = "DaedalusStaging"
clusterAppName Testnet = "DaedalusTestnet"
-}
