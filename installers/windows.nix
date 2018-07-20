{ lib, stdenv, runCommand, binutils
, writeText, writeScript, writeScriptBin, fetchzip
, frontend
, daedalus
, electronBinaries
, network
, daedalus-bridge
, daedalus-installer
, daedalus-config
, dhall
, version
}:

let
  installerFileName = "daedalus-${version}-cardano-sl--mainnet-windows.exe";

  installScripts = runCommand "nsi-files" {} ''
    mkdir -p $out
    cd $out
    ${daedalus-installer}/bin/make-windows-installer make-nsi \
        --output-dir $out \
        --dhall-dir ${dhall} \
        --filename ${installerFileName} \
        --version ${version} \
        --cluster ${network}
  '';

  launcherConfig = daedalus-config.override { system = "x86_64-mingw"; };
  electron = electronBinaries.override { system = "x86_64-mingw"; };

  buildBat = writeText "build.bat" ''
    make-windows-installer build --filename ${installerFileName}
  '';

  dlls = fetchzip {
    url = "https://s3.eu-central-1.amazonaws.com/daedalus-ci-binaries/DLLs.zip";
    sha256 = "1p33n5gmiqhj481l774x8hyl047zv5ybzr9h0qpcz68419wi2pih";
    stripRoot = false;
  };

  makeWindowsInstaller = stdenv.mkDerivation {
    name = "make-windows-installer";
    buildCommand = ''
      mkdir -p $out
      cp ${daedalus-installer}/bin/make-windows-installer* $out
      cp ${daedalus-bridge}/bin/cardano-node* ${daedalus-bridge}/bin/cardano-launcher* $out
      cp ${launcherConfig}/* $out

      cp -RL ${dlls} $out/dlls
      cp -RL ${electronBinaries} $out/electron
      cp -RL ${frontend}/share/daedalus $out/frontend

      cp ${installScripts}/* $out
      cp ${buildBat} $out/build.bat
    '';
  };

in
  makeWindowsInstaller
