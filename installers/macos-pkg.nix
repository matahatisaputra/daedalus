{ lib, stdenv, runCommand, binutils
, writeText, writeScript, writeScriptBin
, frontend
, daedalus
, electronBinaries
, network
, daedalus-installer
, version
}:

#
# An overview of Mac .pkg internals:
#    http://www.peachpit.com/articles/article.aspx?p=605381&seqNum=2
#

let
  nameSuffix = if network == "mainnet" then "" else " ${network}";
  appName = "Daedalus${nameSuffix}";
  appNameApp = "${appName}.app";
  pkgIdentifier = "org.${appName}.pkg";

  pkgbuild = runCommand "pkgbuild" {} ''
    mkdir -p $out/bin
    ln -s /usr/bin/pkgbuild $out/bin
    ln -s /usr/bin/productbuild $out/bin
  '';

  iconutil = runCommand "iconutil" {} ''
    mkdir -p $out/bin
    ln -s /usr/bin/iconutil $out/bin
  '';

  launcherFile = let
    dataDir = "$HOME/Library/Application Support/${appName}";
  in
    writeScript "Frontend" '' 
      #!/usr/bin/env bash
      cd "$(dirname "$0")"
      mkdir -p "${dataDir}/Secrets-1.0" "${dataDir}/Logs/pub"
      export NETWORK=${network}
      export REPORT_URL="fixme"
      ./cardano-launcher
    '';

  icons = stdenv.mkDerivation {
    name = "electron.icns";
    buildInputs = [ iconutil ];
    buildCommand = "iconutil --convert icns --output $out ./icons/electron.iconset";
  };

  scripts = runCommand "scripts" {} ''
    mkdir -p $out
    cp ${./data/scripts/dockutil} $out/dockutil
    cp ${postInstallScript} $out/postinstall
    chmod 755 $out/dockutil $out/postinstall
  '';

  postInstallScript = writeScript "postinstall" ''
    #!/usr/bin/env bash
    # See /var/log/install.log to debug this
    src_pkg="$1"
    dst_root="$2"
    dst_mount="$3"
    sys_root="$4"
    ./dockutil --add "$dst_root" ${appName} --allhomes
  '';

  plist = writeText "Info.plist" ''
    <?xml version="1.0" encoding="UTF-8"?>
    <!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0
    .dtd">
    <plist version="1.0">
      <dict>
        <key>CFBundleDisplayName</key>
        <string>${appName}</string>
        <key>CFBundleExecutable</key>
        <string>${appName}</string>
        <key>CFBundleIconFile</key>
        <string>electron.icns</string>
        <key>CFBundleIdentifier</key>
        <string>${pkgIdentifier}</string>
        <key>CFBundleInfoDictionaryVersion</key>
        <string>6.0</string>
        <key>CFBundleName</key>
        <string>${appName}</string>
        <key>CFBundlePackageType</key>
        <string>APPL</string>
        <key>CFBundleShortVersionString</key>
        <string>${version}</string>
        <key>CFBundleVersion</key>
        <string>${version}</string>
        <key>DTSDKBuild</key>
        <string>14D125</string>
        <key>DTSDKName</key>
        <string>macosx10.1010.10</string>
        <key>DTXcode</key>
        <string>0821</string>
        <key>DTXcodeBuild</key>
        <string>8C1002</string>
        <key>LSApplicationCategoryType</key>
        <string>public.app-category.developer-tools</string>
        <key>LSMinimumSystemVersion</key>
        <string>10.9.0</string>
        <key>NSHighResolutionCapable</key>
        <true/>
        <key>NSMainNibFile</key>
        <string>MainMenu</string>
        <key>NSPrincipalClass</key>
        <string>AtomApplication</string>
        <key>NSSupportsAutomaticGraphicsSwitching</key>
        <true/>
      </dict>
    </plist>
  '';

  macosApp = stdenv.mkDerivation {
    name = lib.replaceStrings [" "] ["-"] appNameApp;
    buildInputs = [ daedalus-installer binutils ];
    buildCommand = ''
      mkdir -p $out

      # Package is based on Electron.app
      cp -R ${electronBinaries}/Electron.app "$out/${appNameApp}"
      chmod -R +w $out

      contents="$out/${appNameApp}/Contents"
      macos="$contents/MacOS"

      # add daedalus, cardano, configs
      cp -R --dereference ${daedalus}/libexec/* ${daedalus}/usr/share/daedalus/* "$macos"
      chmod -R +w "$macos"
      rm -rf "$macos/daedalus-frontend" "$macos/update-runner"

      # add the frontend
      cp -R ${frontend}/share/daedalus "$contents/Resources/app"

      # Replace Info.plist with one for Daedalus
      cp ${plist} "$contents/Info.plist"

      # Bundle dylibs
      darwin-rewrite-libs -o "$macos"          \
        "$macos/cardano-launcher"              \
        "$macos/cardano-node"                  \
        "$macos/cardano-x509-certificates"

      cp ${launcherFile} "$macos/Frontend"

      chmod -R +w $out
    '';
  };

  macosPackage = stdenv.mkDerivation {
    name = "${daedalus.name}.pkg";
    inherit version;
    passthru = {
      inherit network;
      unpacked = macosApp;
    };
    buildInputs = [ pkgbuild ];
    buildCommand = ''
      pkgbuild \
        --identifier "${pkgIdentifier}" \
        --scripts ${scripts} \
        --install-location /Applications \
        --component "${macosApp}/${appNameApp}" \
        daedalus.pkg

      productbuild \
        --product ${./data}/plist \
        --package daedalus.pkg $out
    '';
  };

in
  macosPackage
