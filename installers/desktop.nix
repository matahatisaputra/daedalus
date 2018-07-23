{ runCommand, daedalus, desktopItem, network }:

let
  icon = ./icons/1024x1024.png;

in
  daedalus.overrideAttrs (oldAttrs: {
    passthru = { inherit network; };
    buildCommand = ''
      ${oldAttrs.buildCommand}
      mkdir -p $out/share/icons/hicolor/1024x1024/apps
      ln -s ${desktopItem}/share/applications $out/share
      ln -s ${icon} $out/share/icons/hicolor/1024x1024/apps/daedalus.png
    '';
  })
