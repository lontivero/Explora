# shell.nix

with import /home/lontivero/Projects/nixpkgs { config.allowUnfree = true; };
let
  dotnet-fable = pkgs.buildDotnetGlobalTool {
    pname = "fable";
    nugetName = "fable";
    version = "4.24.0";
    nugetSha256 = "sha256-ERewWqfEyyZKpHFFALpMGJT0fDWywBYY5buU/wTZZTg=";
    dotnet-sdk = pkgs.dotnet-sdk_8;
  };

  DOTNET_NOLOGO=1;
  DOTNET_CLI_TELEMETRY_OPTOUT=1;
  DOTNET_ROOT = "${dotnet-sdk_8}";
  DOTNET_GLOBAL_TOOLS_PATH = "${builtins.getEnv "HOME"}/.dotnet/tools";
in
mkShell {
  name = "Explora-Shell";
  packages = [
    dotnet-sdk_8
    nodejs_20
    dotnet-fable
    jetbrains.rider
  ];


  shellHook = ''
    export PATH="$PATH:$DOTNET_GLOBAL_TOOLS_PATH"
    export PS1='\n\[\033[1;34m\][Explora:\w]\$\[\033[0m\] '
  '';
}
