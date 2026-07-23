{
  description = "csharp-ls development environment";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  outputs =
    {
      self,
      nixpkgs,
      ...
    }:
    let
      systems = [
        "x86_64-linux"
        "aarch64-linux"
        "aarch64-darwin"
      ];
      forAllSystems = nixpkgs.lib.genAttrs systems;

      dotnetSdkAttribute =
        let
          sdkVersion = (builtins.fromJSON (builtins.readFile ./global.json)).sdk.version;
          versionParts = nixpkgs.lib.splitString "." sdkVersion;
          featureBand = builtins.substring 0 1 (builtins.elemAt versionParts 2);
        in
        "sdk_${builtins.elemAt versionParts 0}_${builtins.elemAt versionParts 1}_${featureBand}xx";

      mkDotnetSdk =
        system:
        let
          pkgs = import nixpkgs { inherit system; };
        in
        pkgs.dotnetCorePackages.${dotnetSdkAttribute};

      mkUpdateDeps =
        system:
        let
          pkgs = import nixpkgs { inherit system; };
        in
        pkgs.writeShellApplication {
          name = "csharp-ls-update-deps";
          runtimeInputs = [
            pkgs.git
            pkgs.nix
          ];
          text = ''
            set -euo pipefail

            repo_root=$(git rev-parse --show-toplevel)
            cd "$repo_root"
            fetch_deps=$(nix build --no-link --no-write-lock-file --print-out-paths \
              path:.#csharp-ls.passthru.fetch-deps)
            "$fetch_deps" nix/deps.json
          '';
        };

      mkCsharpLs =
        system:
        let
          pkgs = import nixpkgs { inherit system; };
          inherit (pkgs) lib;
          sdk = mkDotnetSdk system;
        in
        pkgs.buildDotnetModule {
          __structuredAttrs = true;
          pname = "csharp-ls";
          version = "0.26.0";

          src = lib.fileset.toSource {
            root = ./.;
            fileset = lib.fileset.unions [
              ./CHANGELOG.md
              ./Directory.Build.props
              ./Directory.Build.targets
              ./Directory.Packages.props
              ./README.md
              ./global.json
              ./nuget.config
              ./src/CSharpLanguageServer
              ./src/Ionide.LanguageServerProtocol
            ];
          };

          projectFile = "src/CSharpLanguageServer/CSharpLanguageServer.fsproj";
          nugetDeps = ./nix/deps.json;
          dotnet-sdk = sdk;
          dotnet-runtime = sdk;
          executables = [ "CSharpLanguageServer" ];

          postFixup = ''
            ln -s CSharpLanguageServer "$out/bin/csharp-ls"
          '';

          meta = {
            description = "C# LSP language server";
            homepage = "https://github.com/razzmatazz/csharp-language-server";
            license = lib.licenses.mit;
            mainProgram = "csharp-ls";
            platforms = systems;
          };
        };
    in
    {
      packages = forAllSystems (system: {
        default = mkCsharpLs system;
        csharp-ls = self.packages.${system}.default;
      });

      apps = forAllSystems (system: {
        default = self.apps.${system}.csharp-ls;
        csharp-ls = {
          type = "app";
          program = "${self.packages.${system}.default}/bin/csharp-ls";
          meta = self.packages.${system}.default.meta;
        };
        update-deps = {
          type = "app";
          program = "${mkUpdateDeps system}/bin/csharp-ls-update-deps";
          meta.description = "Update the Nix NuGet dependency lock";
        };
      });

      devShells = forAllSystems (
        system:
        let
          pkgs = import nixpkgs { inherit system; };
        in
        {
          default = pkgs.mkShellNoCC {
            packages = [
              (mkDotnetSdk system)
              pkgs.fantomas
              pkgs.git
              pkgs.nixfmt
            ];

            DOTNET_CLI_TELEMETRY_OPTOUT = "1";
            DOTNET_NOLOGO = "1";
            NUGET_XMLDOC_MODE = "skip";
          };
        }
      );

      formatter = forAllSystems (system: (import nixpkgs { inherit system; }).nixfmt);

      checks = forAllSystems (system: {
        package = self.packages.${system}.default;
      });
    };
}
