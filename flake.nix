{
  description = "PACT-code";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-21.05";

    gitignore-hercules-src = {
      url = "github:hercules-ci/gitignore.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    gitignore-src = {
      url = "github:siers/nix-gitignore/4f2d85f2f1aa4c6bff2d9fcfd3caad443f35476e";
      flake = false;
    };

    validity-src = {
      url = "github:NorfairKing/validity/bafcaf3799fa76edd5968923f6efde25085815f9";
      flake = false;
    };

    sydtest-src = {
      url = "github:NorfairKing/sydtest/9a4c6040db470a88172d36ffa8ce3e466b3cd827";
      flake = false;
    };

    safe-coloured-text-src = {
      url = "github:NorfairKing/safe-coloured-text";
      flake = false;
    };

    typed-uuid-src = {
      url = "github:NorfairKing/typed-uuid";
      flake = false;
    };

    yamlparse-applicative-src = {
      url = "github:NorfairKing/yamlparse-applicative";
      flake = false;
    };

    yesod-autoreload-src = {
      url = "github:NorfairKing/yesod-autoreload";
      flake = false;
    };

    flake-compat-ci.url = "github:hercules-ci/flake-compat-ci";

    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, gitignore-hercules-src, gitignore-src, validity-src, sydtest-src, safe-coloured-text-src, typed-uuid-src, yamlparse-applicative-src, yesod-autoreload-src, flake-compat-ci, flake-compat }@inputs:
    let
      # Generate a user-friendly version number.
      version = builtins.substring 0 8 self.lastModifiedDate;

      # System types to support.
      supportedSystems = [ "x86_64-linux" ];

      # Helper function to generate an attrset '{ x86_64-linux = f "x86_64-linux"; ... }'.
      forAllSystems = f: nixpkgs.lib.genAttrs supportedSystems (system: f system);

      # Nixpkgs instantiated for supported system types.
      nixpkgsFor = forAllSystems (system: import nixpkgs { inherit system; overlays = [ self.overlay ]; });

      # The GHC compiler version to use, from haskell.packages.<compiler>
      compiler = "ghc8104";
      # A list of files to ignore when gitignoreSource is used like:
      # gitignoreSource ignorance <directory>
      ignorance = [
        "*.md"
        "*.adoc"
        "intro"
        "*.nix"
        "*.sh"
        "*.yml"
      ];

    in
    {
      overlay = final: prev:
        let
          gitignoreSource = (final.callPackage gitignore-src { }).gitignoreSource;
        in
        {
          haskellPackages = prev.haskellPackages // {
            inherit (self.packages.x86_64-linux)
              yesod-autoreload
              pact-web-server
              pact-db;
          };

        };

      defaultPackage = forAllSystems (system: self.packages.${system}.pact-web-server);

      packages = forAllSystems (system:
        let
          pkgs = import nixpkgs {
            inherit system; overlays =
            [
              (final: prev: {
                haskellPackages = final.haskell.packages.${compiler}.override {
                  overrides = hself: hsuper: {
                    yesod-autoreload = (hself.callCabal2nix "yesod-autoreload" (gitignoreSource ignorance yesod-autoreload-src) { });
                    pact-web-server = (hself.callCabal2nix "pact-web-server" (gitignoreSource ignorance ./pact-web-server) { });
                    pact-db = hself.callCabal2nix "pact-db" (gitignoreSource ignorance ./pact-db) { };
                  };
                };
              })

              # Syd's overlay assumes that it will be given haskellPackages
              # overrides already in place. These, nor haskellPackages, cannot
              # be overriden afterwards without threading Syd's changes.
              # https://github.com/NorfairKing/sydtest/blob/5b0eee208753e3554d9b158a6e48b1760514aed0/nix/overlay.nix#L89
              (import "${sydtest-src}/nix/overlay.nix")
              (import "${validity-src}/nix/overlay.nix")
              (import "${safe-coloured-text-src}/nix/overlay.nix")
              (final: prev: { inherit (gitignore-hercules-src.lib) gitignoreSource; })
            ];
          };
          gitignoreSource = (pkgs.callPackage gitignore-src { }).gitignoreSource;
          haskellPackages = pkgs.haskellPackages;

        in
        rec {
          inherit (haskellPackages)
            pact-web-server
            pact-db;
        });


      devShell = forAllSystems (system: self.devShells.${system}.default);

      # devShells

      ciNix = flake-compat-ci.lib.recurseIntoFlakeWith {
        flake = self;
        systems = [ "x86_64-linux" ];
      };

      # apps = 

      # checks = 

      nixosModules.pact-web-server = { pkgs, lib, config, ... }:
        {
          options.services.pactServer = {
            enable = lib.mkEnableOption
              "Enable PACT server";
            package = lib.mkOption {
              default = pkgs.haskellPackages.pact-web-server;
              defaultText = "pkgs.haskellPackages.pact-web-server";
              type = lib.types.package;
              description = "PACT web server package to use";
            };
            port = lib.mkOption {
              default = 8080;
              defaultText = "8080";
              type = lib.types.port;
              description = "Port to run on";
            };
          };
          config =
            let
              cfg = config.services.danaswapstats;
            in
            lib.mkIf cfg.enable {
              nixpkgs.overlays = [ self.overlay ];
              systemd.services.pact-web-server = {
                description = "The PACT web server service";
                wantedBy = [ "multi-user.target" ];
                after = [ "networking.target" ];
                serviceConfig = {
                  DynamicUser = "true";
                  User = "pact-web-server";
                  ExecStart =
                    "${pkgs.haskellPackages.pact-web-server}/bin/pact-web-server --port ${toString cfg.port}";
                  PrivateTmp = true;
                  Restart = "always";
                };
              };
            };
        };

    };
}
