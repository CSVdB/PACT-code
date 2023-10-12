{
  description = "PACT-code";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

    gitignore-hercules-src = {
      url = "github:hercules-ci/gitignore.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    gitignore-src = {
      url = "github:siers/nix-gitignore/4f2d85f2f1aa4c6bff2d9fcfd3caad443f35476e";
      flake = false;
    };

    validity-src = {
      url = "github:NorfairKing/validity/d88be911a7e2a84f6c089e9269aaed8d10a74acd";
      flake = false;
    };

    sydtest-src = {
      url = "github:NorfairKing/sydtest";
      flake = false;
    };

    safe-coloured-text-src = {
      url = "github:NorfairKing/safe-coloured-text";
      flake = false;
    };

    autodocodec-src = {
      url = "github:NorfairKing/autodocodec";
      flake = false;
    };

    typed-uuid-src = {
      url = "github:NorfairKing/typed-uuid";
      flake = false;
    };

    yesod-autoreload-src = {
      url = "github:NorfairKing/yesod-autoreload";
      flake = false;
    };

    flake-compat-ci = {
      url = "github:hercules-ci/flake-compat-ci";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, gitignore-hercules-src, gitignore-src, validity-src, sydtest-src, safe-coloured-text-src, autodocodec-src, typed-uuid-src, yesod-autoreload-src, flake-compat-ci, flake-compat }@inputs:
    let
      # Generate a user-friendly version number.
      version = builtins.substring 0 8 self.lastModifiedDate;

      # System types to support.
      supportedSystems = [ "x86_64-linux" ];

      # Helper function to generate an attrset '{ x86_64-linux = f "x86_64-linux"; ... }'.
      forAllSystems = f: nixpkgs.lib.genAttrs supportedSystems (system: f system);

      # Nixpkgs instantiated for supported system types.
      nixpkgsFor = forAllSystems (system: import nixpkgs { inherit system; overlays = [ self.overlays.default ]; });

      # The GHC compiler version to use, from haskell.packages.<compiler>
      compiler = "ghc902";
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

      ciNix = flake-compat-ci.lib.recurseIntoFlakeWith {
        flake = self;
        systems = [ "x86_64-linux" ];
      };

      linters = forAllSystems (system:
        let
          pkgs = nixpkgsFor."${system}";
        in
        {
          haskell = import ./nix/linters.nix { inherit pkgs; };
        });

    in
    {
      overlays.default = final: prev:
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

      packages = forAllSystems (system:
        let
          pkgs = import nixpkgs {
            inherit system; config.allowUnfree = true;
            overlays =
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
                # overrides already in place. These nor haskellPackages can be
                # overriden afterwards without threading Syd's changes.
                # https://github.com/NorfairKing/sydtest/blob/5b0eee208753e3554d9b158a6e48b1760514aed0/nix/overlay.nix#L89
                (import "${sydtest-src}/nix/overlay.nix")
                (import "${validity-src}/nix/overlay.nix")
                (import "${safe-coloured-text-src}/nix/overlay.nix")
                (import "${autodocodec-src}/nix/overlay.nix")
                (import "${typed-uuid-src}/nix/overlay.nix")
                (final: prev: { inherit (gitignore-hercules-src.lib) gitignoreSource; })
              ];
          };
          gitignoreSource = (pkgs.callPackage gitignore-src { }).gitignoreSource;
          haskellPackages = pkgs.haskellPackages;

        in
        rec {
          default = pact-web-server;
          inherit (haskellPackages)
            pact-web-server
            pact-db;
        });


      devShells = forAllSystems (system:
        let
          pkgs = nixpkgsFor."${system}";

          hoogle = pkgs.buildEnv {
            name = "hoogle";
            paths = [ (pkgs.haskellPackages.ghcWithHoogle (_: pkgs.lib.attrValues self.packages.${system})) ];
          };

        in
        rec
        {
          default = pkgs.haskell.packages.${compiler}.shellFor {
            # packages = _: with pkgs.haskell.pkgs.${system}; [ ];
            packages = _: with self.packages.${system}; [ ];
            buildInputs = with pkgs; [
              haskellPackages.ormolu
              haskellPackages.hlint
              haskellPackages.hpc
              nixpkgs-fmt
              shellcheck
              hoogle
              haskellPackages.autoexporter
              zlib
            ];
            COMPILER = compiler;
          };
        });

      apps = forAllSystems (system:
        let
          pkgs = nixpkgsFor."${system}";
        in
        {
          lint-haskell = {
            type = "app";
            program = "${linters.${system}.haskell.lintScript}/bin/lint";
          };
        });

      checks = forAllSystems (system:
        with nixpkgsFor.${system};
        lib.optionalAttrs stdenv.isLinux {
          haskellLint = linters.${system}.haskell.lintDerivation ./.;
        });

      nixosModules.pact-web-server = { pkgs, lib, config, ... }:
        {
          options.services.pact-web-server = {
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
            artifacts_dir = lib.mkOption {
              default = "./";
              defaultText = "./";
              type = lib.types.path;
              description = "path where the SQLite dB and session key file are stored";
            };
            hosts = lib.mkOption {
              type = lib.types.listOf (lib.types.str);
              example = "pactcommunity.be";
              default = [ ];
              description = "The host to serve web requests on";
            };
          };
          config =
            let
              cfg = config.services.pact-web-server;

              mergeListRecursively = pkgs.callPackage ./nix/merge-lists-recursively.nix { };

              web-server-host = with cfg; {
                "${head hosts}" =
                  {
                    enableACME = true;
                    forceSSL = true;
                    locations."/" = {
                      proxyPass = "http://localhost:${builtins.toString port}";
                      # To make the websockets api work
                      proxyWebsockets = true;
                      # Just to make sure we don't run into 413 errors on big syncs
                      extraConfig = ''
                        client_max_body_size 0;
                      '';
                    };
                    serverAliases = tail hosts;
                  };
              };
            in
            lib.mkIf cfg.enable {
              nixpkgs.overlays = [ self.overlays.default ];

              services.nginx.virtualHosts =
                mergeListRecursively [
                  web-server-host
                ];

              networking.firewall.allowedTCPPorts = [ cfg.port ];

              systemd.services.pact-web-server = {
                description = "The PACT web server service";
                wantedBy = [ "multi-user.target" ];
                after = [ "networking.target" ];
                serviceConfig = {
                  ExecStart =
                    ''
                      ${pkgs.haskellPackages.pact-web-server}/bin/pact-web-server --port ${toString cfg.port} --artifacts_dir ${cfg.artifacts_dir}
                    '';
                  PrivateTmp = true;
                  Restart = "always";
                };
              };
            };
        };

    };
}
