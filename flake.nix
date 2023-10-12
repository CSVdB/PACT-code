{
  description = "PACT-code";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-23.05";

    fast-myers-diff = {
      url = "github:NorfairKing/fast-myers-diff";
      inputs.nixpkgs.follows = "nixpkgs";
      flake = false;
    };

    validity = {
      url = "github:NorfairKing/validity";
      inputs.nixpkgs.follows = "nixpkgs";
      flake = false;
    };

    sydtest = {
      url = "github:NorfairKing/sydtest";
      inputs.nixpkgs.follows = "nixpkgs";
      flake = false;
    };

    safe-coloured-text = {
      url = "github:NorfairKing/safe-coloured-text";
      inputs.nixpkgs.follows = "nixpkgs";
      flake = false;
    };

    autodocodec = {
      url = "github:NorfairKing/autodocodec";
      inputs.nixpkgs.follows = "nixpkgs";
      flake = false;
    };

    typed-uuid = {
      url = "github:NorfairKing/typed-uuid";
      inputs.nixpkgs.follows = "nixpkgs";
      flake = false;
    };

    yesod-autoreload = {
      url = "github:NorfairKing/yesod-autoreload";
      inputs.nixpkgs.follows = "nixpkgs";
      flake = false;
    };

    # Doesn't depend on nixpkgs
    flake-compat-ci.url = "github:hercules-ci/flake-compat-ci";

    # Doesn't depend on nixpkgs
    # TODO van syd: dit hebt ge _waarschijnlijk_ niemeer nodig nu dat ge een
    # flake gebruikt en geen nix-build meer. YMMV
    flake-compat.url = "github:edolstra/flake-compat";
  };

  outputs = { self, nixpkgs, fast-myers-diff, validity, sydtest, safe-coloured-text, autodocodec, typed-uuid, yesod-autoreload, flake-compat-ci, flake-compat }@inputs:
    let
      # Generate a user-friendly version number.
      version = builtins.substring 0 8 self.lastModifiedDate;

      # System types to support.
      supportedSystems = [ "x86_64-linux" ];
      # TODO van syd: alsge toch maar één system support gebruikt ge beter geen
      # forAllSystems denkik. da's dan simpeler

      # Helper function to generate an attrset '{ x86_64-linux = f "x86_64-linux"; ... }'.
      forAllSystems = f: nixpkgs.lib.genAttrs supportedSystems (system: f system);

      # Nixpkgs instantiated for supported system types.
      nixpkgsFor = forAllSystems (system: import nixpkgs { inherit system; overlays = [ self.overlays.default ]; });

      # The GHC compiler version to use, from haskell.packages.<compiler>
      # TODO van syd: Ik gebruik liever 'haskellPackages' direct ipv zelf een compiler te kiezen.
      compiler = "ghc902";

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
                      yesod-autoreload = (hself.callCabal2nix "yesod-autoreload" yesod-autoreload { });
                      pact-web-server = (hself.callCabal2nix "pact-web-server" ./pact-web-server { });
                      pact-db = hself.callCabal2nix "pact-db" ./pact-db { };
                    };
                  };
                })

                # Syd's overlay assumes that it will be given haskellPackages
                # overrides already in place. These nor haskellPackages can be
                # overriden afterwards without threading Syd's changes.
                # https://github.com/NorfairKing/sydtest/blob/5b0eee208753e3554d9b158a6e48b1760514aed0/nix/overlay.nix#L89
                (import (fast-myers-diff + "/nix/overlay.nix"))
                (import (sydtest + "/nix/overlay.nix"))
                (import (validity + "/nix/overlay.nix"))
                (import (safe-coloured-text + "/nix/overlay.nix"))
                (import (autodocodec + "/nix/overlay.nix"))
                (import (typed-uuid + "/nix/overlay.nix"))
              ];
          };
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
          pkgs = nixpkgsFor.${system};

          # dit kan dan weg met withHoogle=true below
          hoogle = pkgs.buildEnv {
            name = "hoogle";
            paths = [ (pkgs.haskellPackages.ghcWithHoogle (_: pkgs.lib.attrValues self.packages.${system})) ];
          };

        in
        rec
        {
          default = pkgs.haskell.packages.${compiler}.shellFor {
            # packages = _: with pkgs.haskell.pkgs.${system}; [ ];
            # TODO van syd:
            # Hier zou ge dit moeten kunnen doen:
            # packages = p: builtins.attrValues p.pactPackages;
            packages = _: with self.packages.${system}; [ ];
            # Dan kuntge voor hoogle:
            withHoogle = true;
            doBenchmark = true; # Ook benchmark suites bouwen
            buildInputs = with pkgs; [
              haskellPackages.ormolu
              haskellPackages.hlint
              haskellPackages.hpc
              nixpkgs-fmt # Deze wilt ge liever uit uw pre-commit-hooks halen alsge die gebruikt
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
            enable = lib.mkEnableOption "Enable PACT server";
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
                      # TODO van syd: do you use websockets?
                      # Zonee: wegdoen
                      # To make the websockets api work
                      proxyWebsockets = true;
                      # TODO van syd: do you have big uploads?
                      # Zonee: wegdoen
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
                  PrivateTmp = true; # Da's lelek :p warrant een comment.
                  Restart = "always";
                };
              };
            };
        };

    };
}
