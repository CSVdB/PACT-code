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
  };

  outputs = { self, nixpkgs, fast-myers-diff, validity, sydtest, safe-coloured-text, autodocodec, typed-uuid, yesod-autoreload }@inputs:
    let
      # Generate a user-friendly version number.
      version = builtins.substring 0 8 self.lastModifiedDate;

      # System types to support.
      supportedSystems = [ "x86_64-linux" ];

      # Helper function to generate an attrset '{ x86_64-linux = f "x86_64-linux"; ... }'.
      forAllSystems = f: nixpkgs.lib.genAttrs supportedSystems (system: f system);

      # Nixpkgs instantiated for supported system types.
      nixpkgsFor = forAllSystems (system: import nixpkgs { inherit system; overlays = [ self.overlays.default ]; });

      linters = forAllSystems (system: {
        haskell = import ./nix/linters.nix { pkgs = nixpkgsFor.${system}; };
      });

    in
    {
      overlays.default = final: prev:
        {
          haskellPackages = prev.haskell.packages.ghc902 // {
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
                  # We want to build the pact-web-server both as library (into haskellPackages) and as the executable. The executable doesn't need all the libraries and ghc itself to be available, only its output. This is what the next line does.
                  pact-web-server = final.haskell.lib.justStaticExecutables final.haskellPackages.pact-web-server;

                  haskellPackages = final.haskell.packages.ghc902.override {
                    overrides = hself: hsuper:
                    let
                      pactPackages = {
                        yesod-autoreload = hself.callCabal2nix "yesod-autoreload" yesod-autoreload { };
                        pact-web-server = hself.callCabal2nix "pact-web-server" ./pact-web-server { };
                        pact-db = hself.callCabal2nix "pact-db" ./pact-db { };
                      };
                    in 
                    {
                      inherit pactPackages;
                    } // pactPackages;
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

        in {
          default = pkgs.pact-web-server;
          inherit (pkgs) pact-web-server;
        });

      devShells = forAllSystems (system:
        let
          pkgs = nixpkgsFor.${system};

        in
        rec
        {
          default = pkgs.haskellPackages.shellFor {
            packages = p: builtins.attrValues p.pactPackages;
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
              default = pkgs.pact-web-server;
              # defaultText = "pkgs.pact-web-server";
              # type = lib.types.package;
              description = "PACT web server package to use";
            };
            port = lib.mkOption {
              default = 8080;
              # defaultText = "8080";
              # type = lib.types.port;
              description = "Port to run on";
            };
            artifacts_dir = lib.mkOption {
              default = "./";
              # defaultText = "./";
              # type = lib.types.path;
              description = "path where the SQLite dB and session key file are stored";
            };
            hosts = lib.mkOption {
              # type = lib.types.listOf lib.types.str;
              # example = [ "pactcommunity.be" ];
              default = [ ];
              description = "The host to serve web requests on";
            };
          };
          config =
            let
              cfg = config.services.pact-web-server;

              web-server-host = with cfg; {
                "${builtins.head hosts}" =
                  {
                    enableACME = true;
                    forceSSL = true;
                    locations."/".proxyPass = "http://localhost:${builtins.toString port}";
                    serverAliases = builtins.tail hosts;
                  };
              };
            in
            lib.mkIf cfg.enable {
              nixpkgs.overlays = [ self.overlays.default ];

              services.nginx.virtualHosts = web-server-host;

              # Only open this port if you don't use a reverse proxy in the deployment.
              # networking.firewall.allowedTCPPorts = [ cfg.port ];

              systemd.services.pact-web-server = {
                description = "The PACT web server service";
                wantedBy = [ "multi-user.target" ];
                after = [ "networking.target" ];
                serviceConfig = {
                  ExecStart =
                    ''
                      ${pkgs.haskellPackages.pact-web-server}/bin/pact-web-server --port ${toString cfg.port} --artifacts_dir ${cfg.artifacts_dir}
                    '';
                  # PrivateTmp = true; # This is probably not necessary.
                  Restart = "always";
                };
              };
            };
        };

    };
}
