{
  description = "PACT-code";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-23.05";

    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";

    fast-myers-diff = {
      url = "github:NorfairKing/fast-myers-diff";
      flake = false;
    };

    validity = {
      url = "github:NorfairKing/validity";
      flake = false;
    };

    sydtest = {
      url = "github:NorfairKing/sydtest";
      flake = false;
    };

    safe-coloured-text = {
      url = "github:NorfairKing/safe-coloured-text";
      flake = false;
    };

    autodocodec = {
      url = "github:NorfairKing/autodocodec";
      flake = false;
    };

    typed-uuid = {
      url = "github:NorfairKing/typed-uuid";
      flake = false;
    };

    yesod-autoreload = {
      url = "github:NorfairKing/yesod-autoreload";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, pre-commit-hooks, fast-myers-diff, validity, sydtest, safe-coloured-text, autodocodec, typed-uuid, yesod-autoreload }:
    let
      system = "x86_64-linux";

      overlay = final: prev: {
        # We want to build the pact-web-server both as library (into haskellPackages) and as the executable. The executable doesn't need all the libraries and ghc itself to be available, only its output. This is what the next line does.
        oura = final.haskell.lib.justStaticExecutables final.haskellPackages.oura;
        pact-web-server = final.haskell.lib.justStaticExecutables final.haskellPackages.pact-web-server;
        haskellPackages = prev.haskellPackages.override (old: {
          overrides = final.lib.composeExtensions (old.overrides or (_: _: { }))
            (self: _:
              let
                pactPackages = {
                  # callPackage uses default.nix, which is derived from the
                  # packages through the `callCabal2nix` in the
                  # pre-commit-hooks.
                  yesod-autoreload = self.callPackage yesod-autoreload { };
                  oura = self.callPackage ./oura { };
                  pact-web-server = self.callPackage ./pact-web-server { };
                  pact-db = self.callPackage ./pact-db { };
                };
              in
              {
                inherit pactPackages;
              } // pactPackages
            );
        });
      };
      pkgs = import nixpkgs {
        inherit system; config.allowUnfree = true;
        overlays =
          [
            overlay

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
    in
    {
      packages.${system} = {
        default = pkgs.pact-web-server;
        inherit (pkgs) pact-web-server oura;
      };

      devShells.${system} =
        rec
        {
          default = pkgs.haskellPackages.shellFor {
            # Create a shell with all the dependencies of the pactPackages, but not the pactPackages themselves.
            packages = p: builtins.attrValues p.pactPackages;
            withHoogle = true;
            doBenchmark = true; # Ook benchmark suites bouwen
            buildInputs = (with pkgs; [
              zlib
              sqlite
            ]) ++ (with pre-commit-hooks.packages.${system};
              [
                hlint
                hpack
                nixpkgs-fmt
                ormolu
                cabal2nix
              ]);
            shellHook = self.checks.${system}.pre-commit.shellHook;
          };
        };

      checks.${system} =
        with pkgs;
        lib.optionalAttrs stdenv.isLinux {
          pre-commit = pre-commit-hooks.lib.${system}.run {
            src = ./.;
            hooks = {
              hlint.enable = true;
              hpack.enable = true;
              ormolu.enable = true;
              nixpkgs-fmt.enable = true;
              nixpkgs-fmt.excludes = [ ".*/default.nix" ];
              deadnix.enable = true;
              cabal2nix.enable = true; # Generate default.nix for your packages, to avoid IFD
            };
          };
        };

      nixosModules = {
        oura = { lib, config, ... }:
          {
            options.services.oura = {
              enable = lib.mkEnableOption "Enable daily Oura data syncing";
              db_path = lib.mkOption {
                default = "./pact.sqlite3";
                description = "Path to the SQLite database";
              };
            };
            config =
              let
                cfg = config.services.oura;
              in
              lib.mkIf cfg.enable {
                systemd = {
                  services.oura = {
                    description = "Oura data syncing";
                    wantedBy = [ "multi-user.target" ];
                    after = [ "networking.target" ];
                    serviceConfig = {
                      ExecStart = ''
                        ${self.packages.${system}.oura}/bin/oura --db-path ${cfg.db_path}
                      '';
                      Type = "oneshot";
                    };
                  };

                  timers.oura = {
                    wantedBy = [ "timers.target" ];
                    partOf = [ "oura.service" ];
                    timerConfig.OnCalendar = [ "*-*-* *:00:00" ];
                  };
                };
              };
          };

        pact-web-server = { lib, config, ... }:
          {
            options.services.pact-web-server = {
              enable = lib.mkEnableOption "Enable PACT server";
              port = lib.mkOption {
                default = 8080;
                description = "Port to run on";
              };
              artifacts_dir = lib.mkOption {
                default = "./";
                description = "path where the SQLite dB and session key file are stored";
              };
              hosts = lib.mkOption {
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
                services.nginx.virtualHosts = web-server-host;

                # Only open this port if you don't use a reverse proxy in the
                # deployment, i.e. if you want customers to call this port
                # explicitly. Otherwise, we don't need to allow this port's
                # access through the firewall.
                # networking.firewall.allowedTCPPorts = [ cfg.port ];

                systemd.services.pact-web-server = {
                  description = "The PACT web server service";
                  wantedBy = [ "multi-user.target" ];
                  after = [ "networking.target" ];
                  serviceConfig = {
                    ExecStart =
                      ''
                        ${self.packages.${system}.default}/bin/pact-web-server --port ${toString cfg.port} --artifacts_dir ${cfg.artifacts_dir}
                      '';
                    Restart = "always";
                  };
                };
              };
          };
      };
    };
}
