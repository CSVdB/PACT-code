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
        # oura = final.haskell.lib.justStaticExecutables final.haskellPackages.oura;
        pact-web-server = final.haskell.lib.justStaticExecutables final.haskellPackages.pact-web-server;

        haskellPackages = prev.haskellPackages.override (old: {
          overrides = final.lib.composeExtensions (old.overrides or (_: _: { }))
            (self: super:
              let
                pactPackages = {
                  # TODO get rid of IFD
                  yesod-autoreload = self.callCabal2nix "yesod-autoreload" yesod-autoreload { };
                  # oura = self.callCabal2nix "oura" ./oura { };
                  pact-web-server = self.callCabal2nix "pact-web-server" ./pact-web-server { };
                  pact-db = self.callCabal2nix "pact-db" ./pact-db { };
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
        inherit (pkgs) pact-web-server; # oura
      };

      devShells.${system} =
        rec
        {
          default = pkgs.haskellPackages.shellFor {
            packages = p: [
              p.pact-web-server
              # p.oura
            ];
            withHoogle = true;
            doBenchmark = true; # Ook benchmark suites bouwen
            buildInputs = (with pkgs; [
              zlib
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
              # TODO: Use deadnix
              # deadnix.enable = true;
            };
          };
        };

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
                      ${pkgs.pact-web-server}/bin/pact-web-server --port ${toString cfg.port} --artifacts_dir ${cfg.artifacts_dir}
                    '';
                  # PrivateTmp = true; # This is probably not necessary.
                  Restart = "always";
                };
              };
            };
        };

    };
}
