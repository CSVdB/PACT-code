{ sources ? import ./sources.nix
, pkgs ? import ./pkgs.nix { inherit sources; }
}:
let
  pact-production = import (./nixos-module.nix) { envname = "production"; pactPackages = pkgs.pactPackages; };
  home-manager = import (sources.home-manager + "/nixos/default.nix");
  port = 8001;
in
pkgs.nixosTest (
  { lib, pkgs, ... }: {
    name = "pact-module-test";
    nodes = {
      server = {
        imports = [
          pact-production
        ];
        services.pact.production = {
          enable = true;
          api-server = {
            enable = true;
            inherit port;
          };
        };
      };
    };
    testScript = ''
      from shlex import quote

      server.start()
      server.wait_for_unit("multi-user.target")

      server.wait_for_open_port(${builtins.toString port})

    '';
  }
)
