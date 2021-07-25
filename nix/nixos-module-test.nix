{ sources ? import ./sources.nix
, pkgs ? import ./pkgs.nix { inherit sources; }
}:
let
  pact-production = import (./nixos-module.nix) { envname = "production"; pactPackages = pkgs.pactPackages; };
  port = 8001;
in
pkgs.nixosTest (
  { lib, pkgs, ... }: {
    name = "pact-module-test";
    machine = {
      imports = [
        pact-production
      ];
      services.pact.production = {
        enable = true;
        web-server = {
          enable = true;
          inherit port;
        };
      };
    };
    testScript = ''
      from shlex import quote

      machine.start()
      machine.wait_for_unit("multi-user.target")

      machine.wait_for_open_port(${builtins.toString port})
    '';
  }
)
