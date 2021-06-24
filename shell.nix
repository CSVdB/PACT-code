let
  sources = import ./nix/sources.nix;
  pkgs = import ./nix/pkgs.nix { inherit sources; };
  pre-commit = import ./nix/pre-commit.nix { inherit sources; };

  pursPkgs = import
    (pkgs.fetchFromGitHub {
      owner = "justinwoo";
      repo = "easy-purescript-nix";
      rev = "fbbb27c1afd51d729939a6a2006e954dbd844846";
      sha256 = "1kw9cqycrq456dipd5mq7c1ij6jl3d9ajlnba152db3qrw5wmrg0";
    })
    { inherit pkgs; };
in
pkgs.haskell.lib.buildStackProject {
  name = "pact-nix-shell";
  buildInputs = with pkgs; [
    haskellPackages.autoexporter
    (import sources.niv { }).niv
    killall
    zlib
    pursPkgs.purs
    pursPkgs.spago
    pursPkgs.zephyr
    pkgs.nodejs-14_x
  ] ++ pre-commit.tools;
  shellHook = pre-commit.run.shellHook;
}
