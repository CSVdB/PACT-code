let
  sources = import ./nix/sources.nix;
  pkgs = import ./nix/pkgs.nix { inherit sources; };
  pre-commit = import ./nix/pre-commit.nix { inherit sources; };

  pursPkgs = import
    (pkgs.fetchFromGitHub {
      owner = "justinwoo";
      repo = "easy-purescript-nix";
      rev = "47bdc016c7d56e987ca1aca690b1d6c9816a8584";
      sha256 = "sha256:051fzxd03y0c63sll2bhn0h66dywy9lw6ylyh5vq8fymvix20q94";
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
