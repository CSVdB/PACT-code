{ pkgs }:
rec {
  lintScript = pkgs.writers.writeBashBin "lint" ''
    # Lint and modify the files in place.
    ${pkgs.haskellPackages.ormolu} -m inplace **/*.hs
    # TODO: Add hpack formatting
    # TODO: Add nixpkgs-fmt
    # TODO: Add shellcheck

    # Exit with non zero status if we're now in unclean state
    if [ -z "$(git status --porcelain)" ]; then
        echo "No style errors detected."
    else
        echo "Style errors detected:"
        git --no-pager diff
        echo
        echo "Run git status to see the changes made."
        exit 1
    fi
    hlint .
  '';
  lintDerivation = src: pkgs.stdenv.mkDerivation {
    name = "lint-haskell";
    src = src;
    dontBuild = true;
    installPhase = ''
      PATH="$PATH:${pkgs.git}/bin"
      export GIT_AUTHOR_NAME="nobody"
      export EMAIL="no@body.com"
      git init
      git add .
      git commit -m "init"
      ${lintScript}/bin/lint | tee $out
    '';
  };
}
