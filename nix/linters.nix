{ pkgs }:
rec {
  lintScript = pkgs.writers.writeBashBin "lint" ''
    # Lint and modify the files in place.
    HS_FILES=$(find . -type f -name '*.hs' ! -path './dist-newstyle/*')
    HPACK_FILES=$(find . -type f -name 'package.yaml' ! -path './dist-newstyle/*')
    for hs_file in $HS_FILES
    do
      ${pkgs.ormolu}/bin/ormolu -m inplace $hs_file
    done

    for hpack_file in $HPACK_FILES
    do
      ${pkgs.hpack}/bin/hpack $hpack_file
    done

    ${pkgs.nixpkgs-fmt}/bin/nixpkgs-fmt $(find . -type f -name '*.nix')
    ${pkgs.shellcheck}/bin/shellcheck $(find . -type f -name '*.sh')

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
    ${pkgs.hlint}/bin/hlint .
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
