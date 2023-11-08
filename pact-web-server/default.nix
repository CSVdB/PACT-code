{ mkDerivation, autoexporter, base, bytestring, containers
, data-default, directory, envparse, file-embed, filepath
, genvalidity, genvalidity-bytestring, genvalidity-sydtest
, genvalidity-sydtest-aeson, genvalidity-sydtest-persistent
, genvalidity-text, genvalidity-time, genvalidity-typed-uuid
, genvalidity-uuid, hspec, http-client, http-client-tls, lib
, microlens, monad-logger, mtl, optparse-applicative, pact-db
, password, path, path-io, persistent, persistent-sqlite
, pretty-show, QuickCheck, random, shakespeare, sydtest
, sydtest-discover, sydtest-persistent, sydtest-persistent-sqlite
, sydtest-wai, sydtest-yesod, template-haskell, text, time
, typed-uuid, uuid, validity, validity-time, wai-extra, warp, yaml
, yamlparse-applicative, yesod, yesod-auth, yesod-autoreload
, yesod-static
}:
mkDerivation {
  pname = "pact-web-server";
  version = "0.0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring containers data-default envparse file-embed
    http-client http-client-tls microlens monad-logger
    optparse-applicative pact-db path path-io persistent
    persistent-sqlite pretty-show shakespeare template-haskell text
    time typed-uuid uuid validity validity-time wai-extra warp yaml
    yamlparse-applicative yesod yesod-auth yesod-autoreload
    yesod-static
  ];
  libraryToolDepends = [ autoexporter ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    base bytestring containers directory filepath genvalidity
    genvalidity-bytestring genvalidity-sydtest
    genvalidity-sydtest-aeson genvalidity-sydtest-persistent
    genvalidity-text genvalidity-time genvalidity-typed-uuid
    genvalidity-uuid hspec http-client microlens monad-logger mtl
    pact-db password path-io persistent persistent-sqlite QuickCheck
    random sydtest sydtest-persistent sydtest-persistent-sqlite
    sydtest-wai sydtest-yesod text time typed-uuid uuid yesod
    yesod-auth
  ];
  testToolDepends = [ sydtest-discover ];
  homepage = "https://github.com/CSVdB/pact-code#readme";
  license = lib.licenses.unfree;
  hydraPlatforms = lib.platforms.none;
  mainProgram = "pact-web-server";
}
