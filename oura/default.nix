{ mkDerivation, aeson, autoexporter, base, containers, envparse
, http-client, http-client-tls, lib, microlens, monad-logger
, optparse-applicative, pact-db, path, path-io, persistent
, persistent-sqlite, servant, servant-client, sydtest
, sydtest-discover, text, time, vector
}:
mkDerivation {
  pname = "oura";
  version = "0.0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base containers envparse http-client http-client-tls
    microlens monad-logger optparse-applicative pact-db path path-io
    persistent persistent-sqlite servant servant-client text time
    vector
  ];
  libraryToolDepends = [ autoexporter ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [ base sydtest ];
  testToolDepends = [ sydtest-discover ];
  homepage = "https://github.com/CSVdB/pact-code#readme";
  license = lib.licenses.unfree;
  hydraPlatforms = lib.platforms.none;
  mainProgram = "oura";
}
