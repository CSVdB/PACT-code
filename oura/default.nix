{ mkDerivation, autoexporter, base, lib, pact-web-server, servant
, sydtest-discover
}:
mkDerivation {
  pname = "oura";
  version = "0.0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base servant ];
  libraryToolDepends = [ autoexporter ];
  executableHaskellDepends = [ base pact-web-server ];
  testHaskellDepends = [ base ];
  testToolDepends = [ sydtest-discover ];
  homepage = "https://github.com/CSVdB/pact-code#readme";
  license = lib.licenses.unfree;
  hydraPlatforms = lib.platforms.none;
  mainProgram = "pact-web-server";
}
