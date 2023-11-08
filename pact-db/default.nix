{ mkDerivation, aeson, base, blaze-markup, bytestring, containers
, either, format-numbers, jose, lib, monad-logger, mtl, password
, password-instances, persistent, persistent-sqlite
, persistent-template, servant, servant-auth, servant-auth-server
, servant-client-core, text, time, typed-uuid, uuid, validity
, validity-bytestring, validity-persistent, validity-text
, validity-uuid, yamlparse-applicative, yesod
}:
mkDerivation {
  pname = "pact-db";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base blaze-markup bytestring containers either format-numbers
    jose monad-logger mtl password password-instances persistent
    persistent-sqlite persistent-template servant servant-auth
    servant-auth-server servant-client-core text time typed-uuid uuid
    validity validity-bytestring validity-persistent validity-text
    validity-uuid yamlparse-applicative yesod
  ];
  homepage = "https://github.com/CSVdB/pact-code#readme";
  license = lib.licenses.unfree;
  hydraPlatforms = lib.platforms.none;
}
