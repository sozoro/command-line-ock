{ mkDerivation, base, matrix, stdenv, time, vector, vty }:
mkDerivation {
  pname = "command-line-ock";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base matrix time vector vty ];
  executableHaskellDepends = [ base matrix time vector vty ];
  description = "Command Line Clock";
  license = stdenv.lib.licenses.publicDomain;
}
