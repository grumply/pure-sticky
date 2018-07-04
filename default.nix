{ mkDerivation, base, pure, pure-cond, pure-prop, stdenv
}:
mkDerivation {
  pname = "pure-sticky";
  version = "0.7.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base pure pure-cond pure-prop
  ];
  license = stdenv.lib.licenses.bsd3;
}