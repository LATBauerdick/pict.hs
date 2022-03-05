{ mkDerivation, base, directory, filepath, lib, parallel, process
, time
, executableSystemDepends
}:
mkDerivation {
  inherit executableSystemDepends;
  pname = "pict";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base directory filepath parallel process time
  ];
  executableHaskellDepends = [
    base directory filepath parallel process time
  ];
  testHaskellDepends = [ base ];
  homepage = "https://github.com/LATBauerdick/pict.hs#readme";
  description = "haskell implementation of pict JPEG copy";
  license = lib.licenses.bsd3;
}
