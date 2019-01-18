{ mkDerivation, base, pure-core, pure-cache, pure-async, pure-try, pure-default, stdenv }:
mkDerivation {
  pname = "pure-async";
  version = "0.7.0.0";
  src = ./.;
  libraryHaskellDepends = [ base pure-cache pure-core pure-async pure-try pure-default ];
  homepage = "github.com/grumply/pure-async";
  description = "Async decorator";
  license = stdenv.lib.licenses.bsd3;
}
