with import <nixpkgs> {};

let
    rules = self: {
        ghc = haskellPackages.ghcWithPackages (hs: with hs; [
            aeson
            blaze-html
            generic-lens
            ghc
            lens
            mmorph
            megaparsec
            optparse-applicative
            shake
            regex-pcre
        ]);

        m = stdenv.mkDerivation {
            name = "m";
            version = "0.1";
            srcs = ../build;
            buildInputs = [ 
                self.ghc 
            ];
            buildPhase = ''
                ghc --make Build.hs -O -o m -main-is Build.main
            '';
            installPhase = ''
                mkdir -p "$out/bin"
                mv m "$out/bin/.m-unwrapped"
                cat <<EOF > "$out/bin/m"
                #!/bin/sh
                export LOCALE_ARCHIVE='${glibcLocales}/lib/locale/locale-archive'
                export PATH="${self.ghc}/bin:\$PATH"
                exec "$out/bin/.m-unwrapped" "\$@"
                EOF
                chmod +x "$out/bin/m"
            '';
        };
    };
in
    lib.fix rules