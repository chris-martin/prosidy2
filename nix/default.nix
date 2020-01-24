let
    pkgs = 
        import <nixpkgs> {};

    haskellPackages =
        pkgs.haskell.packages.ghc881.override {
            overrides = self: super: {
                lens = pkgs.haskell.lib.overrideCabal super.lens (old:
                    {
                        version = "4.18.1";
                        revision = null;
                        editedCabalFile = null;
                        patches = [];
                        libraryHaskellDepends = old.libraryHaskellDepends ++ [self.type-equality];
                        sha256 = "1lmxjaj32v06l12gy00rpjp2lk1cblh3k7kwklk655ss2vas61ri";
                    }
                );
            };
        };

    rules = self: {
        ghc = haskellPackages.ghcWithPackages (hs: with hs; [
            aeson
            blaze-html
            cabal-install
            generic-lens
            ghc
            lens
            mmorph
            megaparsec
            optparse-applicative
        ]);

        m = pkgs.stdenv.mkDerivation {
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
                export LOCALE_ARCHIVE='${pkgs.glibcLocales}/lib/locale/locale-archive'
                export PATH="${self.ghc}/bin:\$PATH"
                exec "$out/bin/.m-unwrapped" "\$@"
                EOF
                chmod +x "$out/bin/m"
            '';
        };
    };
in
    pkgs.lib.fix rules
