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
            aeson-diff
            aeson-pretty
            blaze-html
            cabal-install
            generic-lens
            ghc
            lens
            mmorph
            megaparsec
            optparse-applicative
            tasty
            tasty-golden
            tasty-hunit
        ]);
    };
in
    pkgs.lib.fix rules
