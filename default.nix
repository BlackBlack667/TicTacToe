let
  # Read in the Niv sources
  sources = import ./nix/sources.nix {};
  # If ./nix/sources.nix file is not found run:
  #   niv init
  #   niv add input-output-hk/haskell.nix -n haskellNix

  # Fetch the haskell.nix commit we have pinned with Niv
  haskellNix = import sources.haskellNix {};
  # If haskellNix is not found run:
  #   niv add input-output-hk/haskell.nix -n haskellNix

  freetype-overlay = self: super:
    {
      libpng = super.libpng.overrideAttrs(old: if !self.stdenv.hostPlatform.isWindows then {} else {
        # freetype on windows complains about the presence of the .la file
        # removing it seems to work
        postInstall = ''
          rm $out/lib/libpng16.la
        '';
      });
      freetype = super.freetype.overrideAttrs(old: if !self.stdenv.hostPlatform.isWindows then {} else {
        # See https://github.com/NixOS/nixpkgs/issues/160323
        # We can just remove these and then we'll avoid the `wrapProgram` (and therefore bash) dependency
        nativeBuildInputs = [];
        postInstall = "";
      });
    };

  freeglut-overlay = self: super:
    {
      freeglut = super.freeglut.overrideAttrs(old: if !self.stdenv.hostPlatform.isWindows then {} else {
        buildInputs = [ ];
        cmakeFlags = old.cmakeFlags ++ [
          # See freeglut's README.mingw_cross
          "-DCMAKE_TOOLCHAIN_FILE=mingw_cross_toolchain.cmake"
          "-DGNU_HOST=x86_64-w64-mingw32"
        ];
      });
    };
  # Import nixpkgs and pass the haskell.nix provided nixpkgsArgs
  pkgs = import
    # haskell.nix provides access to the nixpkgs pins which are used by our CI,
    # hence you will be more likely to get cache hits when using these.
    # But you can also just use your own, e.g. '<nixpkgs>'.
    haskellNix.sources.nixpkgs-unstable
    # These arguments passed to nixpkgs, include some patches and also
    # the haskell.nix functionality itself as an overlay.
    {
      config = haskellNix.nixpkgsArgs.config;
      overlays = haskellNix.nixpkgsArgs.overlays ++ [ freetype-overlay freeglut-overlay ];
    };
in pkgs.haskell-nix.project {
  # 'cleanGit' cleans a source directory based on the files known by git
  src = pkgs.haskell-nix.haskellLib.cleanGit {
    name = "TicTacToe";
    src = ./.;
  };
  # Specify the GHC version to use.
  compiler-nix-name = "ghc924";
}
