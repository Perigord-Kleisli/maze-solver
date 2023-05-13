{pkgs, ...}: {
  name = "maze-solver";
  compiler-nix-name = "ghc925";
  # index-state = "2023-01-21T20:07:39Z";
  modules = [
    {
      packages.gl.components.library = {
        libs = pkgs.lib.mkForce [pkgs.libGL];
        doHaddock = false;
      };
      packages.zlib.components.library.libs = pkgs.lib.mkForce [pkgs.zlib];
      packages.OpenGLRaw.components.library.doHaddock = false;
    }
  ];

  shell = {
    withHoogle = true;
    shellHook = ''
      alias cabal='LD_PRELOAD="${pkgs.freeglut}/lib/libglut.so" cabal'
    '';
    tools.cabal = "latest";
    tools.cabal-fmt = "latest";
    tools.hlint = "latest";
    tools.haskell-language-server = "latest";
    buildInputs = with pkgs; [openjdk bash];
  };
}
