{
  description = "Haskell development environment";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        # These are ansi escape sequences for colors in the bash prompt;
        # see https://gist.github.com/fnky/458719343aabd01cfb17a3a4f7296797
        c0 = "\\[\\033[0m\\]";
        c1 = "${c0}\\[\\033[4;39m\\]";
        c1b = "${c0}\\[\\033[1;39m\\]";
        c2 = "${c0}\\[\\033[1;32m\\]";
        makePromptString = name:
          "${c2}(nix develop .#${name}) ${c1}\\u@\\h:${c1b}\\w${c0}\\n\\$ ";
        prelude = devShellName: ''
          export PS1='${makePromptString devShellName}'

          export DEVSHELL_NAME='${devShellName}'
          # if [ -x ./devshells.current.add-gc-root ]; then
          #   ./devshells.current.add-gc-root > /dev/null
          # fi

          # Fix glitchy blank VSCodium screen after updates.
          # See https://github.com/NixOS/nixpkgs/issues/259929.
          rm -rf "$HOME/.config/VSCodium/GPUCache"
        '';
        compose = pkgs.haskell.lib.compose;
        rw-pkg = compose.addTestToolDepend [ pkgs.diffutils ]
          (compose.dontHaddock (compose.disableLibraryProfiling
            (pkgs.haskellPackages.developPackage { root = ./.; })));
      in {
        packages = rec {
          default = rw;
          # The tests require access to the executable produced in 'rw-pkg'.
          # For some reason, 'developPackage' doesn't recognize this, even
          # though we explicitly state so in the .cabal. As a fix, build the
          # package twice, the first time without tests, using the executable
          # produced in the first go-around to run tests in the second.
          # Unfortunately, this means that it takes twice as long to compile.
          # There's got to be a better way...
          rw = compose.addTestToolDepend (compose.dontCheck rw-pkg) rw-pkg;
        };
        devShells.default = with pkgs;
          haskellPackages.shellFor {
            shellHook = ''
              ${prelude "default"}
            '';

            withHoogle = true;

            packages = p: [ rw-pkg ];

            enableLibraryProfiling = true;
            enableExecutableProfiling = true;

            nativeBuildInputs = with haskellPackages; [
              jq
              cabal-install
              ghcid
              haskell-language-server
              hlint
              ormolu
              nixfmt
              (vscode-with-extensions.override {
                vscode = vscodium;
                vscodeExtensions = with vscode-extensions; [
                  haskell.haskell
                  justusadam.language-haskell
                  jnoortheen.nix-ide
                ];
              })
            ];
          };
      });
}
