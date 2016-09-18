HTas
====

This repository contains my work toward scripting tool-assisted inputs
in Haskell. The HTas directory provides hooks into a slightly modified
libgambatte, while the Red directory provides helpers specifically for
Pokemon Red.

Compiling and Running
=====================
If you want to compile and run this code yourself, here are the steps.

1. [Install Stack](https://docs.haskellstack.org/en/stable/README/#how-to-install)
2. Run `stack setup`
3. To compile, run `stack build`
4. To run, run `stack exec (program name)`. Currently the program names are `test`
   (for Main.hs), `moonsearch` (for MoonSearch.hs), and `moonigt60` (for MoonIGT60.hs).
