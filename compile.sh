# everything in this repo is fucked up, I know, if you are going to complain, fix it first duh

ghc -Wall -O2 -o \
	   yixem yixem.hs yixem-parser.hs yixem-compiler.hs yixem-corelang.hs yixem-coretypechecker.hs \
           yixem-typechecker.hs yixem-optimizer.hs yixem-helpers.hs yixem-optimizer-types.hs           \
           yixem-optimizer-lambdalift.hs yixem-ast.hs yixem-optimizer-shadowelim.hs                    \
           yixem-optimizer-vartracker.hs # ..