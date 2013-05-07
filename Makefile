all:
	ghc -O0 Lispg `llvm-config-3.0 --libs core codegen x86codegen`
