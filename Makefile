llvm-libs := $(shell `which llvm-config-3.0 2> /dev/null || echo llvm-config` --libs core codegen x86codegen)

all:
	ghc -O0 Lispg $(llvm-libs)

clean:
	rm *.o *.hi Lispg
