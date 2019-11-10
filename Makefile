SHELL = /bin/bash

GHC = ghc
GHC_FLAGS = --make -prof -fprof-auto -rtsopts -O2 -hidir hi_dir -odir o_dir -Weverything

BENCHMARKING_DIR = ./Benchmarking
EXTERN_DIR = ./Extern
INTERN_DIR = ./Intern
O_DIR  = ./o_dir
HI_DIR = ./hi_dir

OBJS = *.o $(BENCHMARKING_DIR)/Extern/*.o $(BENCHMARKING_DIR)/Intern/*.o $(EXTERN_DIR)/*.o $(INTERN_DIR)/*.o
INTERFACES = *.hi $(BENCHMARKING_DIR)/Extern/*.hi $(BENCHMARKING_DIR)/Intern/*.hi $(EXTERN_DIR)/*.hi $(INTERN_DIR)/*.hi

.PHONY: all
all: clean benchmarking

benchmark_extern_insert10: benchmarking_extern_deps
	$(GHC) $(GHC_FLAGS) $(BENCHMARKING_DIR)/Extern/Insert10.hs

benchmarking: benchmarking_extern_deps benchmarking_intern_deps
	$(GHC) $(GHC_FLAGS) $(BENCHMARKING_DIR)/Extern/Operations.hs
	$(GHC) $(GHC_FLAGS) $(BENCHMARKING_DIR)/Intern/Operations.hs

benchmarking_extern_deps:
	$(GHC) $(GHC_FLAGS) -no-link $(BENCHMARKING_DIR)/Extern/Operations.hs
benchmarking_intern_deps:
	$(GHC) $(GHC_FLAGS) -no-link $(BENCHMARKING_DIR)/Intern/Operations.hs

.PHONY: clean
clean:
	rm -fr $(O_DIR) $(HI_DIR)
