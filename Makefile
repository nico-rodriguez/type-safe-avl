SHELL = /bin/bash

GHC = ghc
GHC_FLAGS = --make -prof -fprof-auto -rtsopts -O2 -outputdir output_dir -Weverything

BENCHMARKING_DIR = ./Benchmarking
EXTERN_DIR = ./Extern
INTERN_DIR = ./Intern
OUTPUT_DIR = ./output_dir
O_DIR  = ./o_dir
HI_DIR = ./hi_dir

OBJS = *.o $(BENCHMARKING_DIR)/Extern/*.o $(BENCHMARKING_DIR)/Intern/*.o $(EXTERN_DIR)/*.o $(INTERN_DIR)/*.o
INTERFACES = *.hi $(BENCHMARKING_DIR)/Extern/*.hi $(BENCHMARKING_DIR)/Intern/*.hi $(EXTERN_DIR)/*.hi $(INTERN_DIR)/*.hi

.PHONY: all
all: clean benchmarking


# Benchmarks for Extern insertAVL

benchmark_extern_insert10: benchmarking_extern_deps
	$(GHC) $(GHC_FLAGS) -o $(OUTPUT_DIR)/Benchmarking/Extern/Insert/Insert10 $(BENCHMARKING_DIR)/Extern/Insert/Insert10.hs

benchmark_extern_insert20: benchmarking_extern_deps
	$(GHC) $(GHC_FLAGS) -o $(OUTPUT_DIR)/Benchmarking/Extern/Insert/Insert20 $(BENCHMARKING_DIR)/Extern/Insert/Insert20.hs

benchmark_extern_insert30: benchmarking_extern_deps
	$(GHC) $(GHC_FLAGS) -o $(OUTPUT_DIR)/Benchmarking/Extern/Insert/Insert30 $(BENCHMARKING_DIR)/Extern/Insert/Insert30.hs

benchmark_extern_insert40: benchmarking_extern_deps
	$(GHC) $(GHC_FLAGS) -o $(OUTPUT_DIR)/Benchmarking/Extern/Insert/Insert40 $(BENCHMARKING_DIR)/Extern/Insert/Insert40.hs

benchmark_extern_insert50: benchmarking_extern_deps
	$(GHC) $(GHC_FLAGS) -o $(OUTPUT_DIR)/Benchmarking/Extern/Insert/Insert50 $(BENCHMARKING_DIR)/Extern/Insert/Insert50.hs

benchmark_extern_insert60: benchmarking_extern_deps
	$(GHC) $(GHC_FLAGS) -o $(OUTPUT_DIR)/Benchmarking/Extern/Insert/Insert60 $(BENCHMARKING_DIR)/Extern/Insert/Insert60.hs

benchmarking_extern_deps:
	$(GHC) $(GHC_FLAGS) -no-link $(BENCHMARKING_DIR)/Extern/Operations.hs


# Benchmarks for FullExtern insertAVL

benchmark_fullextern_insert10: benchmarking_fullextern_deps
	$(GHC) $(GHC_FLAGS) -o $(OUTPUT_DIR)/Benchmarking/FullExtern/Insert/Insert10 $(BENCHMARKING_DIR)/FullExtern/Insert/Insert10.hs

benchmark_fullextern_insert20: benchmarking_fullextern_deps
	$(GHC) $(GHC_FLAGS) -o $(OUTPUT_DIR)/Benchmarking/FullExtern/Insert/Insert20 $(BENCHMARKING_DIR)/FullExtern/Insert/Insert20.hs

benchmark_fullextern_insert30: benchmarking_fullextern_deps
	$(GHC) $(GHC_FLAGS) -o $(OUTPUT_DIR)/Benchmarking/FullExtern/Insert/Insert30 $(BENCHMARKING_DIR)/FullExtern/Insert/Insert30.hs

benchmark_fullextern_insert40: benchmarking_fullextern_deps
	$(GHC) $(GHC_FLAGS) -o $(OUTPUT_DIR)/Benchmarking/FullExtern/Insert/Insert40 $(BENCHMARKING_DIR)/FullExtern/Insert/Insert40.hs

benchmark_fullextern_insert50: benchmarking_fullextern_deps
	$(GHC) $(GHC_FLAGS) -o $(OUTPUT_DIR)/Benchmarking/FullExtern/Insert/Insert50 $(BENCHMARKING_DIR)/FullExtern/Insert/Insert50.hs

benchmark_fullextern_insert60: benchmarking_fullextern_deps
	$(GHC) $(GHC_FLAGS) -o $(OUTPUT_DIR)/Benchmarking/FullExtern/Insert/Insert60 $(BENCHMARKING_DIR)/FullExtern/Insert/Insert60.hs

benchmarking_fullextern_deps:
	$(GHC) $(GHC_FLAGS) -no-link $(BENCHMARKING_DIR)/FullExtern/Operations.hs


# Benchmarks for Intern insertAVL

benchmark_intern_insert10: benchmarking_intern_deps
	$(GHC) $(GHC_FLAGS) -o $(OUTPUT_DIR)/Benchmarking/Intern/Insert/Insert10 $(BENCHMARKING_DIR)/Intern/Insert/Insert10.hs

benchmark_intern_insert20: benchmarking_intern_deps
	$(GHC) $(GHC_FLAGS) -o $(OUTPUT_DIR)/Benchmarking/Intern/Insert/Insert20 $(BENCHMARKING_DIR)/Intern/Insert/Insert20.hs

benchmark_intern_insert30: benchmarking_intern_deps
	$(GHC) $(GHC_FLAGS) -o $(OUTPUT_DIR)/Benchmarking/Intern/Insert/Insert30 $(BENCHMARKING_DIR)/Intern/Insert/Insert30.hs

benchmark_intern_insert40: benchmarking_intern_deps
	$(GHC) $(GHC_FLAGS) -o $(OUTPUT_DIR)/Benchmarking/Intern/Insert/Insert40 $(BENCHMARKING_DIR)/Intern/Insert/Insert40.hs

benchmark_intern_insert50: benchmarking_intern_deps
	$(GHC) $(GHC_FLAGS) -o $(OUTPUT_DIR)/Benchmarking/Intern/Insert/Insert50 $(BENCHMARKING_DIR)/Intern/Insert/Insert50.hs

benchmark_intern_insert60: benchmarking_intern_deps
	$(GHC) $(GHC_FLAGS) -o $(OUTPUT_DIR)/Benchmarking/Intern/Insert/Insert60 $(BENCHMARKING_DIR)/Intern/Insert/Insert60.hs

benchmarking_intern_deps:
	$(GHC) $(GHC_FLAGS) -no-link $(BENCHMARKING_DIR)/Intern/Operations.hs


.PHONY: clean
clean:
	rm -fr $(OUTPUT_DIR)
