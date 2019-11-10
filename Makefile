SHELL = /bin/bash

GHC = ghc
GHC_DEBUG_FLAGS = -Wno-safe -Wno-unsafe
GHC_FLAGS = --make -prof -fprof-auto -rtsopts -O2 -outputdir output_dir -Weverything $(GHC_DEBUG_FLAGS)

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


# Benchmarking

benchmarking_extern_deps:
	$(GHC) $(GHC_FLAGS) -no-link $(BENCHMARKING_DIR)/Extern/Operations.hs

benchmarking_fullextern_deps:
	$(GHC) $(GHC_FLAGS) -no-link $(BENCHMARKING_DIR)/FullExtern/Operations.hs

benchmarking_intern_deps:
	$(GHC) $(GHC_FLAGS) -no-link $(BENCHMARKING_DIR)/Intern/Operations.hs


# Benchmarks for insertAVL

# Benchmarks for Extern insertAVL

Extern/Insert/Insert%: benchmarking_extern_deps
	mkdir -p $(OUTPUT_DIR)/Benchmarking/Extern/Insert
	$(GHC) $(GHC_FLAGS) -o $(OUTPUT_DIR)/Benchmarking/$@ $(BENCHMARKING_DIR)/$@.hs


# Benchmarks for FullExtern insertAVL

FullExtern/Insert/Insert%: benchmarking_fullextern_deps
	mkdir -p $(OUTPUT_DIR)/Benchmarking/FullExtern/Insert
	$(GHC) $(GHC_FLAGS) -o $(OUTPUT_DIR)/Benchmarking/$@ $(BENCHMARKING_DIR)/$@.hs


# Benchmarks for Intern insertAVL

Intern/Insert/Insert%: benchmarking_intern_deps
	mkdir -p $(OUTPUT_DIR)/Benchmarking/Intern/Insert
	$(GHC) $(GHC_FLAGS) -o $(OUTPUT_DIR)/Benchmarking/$@ $(BENCHMARKING_DIR)/$@.hs


# Benchmarks for deleteAVL

# Benchmarks for Extern deleteAVL

Extern/Delete/Delete%: benchmarking_extern_deps
	mkdir -p $(OUTPUT_DIR)/Benchmarking/Extern/Delete
	$(GHC) $(GHC_FLAGS) -o $(OUTPUT_DIR)/Benchmarking/$@ $(BENCHMARKING_DIR)/$@.hs


# Benchmarks for FullExtern deleteAVL

FullExtern/Delete/Delete%: benchmarking_fullextern_deps
	mkdir -p $(OUTPUT_DIR)/Benchmarking/FullExtern/Delete
	$(GHC) $(GHC_FLAGS) -o $(OUTPUT_DIR)/Benchmarking/$@ $(BENCHMARKING_DIR)/$@.hs


# Benchmarks for Intern deleteAVL

Intern/Delete/Delete%: benchmarking_intern_deps
	mkdir -p $(OUTPUT_DIR)/Benchmarking/Intern/Delete
	$(GHC) $(GHC_FLAGS) -o $(OUTPUT_DIR)/Benchmarking/$@ $(BENCHMARKING_DIR)/$@.hs


# Benchmarks for lookupAVL

# Benchmarks for Extern lookupAVL

Extern/Lookup/Lookup%: benchmarking_extern_deps
	mkdir -p $(OUTPUT_DIR)/Benchmarking/Extern/Lookup
	$(GHC) $(GHC_FLAGS) -o $(OUTPUT_DIR)/Benchmarking/$@ $(BENCHMARKING_DIR)/$@.hs


# Benchmarks for FullExtern lookupAVL

FullExtern/Lookup/Lookup%: benchmarking_fullextern_deps
	mkdir -p $(OUTPUT_DIR)/Benchmarking/FullExtern/Lookup
	$(GHC) $(GHC_FLAGS) -o $(OUTPUT_DIR)/Benchmarking/$@ $(BENCHMARKING_DIR)/$@.hs


# Benchmarks for Intern lookupAVL

Intern/Lookup/Lookup%: benchmarking_intern_deps
	mkdir -p $(OUTPUT_DIR)/Benchmarking/Intern/Lookup
	$(GHC) $(GHC_FLAGS) -o $(OUTPUT_DIR)/Benchmarking/$@ $(BENCHMARKING_DIR)/$@.hs


.PHONY: clean
clean:
	rm -fr $(OUTPUT_DIR)
