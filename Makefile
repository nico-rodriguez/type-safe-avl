SHELL = /bin/bash

GHC = ghc
GHC_DEBUG_FLAGS = -Wno-safe -Wno-unsafe
GHC_FLAGS = --make -prof -fprof-auto -rtsopts -O2 -outputdir output_dir -freduction-depth=0 -Weverything $(GHC_DEBUG_FLAGS)

BENCHMARKING_DIR = Benchmarking
EXTERN_DIR = Extern
INTERN_DIR = Intern
OUTPUT_DIR = output_dir

.PHONY: all
all: clean benchmarking


# Benchmarking

benchmarking_extern_deps:
	mkdir -p $(OUTPUT_DIR)/Benchmarking/Extern/Insert
	mkdir -p $(OUTPUT_DIR)/Benchmarking/Extern/Delete
	mkdir -p $(OUTPUT_DIR)/Benchmarking/Extern/Lookup
	$(GHC) $(GHC_FLAGS) -no-link $(BENCHMARKING_DIR)/Extern/Operations.hs

benchmarking_fullextern_deps:
	mkdir -p $(OUTPUT_DIR)/Benchmarking/FullExtern/Insert
	mkdir -p $(OUTPUT_DIR)/Benchmarking/FullExtern/Delete
	mkdir -p $(OUTPUT_DIR)/Benchmarking/FullExtern/Lookup
	$(GHC) $(GHC_FLAGS) -no-link $(BENCHMARKING_DIR)/FullExtern/Operations.hs

benchmarking_intern_deps:
	mkdir -p $(OUTPUT_DIR)/Benchmarking/Intern/Insert
	mkdir -p $(OUTPUT_DIR)/Benchmarking/Intern/Delete
	mkdir -p $(OUTPUT_DIR)/Benchmarking/Intern/Lookup
	$(GHC) $(GHC_FLAGS) -no-link $(BENCHMARKING_DIR)/Intern/Operations.hs


# Benchmarks for insertAVL

# Benchmarks for Extern insertAVL
Extern/Insert/Insert%: benchmarking_extern_deps
	$(GHC) $(GHC_FLAGS) -o $(OUTPUT_DIR)/Benchmarking/$@ $(BENCHMARKING_DIR)/$@.hs
	rm -f $(OUTPUT_DIR)/Main*

# Benchmarks for FullExtern insertAVL
FullExtern/Insert/Insert%: benchmarking_fullextern_deps
	$(GHC) $(GHC_FLAGS) -o $(OUTPUT_DIR)/Benchmarking/$@ $(BENCHMARKING_DIR)/$@.hs
	rm -f $(OUTPUT_DIR)/Main*

# Benchmarks for Intern insertAVL
Intern/Insert/Insert%: benchmarking_intern_deps
	$(GHC) $(GHC_FLAGS) -o $(OUTPUT_DIR)/Benchmarking/$@ $(BENCHMARKING_DIR)/$@.hs
	rm -f $(OUTPUT_DIR)/Main*


# Benchmarks for deleteAVL

# Benchmarks for Extern deleteAVL
Extern/Delete/Delete%: benchmarking_extern_deps
	$(GHC) $(GHC_FLAGS) -o $(OUTPUT_DIR)/Benchmarking/$@ $(BENCHMARKING_DIR)/$@.hs
	rm -f $(OUTPUT_DIR)/Main*

# Benchmarks for FullExtern deleteAVL
FullExtern/Delete/Delete%: benchmarking_fullextern_deps
	$(GHC) $(GHC_FLAGS) -o $(OUTPUT_DIR)/Benchmarking/$@ $(BENCHMARKING_DIR)/$@.hs
	rm -f $(OUTPUT_DIR)/Main*

# Benchmarks for Intern deleteAVL
Intern/Delete/Delete%: benchmarking_intern_deps
	$(GHC) $(GHC_FLAGS) -o $(OUTPUT_DIR)/Benchmarking/$@ $(BENCHMARKING_DIR)/$@.hs
	rm -f $(OUTPUT_DIR)/Main*


# Benchmarks for lookupAVL

# Benchmarks for Extern lookupAVL
Extern/Lookup/Lookup%: benchmarking_extern_deps
	$(GHC) $(GHC_FLAGS) -o $(OUTPUT_DIR)/Benchmarking/$@ $(BENCHMARKING_DIR)/$@.hs
	rm -f $(OUTPUT_DIR)/Main*

# Benchmarks for FullExtern lookupAVL
FullExtern/Lookup/Lookup%: benchmarking_fullextern_deps
	$(GHC) $(GHC_FLAGS) -o $(OUTPUT_DIR)/Benchmarking/$@ $(BENCHMARKING_DIR)/$@.hs
	rm -f $(OUTPUT_DIR)/Main*

# Benchmarks for Intern lookupAVL
Intern/Lookup/Lookup%: benchmarking_intern_deps
	$(GHC) $(GHC_FLAGS) -o $(OUTPUT_DIR)/Benchmarking/$@ $(BENCHMARKING_DIR)/$@.hs
	rm -f $(OUTPUT_DIR)/Main*


# Benchmarks for Extern
Extern/Benchmark: benchmarking_extern_deps
	$(GHC) $(GHC_FLAGS) -o $(OUTPUT_DIR)/Benchmarking/$@ $(BENCHMARKING_DIR)/$@.hs

# Benchmarks for FullExtern
FullExtern/Benchmark: benchmarking_fullextern_deps
	$(GHC) $(GHC_FLAGS) -o $(OUTPUT_DIR)/Benchmarking/$@ $(BENCHMARKING_DIR)/$@.hs

# Benchmarks for Intern
Intern/Benchmark: benchmarking_intern_deps
	$(GHC) $(GHC_FLAGS) -o $(OUTPUT_DIR)/Benchmarking/$@ $(BENCHMARKING_DIR)/$@.hs


.PHONY: clean
clean:
	rm -fr $(OUTPUT_DIR)
