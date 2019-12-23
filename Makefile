SHELL = /bin/bash

GHC = ghc
GHC_DEBUG_FLAGS =#-Weverything -Wno-safe -Wno-unsafe
GHC_PROF_FLAGS =#-prof -fprof-auto -rtsopts -O2
GHC_COMPILER_FLAGS = -ddump-to-file -dshow-passes
GHC_FLAGS = --make -outputdir output_dir -freduction-depth=0 -dynamic# -fomit-interface-pragmas

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
	$(GHC) $(GHC_FLAGS) $(GHC_DEBUG_FLAGS) $(GHC_PROF_FLAGS) -no-link $(BENCHMARKING_DIR)/Extern/Operations.hs

benchmarking_fullextern_deps:
	mkdir -p $(OUTPUT_DIR)/Benchmarking/FullExtern/Insert
	mkdir -p $(OUTPUT_DIR)/Benchmarking/FullExtern/Delete
	mkdir -p $(OUTPUT_DIR)/Benchmarking/FullExtern/Lookup
	$(GHC) $(GHC_FLAGS) $(GHC_DEBUG_FLAGS) $(GHC_PROF_FLAGS) -no-link $(BENCHMARKING_DIR)/FullExtern/Operations.hs

benchmarking_intern_deps:
	mkdir -p $(OUTPUT_DIR)/Benchmarking/Intern/Insert
	mkdir -p $(OUTPUT_DIR)/Benchmarking/Intern/Delete
	mkdir -p $(OUTPUT_DIR)/Benchmarking/Intern/Lookup
	$(GHC) $(GHC_FLAGS) $(GHC_DEBUG_FLAGS) $(GHC_PROF_FLAGS) -no-link $(BENCHMARKING_DIR)/Intern/Operations.hs


# Benchmarks for insertAVL

# Benchmarks for Extern insertAVL
Extern/Insert/Insert%: benchmarking_extern_deps
	$(GHC) $(GHC_FLAGS) -main-is Benchmarking.Extern.Insert.Insert$* -o $(OUTPUT_DIR)/Benchmarking/$@ $(BENCHMARKING_DIR)/$@.hs
	rm -fv $(OUTPUT_DIR)/Main*

# Benchmarks for FullExtern insertAVL
FullExtern/Insert/Insert%: benchmarking_fullextern_deps
	$(GHC) $(GHC_FLAGS) -main-is Benchmarking.FullExtern.Insert.Insert$* -o $(OUTPUT_DIR)/Benchmarking/$@ $(BENCHMARKING_DIR)/$@.hs
	rm -fv $(OUTPUT_DIR)/Main*

# Benchmarks for Intern insertAVL
Intern/Insert/Insert%: benchmarking_intern_deps
	$(GHC) $(GHC_FLAGS) -main-is Benchmarking.Intern.Insert.Insert$* -o $(OUTPUT_DIR)/Benchmarking/$@ $(BENCHMARKING_DIR)/$@.hs
	rm -fv $(OUTPUT_DIR)/Main*


# Benchmarks for deleteAVL

# Benchmarks for Extern deleteAVL
Extern/Delete/Delete%: benchmarking_extern_deps
	$(GHC) $(GHC_FLAGS) -main-is Benchmarking.Extern.Delete.Delete$* -o $(OUTPUT_DIR)/Benchmarking/$@ $(BENCHMARKING_DIR)/$@.hs
	rm -fv $(OUTPUT_DIR)/Main*

# Benchmarks for FullExtern deleteAVL
FullExtern/Delete/Delete%: benchmarking_fullextern_deps
	$(GHC) $(GHC_FLAGS) -main-is Benchmarking.FullExtern.Delete.Delete$* -o $(OUTPUT_DIR)/Benchmarking/$@ $(BENCHMARKING_DIR)/$@.hs
	rm -fv $(OUTPUT_DIR)/Main*

# Benchmarks for Intern deleteAVL
Intern/Delete/Delete%: benchmarking_intern_deps
	$(GHC) $(GHC_FLAGS) -main-is Benchmarking.Intern.Delete.Delete$* -o $(OUTPUT_DIR)/Benchmarking/$@ $(BENCHMARKING_DIR)/$@.hs
	rm -fv $(OUTPUT_DIR)/Main*


# Benchmarks for lookupAVL

# Benchmarks for Extern lookupAVL
Extern/Lookup/Lookup%: benchmarking_extern_deps
	$(GHC) $(GHC_FLAGS) -main-is Benchmarking.Extern.Lookup.Lookup$* -o $(OUTPUT_DIR)/Benchmarking/$@ $(BENCHMARKING_DIR)/$@.hs
	rm -fv $(OUTPUT_DIR)/Main*

# Benchmarks for FullExtern lookupAVL
FullExtern/Lookup/Lookup%: benchmarking_fullextern_deps
	$(GHC) $(GHC_FLAGS) -main-is Benchmarking.FullExtern.Lookup.Lookup$* -o $(OUTPUT_DIR)/Benchmarking/$@ $(BENCHMARKING_DIR)/$@.hs
	rm -fv $(OUTPUT_DIR)/Main*

# Benchmarks for Intern lookupAVL
Intern/Lookup/Lookup%: benchmarking_intern_deps
	$(GHC) $(GHC_FLAGS) -main-is Benchmarking.Intern.Lookup.Lookup$* -o $(OUTPUT_DIR)/Benchmarking/$@ $(BENCHMARKING_DIR)/$@.hs
	rm -fv $(OUTPUT_DIR)/Main*


# Benchmarks for Extern
Extern/Benchmark: benchmarking_extern_deps
	$(GHC) $(GHC_FLAGS) $(GHC_PROF_FLAGS) -o $(OUTPUT_DIR)/Benchmarking/$@ $(BENCHMARKING_DIR)/$@.hs

# Benchmarks for FullExtern
FullExtern/Benchmark: benchmarking_fullextern_deps
	$(GHC) $(GHC_FLAGS) $(GHC_PROF_FLAGS) -o $(OUTPUT_DIR)/Benchmarking/$@ $(BENCHMARKING_DIR)/$@.hs

# Benchmarks for Intern
Intern/Benchmark: benchmarking_intern_deps
	$(GHC) $(GHC_FLAGS) $(GHC_PROF_FLAGS) -o $(OUTPUT_DIR)/Benchmarking/$@ $(BENCHMARKING_DIR)/$@.hs


.PHONY: clean
clean:
	rm -fvr $(OUTPUT_DIR)
