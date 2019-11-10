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


# Benchmarking

benchmarking_extern_deps:
	$(GHC) $(GHC_FLAGS) -no-link $(BENCHMARKING_DIR)/Extern/Operations.hs

benchmarking_fullextern_deps:
	$(GHC) $(GHC_FLAGS) -no-link $(BENCHMARKING_DIR)/FullExtern/Operations.hs

benchmarking_intern_deps:
	$(GHC) $(GHC_FLAGS) -no-link $(BENCHMARKING_DIR)/Intern/Operations.hs


# Benchmarks for insertAVL

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


# Benchmarks for deleteAVL

# Benchmarks for Extern deleteAVL

benchmark_extern_delete10: benchmarking_extern_deps
	$(GHC) $(GHC_FLAGS) -o $(OUTPUT_DIR)/Benchmarking/Extern/Delete/Delete10 $(BENCHMARKING_DIR)/Extern/Delete/Delete10.hs

benchmark_extern_delete20: benchmarking_extern_deps
	$(GHC) $(GHC_FLAGS) -o $(OUTPUT_DIR)/Benchmarking/Extern/Delete/Delete20 $(BENCHMARKING_DIR)/Extern/Delete/Delete20.hs

benchmark_extern_delete30: benchmarking_extern_deps
	$(GHC) $(GHC_FLAGS) -o $(OUTPUT_DIR)/Benchmarking/Extern/Delete/Delete30 $(BENCHMARKING_DIR)/Extern/Delete/Delete30.hs

benchmark_extern_delete40: benchmarking_extern_deps
	$(GHC) $(GHC_FLAGS) -o $(OUTPUT_DIR)/Benchmarking/Extern/Delete/Delete40 $(BENCHMARKING_DIR)/Extern/Delete/Delete40.hs

benchmark_extern_delete50: benchmarking_extern_deps
	$(GHC) $(GHC_FLAGS) -o $(OUTPUT_DIR)/Benchmarking/Extern/Delete/Delete50 $(BENCHMARKING_DIR)/Extern/Delete/Delete50.hs

benchmark_extern_delete60: benchmarking_extern_deps
	$(GHC) $(GHC_FLAGS) -o $(OUTPUT_DIR)/Benchmarking/Extern/Delete/Delete60 $(BENCHMARKING_DIR)/Extern/Delete/Delete60.hs


# Benchmarks for FullExtern deleteAVL

benchmark_fullextern_delete10: benchmarking_fullextern_deps
	$(GHC) $(GHC_FLAGS) -o $(OUTPUT_DIR)/Benchmarking/FullExtern/Delete/Delete10 $(BENCHMARKING_DIR)/FullExtern/Delete/Delete10.hs

benchmark_fullextern_delete20: benchmarking_fullextern_deps
	$(GHC) $(GHC_FLAGS) -o $(OUTPUT_DIR)/Benchmarking/FullExtern/Delete/Delete20 $(BENCHMARKING_DIR)/FullExtern/Delete/Delete20.hs

benchmark_fullextern_delete30: benchmarking_fullextern_deps
	$(GHC) $(GHC_FLAGS) -o $(OUTPUT_DIR)/Benchmarking/FullExtern/Delete/Delete30 $(BENCHMARKING_DIR)/FullExtern/Delete/Delete30.hs

benchmark_fullextern_delete40: benchmarking_fullextern_deps
	$(GHC) $(GHC_FLAGS) -o $(OUTPUT_DIR)/Benchmarking/FullExtern/Delete/Delete40 $(BENCHMARKING_DIR)/FullExtern/Delete/Delete40.hs

benchmark_fullextern_delete50: benchmarking_fullextern_deps
	$(GHC) $(GHC_FLAGS) -o $(OUTPUT_DIR)/Benchmarking/FullExtern/Delete/Delete50 $(BENCHMARKING_DIR)/FullExtern/Delete/Delete50.hs

benchmark_fullextern_delete60: benchmarking_fullextern_deps
	$(GHC) $(GHC_FLAGS) -o $(OUTPUT_DIR)/Benchmarking/FullExtern/Delete/Delete60 $(BENCHMARKING_DIR)/FullExtern/Delete/Delete60.hs


# Benchmarks for Intern deleteAVL

benchmark_intern_delete10: benchmarking_intern_deps
	$(GHC) $(GHC_FLAGS) -o $(OUTPUT_DIR)/Benchmarking/Intern/Delete/Delete10 $(BENCHMARKING_DIR)/Intern/Delete/Delete10.hs

benchmark_intern_delete20: benchmarking_intern_deps
	$(GHC) $(GHC_FLAGS) -o $(OUTPUT_DIR)/Benchmarking/Intern/Delete/Delete20 $(BENCHMARKING_DIR)/Intern/Delete/Delete20.hs

benchmark_intern_delete30: benchmarking_intern_deps
	$(GHC) $(GHC_FLAGS) -o $(OUTPUT_DIR)/Benchmarking/Intern/Delete/Delete30 $(BENCHMARKING_DIR)/Intern/Delete/Delete30.hs

benchmark_intern_delete40: benchmarking_intern_deps
	$(GHC) $(GHC_FLAGS) -o $(OUTPUT_DIR)/Benchmarking/Intern/Delete/Delete40 $(BENCHMARKING_DIR)/Intern/Delete/Delete40.hs

benchmark_intern_delete50: benchmarking_intern_deps
	$(GHC) $(GHC_FLAGS) -o $(OUTPUT_DIR)/Benchmarking/Intern/Delete/Delete50 $(BENCHMARKING_DIR)/Intern/Delete/Delete50.hs

benchmark_intern_delete60: benchmarking_intern_deps
	$(GHC) $(GHC_FLAGS) -o $(OUTPUT_DIR)/Benchmarking/Intern/Delete/Delete60 $(BENCHMARKING_DIR)/Intern/Delete/Delete60.hs


# Benchmarks for lookupAVL

# Benchmarks for Extern lookupAVL

benchmark_extern_lookup10: benchmarking_extern_deps
	$(GHC) $(GHC_FLAGS) -o $(OUTPUT_DIR)/Benchmarking/Extern/Lookup/Lookup10 $(BENCHMARKING_DIR)/Extern/Lookup/Lookup10.hs

benchmark_extern_lookup20: benchmarking_extern_deps
	$(GHC) $(GHC_FLAGS) -o $(OUTPUT_DIR)/Benchmarking/Extern/Lookup/Lookup20 $(BENCHMARKING_DIR)/Extern/Lookup/Lookup20.hs

benchmark_extern_lookup30: benchmarking_extern_deps
	$(GHC) $(GHC_FLAGS) -o $(OUTPUT_DIR)/Benchmarking/Extern/Lookup/Lookup30 $(BENCHMARKING_DIR)/Extern/Lookup/Lookup30.hs

benchmark_extern_lookup40: benchmarking_extern_deps
	$(GHC) $(GHC_FLAGS) -o $(OUTPUT_DIR)/Benchmarking/Extern/Lookup/Lookup40 $(BENCHMARKING_DIR)/Extern/Lookup/Lookup40.hs

benchmark_extern_lookup50: benchmarking_extern_deps
	$(GHC) $(GHC_FLAGS) -o $(OUTPUT_DIR)/Benchmarking/Extern/Lookup/Lookup50 $(BENCHMARKING_DIR)/Extern/Lookup/Lookup50.hs

benchmark_extern_lookup60: benchmarking_extern_deps
	$(GHC) $(GHC_FLAGS) -o $(OUTPUT_DIR)/Benchmarking/Extern/Lookup/Lookup60 $(BENCHMARKING_DIR)/Extern/Lookup/Lookup60.hs


# Benchmarks for FullExtern lookupAVL

benchmark_fullextern_lookup10: benchmarking_fullextern_deps
	$(GHC) $(GHC_FLAGS) -o $(OUTPUT_DIR)/Benchmarking/FullExtern/Lookup/Lookup10 $(BENCHMARKING_DIR)/FullExtern/Lookup/Lookup10.hs

benchmark_fullextern_lookup20: benchmarking_fullextern_deps
	$(GHC) $(GHC_FLAGS) -o $(OUTPUT_DIR)/Benchmarking/FullExtern/Lookup/Lookup20 $(BENCHMARKING_DIR)/FullExtern/Lookup/Lookup20.hs

benchmark_fullextern_lookup30: benchmarking_fullextern_deps
	$(GHC) $(GHC_FLAGS) -o $(OUTPUT_DIR)/Benchmarking/FullExtern/Lookup/Lookup30 $(BENCHMARKING_DIR)/FullExtern/Lookup/Lookup30.hs

benchmark_fullextern_lookup40: benchmarking_fullextern_deps
	$(GHC) $(GHC_FLAGS) -o $(OUTPUT_DIR)/Benchmarking/FullExtern/Lookup/Lookup40 $(BENCHMARKING_DIR)/FullExtern/Lookup/Lookup40.hs

benchmark_fullextern_lookup50: benchmarking_fullextern_deps
	$(GHC) $(GHC_FLAGS) -o $(OUTPUT_DIR)/Benchmarking/FullExtern/Lookup/Lookup50 $(BENCHMARKING_DIR)/FullExtern/Lookup/Lookup50.hs

benchmark_fullextern_lookup60: benchmarking_fullextern_deps
	$(GHC) $(GHC_FLAGS) -o $(OUTPUT_DIR)/Benchmarking/FullExtern/Lookup/Lookup60 $(BENCHMARKING_DIR)/FullExtern/Lookup/Lookup60.hs


# Benchmarks for Intern lookupAVL

benchmark_intern_lookup10: benchmarking_intern_deps
	$(GHC) $(GHC_FLAGS) -o $(OUTPUT_DIR)/Benchmarking/Intern/Lookup/Lookup10 $(BENCHMARKING_DIR)/Intern/Lookup/Lookup10.hs

benchmark_intern_lookup20: benchmarking_intern_deps
	$(GHC) $(GHC_FLAGS) -o $(OUTPUT_DIR)/Benchmarking/Intern/Lookup/Lookup20 $(BENCHMARKING_DIR)/Intern/Lookup/Lookup20.hs

benchmark_intern_lookup30: benchmarking_intern_deps
	$(GHC) $(GHC_FLAGS) -o $(OUTPUT_DIR)/Benchmarking/Intern/Lookup/Lookup30 $(BENCHMARKING_DIR)/Intern/Lookup/Lookup30.hs

benchmark_intern_lookup40: benchmarking_intern_deps
	$(GHC) $(GHC_FLAGS) -o $(OUTPUT_DIR)/Benchmarking/Intern/Lookup/Lookup40 $(BENCHMARKING_DIR)/Intern/Lookup/Lookup40.hs

benchmark_intern_lookup50: benchmarking_intern_deps
	$(GHC) $(GHC_FLAGS) -o $(OUTPUT_DIR)/Benchmarking/Intern/Lookup/Lookup50 $(BENCHMARKING_DIR)/Intern/Lookup/Lookup50.hs

benchmark_intern_lookup60: benchmarking_intern_deps
	$(GHC) $(GHC_FLAGS) -o $(OUTPUT_DIR)/Benchmarking/Intern/Lookup/Lookup60 $(BENCHMARKING_DIR)/Intern/Lookup/Lookup60.hs


.PHONY: clean
clean:
	rm -fr $(OUTPUT_DIR)
