SHELL := /bin/bash

# Variables
TEXFILE = main
RDIR = R
TEX_FILES = $(wildcard *.tex)
BIB_FILES = $(wildcard *.bib)
R_SCRIPTS = $(wildcard $(RDIR)/*.R)

.PHONY: all update clean clean-latex clean-figures

# Default target
all: $(TEXFILE).pdf supplement.pdf

# Update all R packages to their latest versions. lookout is dropped from the
# project and re-added, because `uvr update` only moves a GitHub dependency when
# its version number changes, and lookout's does not change on every commit.
update:
	source .uvr/activate && \
	  uvr remove lookout && rm -rf .uvr/library/lookout && \
	  uvr update && \
	  uvr add sevvandi/lookout && \
	  uvr sync

# Generate figures using R targets package
figures: $(R_SCRIPTS) _targets.R
	Rscript -e "targets::tar_make()"
	@touch figures # Create timestamp file

# Main PDF compilation using latexmk
$(TEXFILE).pdf: $(TEX_FILES) $(BIB_FILES) figures
	latexmk -pdf -quiet $(TEXFILE)

# Online appendix
supplement.pdf: supplement.tex 7_proofs.tex $(BIB_FILES)
	latexmk -pdf -quiet supplement

# Clean auxiliary LaTeX files (using latexmk)
clean-latex:
	latexmk -C $(TEXFILE).tex supplement.tex

# Clean R targets cache
clean-figures:
	Rscript -e "targets::tar_destroy()"
	rm -f figures

# Full clean
clean: clean-latex clean-figures
