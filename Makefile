SHELL := /bin/bash

# Variables
TEXFILE = main
RDIR = R
TEX_FILES = $(wildcard *.tex)
BIB_FILES = $(wildcard *.bib)
R_SCRIPTS = $(wildcard $(RDIR)/*.R)

.PHONY: all update clean clean-latex clean-figures sync-desktop sync-laptop

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

# Main PDF compilation using ratex; --keep-logs writes main.log beside the PDF
$(TEXFILE).pdf: $(TEX_FILES) $(BIB_FILES) figures
	ratex --keep-logs $(TEXFILE).tex

# Online appendix
supplement.pdf: supplement.tex 7_proofs.tex $(BIB_FILES)
	ratex --keep-logs supplement.tex

# Copy the targets store, figures and tables from another machine instead of
# running the pipeline here. Both machines must hold the repository at the same
# path relative to the home directory. The ssh options override RemoteCommand
# and RequestTTY in ~/.ssh/config, which break rsync. Touching the figures
# stamp stops make from rerunning tar_make().
SYNC_DIRS = _targets Figures Data_Output
REMOTE_DIR = $(patsubst $(HOME)/%,%,$(CURDIR))
RSH = ssh -o RemoteCommand=none -o RequestTTY=no

sync-desktop sync-laptop: sync-%:
	rsync -av -e "$(RSH)" $(addprefix $*:$(REMOTE_DIR)/,$(SYNC_DIRS)) ./
	@touch figures

# Clean ratex build state and the PDFs it produced
clean-latex:
	ratex -C $(TEXFILE).tex
	ratex -C supplement.tex

# Clean R targets cache
clean-figures:
	Rscript -e "targets::tar_destroy()"
	rm -f figures

# Full clean
clean: clean-latex clean-figures
