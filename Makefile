# Variables
TEXFILE = main
RDIR = R
TEX_FILES = $(wildcard *.tex)
BIB_FILES = $(wildcard *.bib)
R_SCRIPTS = $(wildcard $(RDIR)/*.R)

# Default target
all: $(TEXFILE).pdf

# Generate figures using R targets package
figures: $(R_SCRIPTS) _targets.R
	Rscript -e "targets::tar_make()"
	@touch figures # Create timestamp file

# Main PDF compilation using latexmk
$(TEXFILE).pdf: $(TEX_FILES) $(BIB_FILES) figures
	latexmk -pdf -quiet $(TEXFILE)

# Clean auxiliary LaTeX files (using latexmk)
clean-latex:
	latexmk -C $(TEXFILE).tex

# Clean R targets cache
clean-figures:
	Rscript -e "targets::tar_destroy()"
	rm -f figures

# Full clean
clean: clean-latex clean-figures
