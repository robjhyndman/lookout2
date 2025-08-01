# Main variables
TEXFILE = main
RDIR = R
FIGDIR = Figures

# All tex files - main.tex and any other .tex files it might include
TEX_FILES = $(wildcard *.tex)

# Functions file in R directory
FUNCTIONS_R = $(RDIR)/functions.R

# Synthetic experiment R scripts
SYNTHETIC_R = $(wildcard $(RDIR)/Synthetic*.R)
SYNTHETIC_ROUTS = $(SYNTHETIC_R:.R=.Rout)

# R scripts to produce figures
VIZ_R = $(wildcard $(RDIR)/Figure*.R)
VIZ_ROUT = $(VIZ_R:.R=.Rout)

# Other R scripts excluding functions.R
ANALYSIS_R = $(filter-out $(SYNTHETIC_R) $(VIZ_R) $(FUNCTIONS_R), $(wildcard $(RDIR)/*.R))
ANALYSIS_ROUTS = $(ANALYSIS_R:.R=.Rout)

# All R scripts and outputs
ALL_R_SCRIPTS = $(SYNTHETIC_R) $(ANALYSIS_R) $(VIZ_R)
ALL_ROUTS = $(SYNTHETIC_ROUTS) $(ANALYSIS_ROUTS) $(VIZ_ROUT)

# Figure files
FIGURES = $(wildcard $(FIGDIR)/*.pdf $(FIGDIR)/*.png $(FIGDIR)/*.eps)

.PHONY: all clean R

# Default target - runs all R scripts and compiles LaTeX
all: $(TEXFILE).pdf

# Run all R scripts
R: $(ALL_ROUTS)

# Rule for Synthetic R scripts - depend only on functions.R
$(SYNTHETIC_ROUTS): %.Rout: %.R $(FUNCTIONS_R)
	@echo "Running synthetic script: $<"
	@R --vanilla -f $< > $@ 2>&1 || (cat $@ && exit 1)
	@touch $@

# Rule for generating figures - depends on functions.R AND synthetic outputs
$(VIZ_ROUT): %.Rout: %.R $(FUNCTIONS_R) $(SYNTHETIC_ROUTS)
	@echo "Running data visualization script: $<"
	@R --vanilla -f $< > $@ 2>&1 || (cat $@ && exit 1)
	@touch $@

# Rule for other R scripts - depend on functions.R and synthetic outputs
$(ANALYSIS_ROUTS): %.Rout: %.R $(FUNCTIONS_R) $(SYNTHETIC_ROUTS)
	@echo "Running analysis script: $<"
	@R --vanilla -f $< > $@ 2>&1 || (cat $@ && exit 1)
	@touch $@

# R/functions.R is a source file that doesn't need to be built
$(FUNCTIONS_R): ;

# LaTeX compilation - depends on ALL .tex files, R script outputs, figures and references
$(TEXFILE).pdf: $(TEX_FILES) $(ALL_ROUTS) $(FIGURES) References.bib
	@echo "Compiling LaTeX"
	@latexmk -pdf -quiet $(TEXFILE)

clean:
	rm -f $(ALL_ROUTS)
	rm -f *.aux *.log *.toc *.blg *.bbl *.synctex.gz *.out *.bcf *blx.bib *.run.xml
	rm -f *.fdb_latexmk *.fls
	rm -f $(TEXFILE).pdf
