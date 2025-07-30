# Main variables
TEXFILE = main
RDIR = R
FIGDIR = Figures

# Functions file in R directory
FUNCTIONS_R = $(RDIR)/functions.R

# Synthetic experiment R scripts
SYNTHETIC_R = $(wildcard $(RDIR)/Synthetic*.R)
SYNTHETIC_ROUTS = $(SYNTHETIC_R:.R=.Rout)

# Analysis R scripts
ANALYSIS_R = $(wildcard $(RDIR)/Analysis*.R)
ANALYSIS_ROUTS = $(ANALYSIS_R:.R=.Rout)

# Data Viz R script
VIZ_R = $(RDIR)/Data_Viz.R
VIZ_ROUT = $(VIZ_R:.R=.Rout)

# All R scripts and outputs
ALL_R_SCRIPTS = $(SYNTHETIC_R) $(ANALYSIS_R) $(VIZ_R)
ALL_ROUTS = $(SYNTHETIC_ROUTS) $(ANALYSIS_ROUTS) $(VIZ_ROUT)

# Figure files
FIGURES = $(wildcard $(FIGDIR)/*.pdf $(FIGDIR)/*.png $(FIGDIR)/*.eps)

.PHONY: all clean R R-synthetic R-analysis R-viz debug

# Default target - runs all R scripts and compiles LaTeX
all: $(TEXFILE).pdf

# Run all R scripts
R: $(ALL_ROUTS)

# Individual targets for each stage
R-synthetic: $(SYNTHETIC_ROUTS)
R-analysis: $(ANALYSIS_ROUTS)
R-viz: $(VIZ_ROUT)

# Rule for Synthetic R scripts - depend only on functions.R
$(RDIR)/Synthetic%.Rout: $(RDIR)/Synthetic%.R $(FUNCTIONS_R)
	@echo "Running synthetic script: $<"
	@R --slave --vanilla -f $< > $@ 2>&1 || (cat $@ && exit 1)
	@touch $@

# Rule for Analysis R scripts - depend only on functions.R (independent of synthetic)
$(RDIR)/Analysis%.Rout: $(RDIR)/Analysis%.R $(FUNCTIONS_R)
	@echo "Running analysis script: $<"
	@R --slave --vanilla -f $< > $@ 2>&1 || (cat $@ && exit 1)
	@touch $@

# Rule for Data Viz R script - depends on functions.R AND synthetic outputs (but not analysis)
$(VIZ_ROUT): $(VIZ_R) $(FUNCTIONS_R) $(SYNTHETIC_ROUTS)
	@echo "Running data visualization script: $<"
	@R --slave --vanilla -f $< > $@ 2>&1 || (cat $@ && exit 1)
	@touch $@

# R/functions.R is a source file that doesn't need to be built
$(FUNCTIONS_R): ;

# LaTeX compilation - depends on ALL R script outputs
$(TEXFILE).pdf: $(TEXFILE).tex $(ALL_ROUTS) $(FIGURES) References.bib
	@echo "Compiling LaTeX"
	@latexmk -pdf -quiet $(TEXFILE)

clean:
	rm -f $(ALL_ROUTS)
	rm -f *.aux *.log *.toc *.blg *.bbl *.synctex.gz *.out *.bcf *blx.bib *.run.xml
	rm -f *.fdb_latexmk *.fls
	rm -f $(TEXFILE).pdf

# Note: Don't remove figures in clean - they take time to regenerate
clean-all: clean
	rm -f $(FIGURES)

# Debug target to show dependencies and timestamps
debug:
	@echo "=== Makefile Debug Info ==="
	@echo "Functions file: $(FUNCTIONS_R)"
	@echo ""
	@echo "Synthetic R scripts ($(words $(SYNTHETIC_R)) files):"
	@printf "  %s\n" $(SYNTHETIC_R)
	@echo ""
	@echo "Analysis R scripts ($(words $(ANALYSIS_R)) files):"
	@printf "  %s\n" $(ANALYSIS_R)
	@echo ""
	@echo "Data Viz R script: $(VIZ_R)"
	@echo ""
	@echo "Figures: $(words $(FIGURES)) files"
	@echo ""
	@echo "=== Dependency Chain ==="
	@echo "1. Synthetic scripts (independent) → Synthetic .Rout files"
	@echo "2. Analysis scripts (independent) → Analysis .Rout files"
	@echo "3. Data Viz script (depends on synthetic outputs) → Data Viz .Rout file"
	@echo "4. LaTeX compilation (depends on all .Rout files)"
	@echo ""
	@echo "=== File Status ==="
	@echo "Functions file:"
	@stat -c "%Y %n" $(FUNCTIONS_R) 2>/dev/null || echo "$(FUNCTIONS_R) not found"
	@echo ""
	@echo "Synthetic .Rout files:"
	@stat -c "%Y %n" $(SYNTHETIC_ROUTS) 2>/dev/null || echo "No synthetic .Rout files found"
	@echo ""
	@echo "Analysis .Rout files:"
	@stat -c "%Y %n" $(ANALYSIS_ROUTS) 2>/dev/null || echo "No analysis .Rout files found"
	@echo ""
	@echo "Data Viz .Rout file:"
	@stat -c "%Y %n" $(VIZ_ROUT) 2>/dev/null || echo "$(VIZ_ROUT) not found"
	@echo ""
	@echo "LaTeX file:"
	@stat -c "%Y %n" $(TEXFILE).tex 2>/dev/null || echo "$(TEXFILE).tex not found"
	@echo ""
	@echo "PDF output:"
	@stat -c "%Y %n" $(TEXFILE).pdf 2>/dev/null || echo "$(TEXFILE).pdf not found"

# Help target
help:
	@echo "Available targets:"
	@echo "  all         - Run all R scripts and compile LaTeX (default)"
	@echo "  R           - Run all R scripts"
	@echo "  R-synthetic - Run only synthetic experiment scripts"
	@echo "  R-analysis  - Run only analysis scripts (independent of synthetic)"
	@echo "  R-viz       - Run data visualization script (requires synthetic to be done first)"
	@echo "  clean       - Remove generated .Rout and LaTeX auxiliary files"
	@echo "  clean-all   - Remove all generated files including figures"
	@echo "  debug       - Show dependency information and file status"
	@echo "  help        - Show this help message"
	@echo ""
	@echo "Dependencies:"
	@echo "  - Synthetic and Analysis scripts are independent of each other"
	@echo "  - Data Viz depends on Synthetic outputs (but not Analysis)"
	@echo "  - LaTeX depends on all script outputs"
