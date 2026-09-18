# Lookout 2: Anomaly Detection via Leave-One-Out Kernel Density Estimation

Source for the paper by Rob J Hyndman, Sevvandi Kandanaarachchi and Katharine Turner, and the code that produces its figures and tables. The algorithm is implemented in the [`lookout`](https://sevvandi.github.io/lookout/) R package.

`main.tex` and the numbered `.tex` files are the paper; `supplement.tex` builds the online appendix. The R scripts are in `R/`, run by the [`targets`](https://docs.ropensci.org/targets/) pipeline in `_targets.R`; they write the figures to `Figures/` and the tables to `Data_Output/`. Experiments are numbered as in the paper, so `R/Experiment3.R` produces the paper's Experiment 3 and `R/ExperimentS1.R` Experiment S1 of the appendix.

## Reproducing the results

R packages are managed by [`uvr`](https://github.com/nbafrank/uvr); see its page for installation. Then, from the repository root, `uvr sync` installs the packages pinned in `uvr.lock`, including the `lookout` package at the commit used for the paper. In R, `targets::tar_make()` runs the pipeline, which takes about an hour, and `latexmk -pdf main` and `latexmk -pdf supplement` compile the two documents. `make` does all of this.

The random seeds are fixed, so a rerun reproduces the figures exactly. The timing table records elapsed seconds on the machine it runs on; the paper's numbers come from an Intel Core Ultra 9 285.

## Data

The Old Faithful eruption records were downloaded from <https://geysertimes.org> and are distributed in the `weird` package as `oldfaithful`. The wine reviews were downloaded from <https://www.kaggle.com/datasets/zynicide/wine-reviews/data> and are fetched by `weird::fetch_wine_reviews()`.
