# lookout 2

This project uses the [`renv`](https://rstudio.github.io/renv/) package to manage R package versions, and the [`targets`](https://docs.ropensci.org/targets/) package to manage the R workflow.

To set up:

```r
renv::restore()
```

To run the workflow from within R:

```r
targets::tar_make()
```

To run the workflow from the command line, you can use the provided `Makefile`. This will execute the R scripts and compile the LaTeX document.

```bash
make
```
