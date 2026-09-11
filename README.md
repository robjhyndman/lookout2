# lookout 2

This project uses [`uvr`](https://github.com/nbafrank/uvr) to manage R package versions, and the [`targets`](https://docs.ropensci.org/targets/) package to manage the R workflow.

If you do not already have `uvr`, install it once. On macOS and Linux:

```bash
curl -fsSL https://raw.githubusercontent.com/nbafrank/uvr/main/install.sh | sh
```

On Windows, use the PowerShell installer given in the `uvr` README.

To set up, from the repository root:

```bash
uvr sync
```

That installs every package listed in `uvr.lock` into `.uvr/library`, which the project `.Rprofile` puts on the library path. Any R session started from the repository root then sees the right packages.

To run the workflow from within R:

```r
targets::tar_make()
```

To run the workflow from the command line, you can use the provided `Makefile`. This will execute the R scripts and compile the LaTeX document.

```bash
make
```
