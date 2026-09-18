## CRAN submission comments

This is a resubmission of version `0.18.5`.

### Test environments

* macOS (GitHub Actions, R release)
* Windows (GitHub Actions, R release)
* Ubuntu (GitHub Actions, R release, oldrel-1, and devel)
* Local macOS, R 4.2.3

### R CMD check results

`R CMD check --as-cran --no-manual` is run on the GitHub Actions matrix and
treats warnings as failures. A separate Linux release job installs TinyTeX and
builds the reference manual. The local check is run before submission where the
CRAN incoming network checks are available.

The reference manual was also built locally with R 4.2.3 and TinyTeX.

### Optional dependencies

`plumber`, `jsonlite`, `strategyr`, `fst`, `ggplot2`, `lubridate`, and `zoo`
are optional `Suggests`. The core package loads and runs without them. Local
service, JSON export, dashboard, data-adapter, and legacy helper functions
check availability at runtime and provide an explicit installation message.
The two `strategyr` integration tests are intentionally skipped on CRAN; their
adapter behavior is covered by package-local strategy contract tests.
The Vox-scale 66-account and profiling regressions are likewise skipped on
CRAN to keep incoming checks within the time limit. Smaller batch, replay,
ledger, save/load, and export parity tests remain part of every CRAN run.

### Scope

The package is an R-native simulated execution and accounting engine. It does
not connect to brokerages, obtain credentials, access network market-data
providers, or start a network service during installation, examples, or tests.
Its CRAN-core contract is documented in `inst/CRAN-CORE.md`.
