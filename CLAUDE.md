# CLAUDE.md

Guidance for Claude Code working **on** the rerddapXtracto package source (drop this at the repo root of `rmendels/rerddapXtracto`; it currently lives beside `SKILL.md` because no local checkout exists in this environment). rerddapXtracto extracts data along a track (`rxtracto()`), in a 3D box (`rxtracto_3D()`), or within a polygon (`rxtractogon()`) from ERDDAP(TM) servers, built on `rerddap`. R >= 4.0.

## Skill vs. this file

`SKILL.md` in this same directory is LLM-targeted guidance for *writing user code that calls rerddapXtracto*. When helping a user *use* rerddapXtracto, defer to the skill. **This file is for working on the package source itself.**

## Commands

```r
devtools::document()     # roxygen2 -> man/, NAMESPACE
devtools::load_all()     # load for interactive testing
devtools::run_examples(run = TRUE)   # exercise @examples
```

```bash
R CMD build .
R CMD check --as-cran --no-manual rerddapXtracto_*.tar.gz
```

**There is no `tests/testthat` directory in this repo** and no CI workflow (`.github/workflows` doesn't exist here, unlike its sibling `rerddap`). Coverage comes entirely from roxygen `@examples`, most wrapped in `\dontrun{}` or exercised against the bundled example data (see below) so `R CMD check` stays within CRAN time limits without a live server call. Manual verification against a real ERDDAP server is the primary way changes actually get tested — do this before claiming a fix works.

## Codebase Shape (`R/`)

- `rxtracto.R` — the track-extraction entry point; by far the largest/densest file. Owns the outer per-time-step request loop, dateline-crossing dual-extract-and-combine logic, and the output dataframe assembly (`create_out_dataframe`, `populate_dataframe`, `define_bbox`, `combine_extracts`).
- `rxtracto_3D.R` — the bounding-box extractor; simpler than `rxtracto()` but shares its dateline-crossing combine logic (near-duplicated, not factored out — if you fix the dateline bug in one, check the other).
- `rxtractogon.R` — polygon extraction; calls `rxtracto_3D()` internally then masks with `sp::point.in.polygon()`. Grid-only (checks `NC_GLOBAL` for `"Grid"`).
- `checkInput.R` / `checkBounds.R` / `readjustCoords.R` / `remapCoords.R` — the shared pre-flight validation/coordinate-normalization pipeline all three extractors funnel through before making any request.
- `getFIleCoords.R` — note the non-standard capitalization (`FI`, not `Fi`) in the filename itself; the function is `getfileCoords()` (lowercase). Search by function name, not filename, when unsure.
- `interp.R` — `erddap_interp()` (batches requests through ERDDAP's `/convert/interpolate.csv` service) and `check_interp()` (validates the `interp` type/neighborhood strings — the canonical list of valid values lives only here, not in any exported constant).
- `make180.R` / `make360.R` — longitude-convention converters, used throughout the dateline-handling code paths.
- `plotTrack.R` / `plotBBox.R` — thin wrappers around `plotdap::add_tabledap()`/`add_griddap()`; keep these in sync with plotdap's actual function signatures (`plotdap`'s own `CLAUDE.md` documents that side).
- `tidy_grid.R` — reshapes `rxtracto_3D()`/`rxtractogon()` output to long format; requires class `rxtracto3D`.
- `utils.R` — private re-implementation of rerddap's internal helpers (`dimvars`, `getvar`, etc., some suffixed `1` to avoid any accidental collision if both packages' internals were ever in scope together) **plus** `safe_info()` (the only exported function in this file), `findERDDAPcoord()`, `makeCmd()`, and `removeLast()` (the `'last'`/`'last-N'` time-keyword resolver). This helper set is a copy of rerddap's own `utils.R`, not shared — see the note in rerddap's `CLAUDE.md`.
- `dataInfo.R`, `MBsst.R`, `Marlintag38606.R`, `PB_Argos.R`, `mbnms.R`, `swchl.R` — `data/*.rda` documentation stubs only (no logic); these are the pre-fetched example objects that keep `@examples` runnable offline and within CRAN time limits. If you add a new example needing live data, add a matching bundled dataset rather than making a network call in `@examples`.

## Known Gotcha: README Drift From Source

The README documents `latlon_to_xy()`'s signature (from the sibling `rerddapUtils` package, which this package's docs reference) inconsistently with the actual exported function signature in that package's `conversion.R` — argument order and names differ. Don't trust README-documented signatures without checking the actual source; this has already caused at least one skill-authoring mistake caught only by reading `conversion.R` directly (see `rerddapUtils`'s `CLAUDE.md`).

## Packaging Notes

- No `Roxygen: list(markdown = TRUE)` line in `DESCRIPTION` (unlike `rerddap`/`rerddapUtils`) — check current roxygen comment style (mostly plain-text/Rd-style, not markdown) before adding new `@param`/`@details` blocks so new docs match existing ones.
- `LICENSE.note` (not `LICENSE`) is the license file referenced — CC0.
- No formatter/linter config — match surrounding style by hand.
