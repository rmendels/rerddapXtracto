---
name: rerddapXtracto
description: Help users write correct R code with rerddapXtracto to extract satellite/oceanographic data from ERDDAP(TM) servers along a track (rxtracto), in a 3D bounding box (rxtracto_3D), or within a polygon (rxtractogon), and to plot or reshape the results. Use when users call rxtracto(), rxtracto_3D(), rxtractogon(), safe_info(), plotTrack(), plotBBox(), tidy_grid(), or need the ERDDAP interpolation-service option for large tracks.
metadata:
  author: Roy Mendelssohn (@rmendels)
  version: "1.0"
license: CC0
---

rerddapXtracto builds on `rerddap` to pull satellite/oceanographic data out of ERDDAP(TM) servers in three shapes: along a moving track with a search radius, in a fixed 3D bounding box, or within a polygon through time. Two companion functions plot the results, one reshapes them to long format.

## Core Principle: info() Feeds All Three Extractors

All three extraction functions take the same first argument: an `info` object, normally from `rerddap::info()`. This package adds `safe_info()`, which returns `NULL` instead of throwing when the server is unreachable — use it in any unattended script (cron job, batch pipeline, vignette/CRAN examples).

```r
library(rerddapXtracto)

dataInfo <- safe_info('erdMBsstd1day')        # NULL, not an error, if the server is down
if (is.null(dataInfo)) stop('ERDDAP server unavailable')

box <- rxtracto_3D(dataInfo, parameter = 'sst',
  xcoord = c(230, 235), ycoord = c(33, 36),
  tcoord = c('2006-01-15', '2006-01-20'))
```

**Wrong pattern:** calling `rerddap::info()` directly inside a script meant to run unattended — a transient server hiccup becomes an uncaught error instead of a clean `NULL` you can check for.

## Three Extraction Shapes

| Function | Shape of request | Returns |
|---|---|---|
| `rxtracto()` | moving point + search radius, one row per (x,y,t) | data.frame: mean/stdev/median/mad/n per point (class `rxtractoTrack`) |
| `rxtracto_3D()` | fixed (x,y,t) bounding box | list with a 3D data array + dims (class `rxtracto3D`) |
| `rxtractogon()` | polygon (x,y vertices) through time | same shape as `rxtracto_3D()`, masked to the polygon |

### rxtracto(): Track + Search Radius

`xlen`/`ylen`/`zlen` are the **full width** of the box averaged around each point (half applied on each side), not a radius. A single value is recycled across all points; otherwise it must match the trajectory length.

```r
xpos <- Marlintag38606$lon
ypos <- Marlintag38606$lat
tpos <- Marlintag38606$date

dataInfo <- safe_info('erdSWchla8day')
track <- rxtracto(dataInfo, parameter = 'chlorophyll',
  xcoord = xpos, ycoord = ypos, tcoord = tpos,
  xlen = 0.2, ylen = 0.2, progress_bar = TRUE)
```

### rxtracto_3D(): Fixed Bounding Box

`xcoord`/`ycoord`/`tcoord` are each length 2 (`c(min, max)`); `zcoord` is optional. This is essentially `rerddap::griddap()` with coordinate/dateline handling built in — same call could be made directly in rerddap, but this function is what `rxtractogon()` builds on.

```r
dataInfo <- safe_info('erdMBsstd1day')
box <- rxtracto_3D(dataInfo, parameter = 'sst',
  xcoord = c(230, 235), ycoord = c(33, 36),
  tcoord = c('2006-01-15', '2006-01-20'))
box$data[, , 1]     # first time slice of the (lon, lat, time) array
```

### rxtractogon(): Polygon Through Time

Extracts the smallest bounding box containing the polygon via `rxtracto_3D()`, then masks everything outside it using `sp::point.in.polygon()`. Grid datasets only (checks for `"Grid"` in `dataInfo$alldata$NC_GLOBAL`); the polygon is auto-closed if the first/last vertex don't match.

```r
dataInfo <- safe_info('erdMBsstd1day')
sanctuary_sst <- rxtractogon(dataInfo, parameter = 'sst',
  xcoord = mbnms$Longitude, ycoord = mbnms$Latitude,
  tcoord = c('2006-01-15'))
```

## Longitude Convention & the Dateline

`xcoord` accepts either -180/180 or 0/360; the package remaps internally to match whatever convention the target dataset actually uses. But if a request **crosses the dateline** on a dataset stored on a -180/180 grid, the request must be given on a 0/360 grid — and several sanity checks are skipped in that case, so double-check the request makes sense for the dataset.

```r
# request crossing the dateline: use 0-360, not -180/180
xcoord <- c(170, 190)   # correct
# xcoord <- c(170, -170)  # wrong — will not do what you expect
```

## ERDDAP Interpolation Option: Faster Large Tracks

`rxtracto()`'s `interp` argument routes the request through ERDDAP's server-side `/convert/interpolate.csv` service in batches of 100 points, instead of one `griddap()` call per unique time in the track — much faster for tracks with many points. Requires ERDDAP >= 2.10, and requires a time coordinate.

```r
track <- rxtracto(dataInfo, parameter = 'chlorophyll',
  xcoord = xpos, ycoord = ypos, tcoord = tpos,
  interp = c('Bilinear', '4'))
```

- `interp[1]`: one of `'Nearest'`, `'Bilinear'`, `'Mean'`, `'SD'`, `'Median'`, `'Scaled'`, `'InverseDistance'`, `'InverseDistance2'`, `'InverseDistance4'`, `'InverseDistance6'`
- `interp[2]`: neighborhood size, one of `'1'`, `'4'`, `'8'`, `'16'`, `'36'`, `'64'`, `'216'` — `'Bilinear'` requires `'4'`
- The result has different columns than a non-interpolated `rxtracto()` call — it's ERDDAP's raw interpolated csv, not the mean/sd/median/mad summary you get without `interp`.

## Plotting

```r
p <- plotTrack(track, xpos, ypos, tpos, plotColor = 'thermal')      # rxtracto() output only
p <- plotBBox(box, plotColor = 'viridis', maxpixels = 500)           # rxtracto_3D()/rxtractogon() output only
```

Both wrap `plotdap`. `animate = TRUE` animates over time (`cumulative = TRUE` accumulates frames); `crs = "<proj string>"` reprojects and is validated with `sf::st_crs()`; `mapData` must be a `"map"`-class object from `maps`/`mapdata`.

**Wrong pattern:** passing `rxtracto()` output to `plotBBox()`, or `rxtracto_3D()`/`rxtractogon()` output to `plotTrack()` — both check the input's class and `stop()` immediately with a class-mismatch message.

## Reshaping: tidy_grid()

Converts the 3D array from `rxtracto_3D()`/`rxtractogon()` into a long-format data.frame — one row per (lon, lat, time) combination. Requires class `rxtracto3D` input (i.e. not `rxtracto()`'s track output).

```r
box_tidy <- tidy_grid(box)                 # tibble by default
box_tidy <- tidy_grid(box, as_tibble = FALSE)   # plain data.frame
```

## Bundled Example Datasets

Ships with pre-fetched data so the package's own examples/vignette run offline and within CRAN time limits:

- `dataInfo` — cached `rerddap::info('erdHadISST')` result
- `Marlintag38606` — a tagged marlin track (lon/lat/date)
- `MBsst`, `swchl` — example `rxtracto_3D()`/`rxtracto()` outputs
- `PB_Argos` — Argos tag data
- `mbnms` — Monterey Bay National Marine Sanctuary boundary (lon/lat polygon), used with `rxtractogon()`

## Common Mistakes

| Mistake | Fix |
|---|---|
| Calling `rerddap::info()` directly in unattended code | Use `safe_info()` — returns `NULL` instead of erroring on a down server |
| Treating `xlen`/`ylen` as a radius | They're the full box width; half is applied on each side of the point |
| Dateline-crossing request on a -180/180 dataset written as `c(170, -170)` | Use the 0-360 form for cross-dateline requests: `c(170, 190)` |
| Passing `rxtracto()` output to `plotBBox()` (or the reverse) | `rxtracto()` → `plotTrack()`; `rxtracto_3D()`/`rxtractogon()` → `plotBBox()` |
| Expecting rerddapXtracto to use your `rerddap::cache_setup()` path | Every `rxtracto*()` call runs `rerddap::cache_setup(temp_dir = TRUE)` internally — cache is always a temp dir here, not your configured one |
| Using `interp` without checking the ERDDAP server version | The interpolation service requires ERDDAP >= 2.10; older servers are rejected before the request is made |
| Calling `rxtractogon()` on a tabledap-only dataset | Only works on Grid datasets — checks `dataInfo$alldata$NC_GLOBAL` for `"Grid"` |

## Quick Reference

| Task | Function |
|---|---|
| Safe dataset metadata (no error on down server) | `safe_info()` |
| Extract along a track with search radius | `rxtracto()` |
| Extract a fixed 3D bounding box | `rxtracto_3D()` |
| Extract within a polygon through time | `rxtractogon()` |
| Plot `rxtracto()` track output | `plotTrack()` |
| Plot `rxtracto_3D()`/`rxtractogon()` grid output | `plotBBox()` |
| Reshape 3D/polygon output to long format | `tidy_grid()` |
| Faster large-track extraction | `rxtracto(..., interp = c(<type>, <neighbors>))` |
