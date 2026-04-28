# NanopixR

<!-- badges: start -->
![Version](https://img.shields.io/badge/version-0.1.0-blue)
![License](https://img.shields.io/badge/license-LGPL%20(%3E%3D3)-green)
<!-- badges: end -->

**NanopixR** is an R package for the automated analysis of nanoparticles and regions of interest in biotechnological microscopy images. It bridges the R package [`biopixR`](https://github.com/Brauckhoff/biopixR) and the Python-based deep learning segmentation tool [Cellpose](https://github.com/MouseLand/cellpose), allowing both to be used directly from R within a unified workflow.

---

## Features

- 🔬 **Two analysis backends** — classical image processing via `biopixR` and deep learning segmentation via `Cellpose`
- 🤖 **Automatic method recommendation** — extracts image features and recommends the best analysis method per image
- 📁 **Folder-level batch processing** — analyze entire folders of microscopy images in one call
- 📏 **Physical unit conversion** — converts pixel measurements to nanometers using scale information from DM3 files
- 📊 **Interactive result inspection** — visualize segmentation masks alongside original images and export result tables
- ⚙️ **Guided setup** — interactive setup assistant for configuring the Python/Cellpose environment

---

## Installation

### Prerequisites

- R ≥ 4.0
- Python ≥ 3.8 (for Cellpose functionality)

### Install from GitHub

```r
# install.packages("remotes")
remotes::install_github("RonjaTittel/NanopixR")
```

### Python / Cellpose Setup

After installation, run the interactive setup assistant to configure the Python environment:

```r
library(NanopixR)
setup()
```

This will guide you through installing Cellpose and linking it to R via `reticulate`.

### In Session setup

Please run at the beginning of **each** R Session:

```{r}
library(NanopixR)
setup()
```

to ensure that all dependencies are connected correctly. Do not use any '*reticulate*::' functions before. That will lead to a connection with an incorrect Python environment.

---

## Quick Start

### Option 1: Full automated pipeline

```r
library(NanopixR)

setup()

img_dir <- "path/to/your/images"

results <- analysis_pip(
  folder     = img_dir,
  write_csv  = TRUE,
  scale_info = TRUE,
  gpu        = TRUE,
  method_bp  = "edge"
)

head(results$pixel)
head(results$converted)
```

### Option 2: Run methods individually

```r
# BiopixR-based analysis
results_bp <- run_biopixR(
  folder  = img_dir,
  method  = "edge"
)

# Cellpose-based analysis
results_cp <- run_cellpose(
  folder = img_dir,
  gpu    = TRUE
)
```

### Inspect results

```r
show_results(
  results_list = results_cp$pixel,
  folder       = img_dir,
  image_name   = "your_image_name",
  download     = TRUE
)
```

---

## Functions Overview

| Function | Description |
|---|---|
| `analysis_pip()` | Full automated pipeline with method recommendation |
| `run_biopixR()` | Run biopixR-based object detection on a folder |
| `run_cellpose()` | Run Cellpose-based deep learning segmentation on a folder |
| `extract_image_features()` | Extract image features and recommend an analysis method |
| `get_scales()` | Extract pixel scale information from DM3 files |
| `show_results()` | Visualize segmentation results and export tables |
| `setup()` | Interactive Python/Cellpose environment setup |

---

## Dependencies

**R packages:** `EBImage`, `reticulate`, `biopixR`, `ijtiff`, `imager`, `DT`

**Python:** `cellpose`

---

## License

LGPL (≥ 3) — see [LICENSE](LICENSE) for details.

---

## Citation

If you use NanopixR in your research, please cite:

> R package version 0.1.0. https://github.com/RonjaTittel/NanopixR












