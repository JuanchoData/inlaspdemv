# inlaspdemv
Code INLA-SPDE to explain changes of mesic vegetation 
Here’s a clear and concise explanation for your README file on GitHub to help users understand what your R script does and how they can use it. You can copy-paste or adapt it as needed:

---

## 📜 Project Overview

This project uses R and the [INLA](https://www.r-inla.org/) package to prepare and model spatial data of mesic vegetation conditions across four different years (2004, 2009, 2014, 2019). It includes raster data extraction, covariate standardization, and mesh generation for spatial modeling using the SPDE approach. This analysis supports understanding the spatial patterns of mesic vegetation in relation to precipitation, temperature, and land tenure.

---
## 📦 Required Data

You can download the images in those following links. 

Precipitation and temperature: https://code.earthengine.google.com/6649b7d5bd1fb2088b7ede64ba9d3b16
Mesic vegetation proportion: https://code.earthengine.google.com/a36833e5d2d99b1002108096e8de0193
Land Tenure: https://code.earthengine.google.com/3add7ddef9fb84b11706ec5871e6c379



## 📦 Required Packages

Make sure to install and load the following packages:

```r
install.packages("INLA", repos=c(getOption("repos"),
                      INLA="https://inla.r-inla-download.org/R/stable"),
                      dependencies=TRUE)

install.packages("rgdal") # required for raster and spatial data handling
# Also use install.packages() for: raster, sp, terra, dplyr, sf
```

---

## 📂 Data Preparation

1. **Set working directory**  
   Set the file path to your local directory where all raster datasets are stored.

2. **Load raster datasets**  
   Load mesic vegetation rasters for four years and a land tenure raster.

3. **Sample pixels randomly**  
   Randomly select 500 spatial points from the 2004 mesic raster to extract values from all rasters at the same locations for consistency.

4. **Extract covariates**  
   - Extract standardized precipitation and temperature values for each year.
   - Extract land tenure values for the sampled locations.

5. **Normalize climate variables**  
   Precipitation and temperature values are z-score normalized:  
   \[
   x_{\text{norm}} = \frac{x - \mu}{2\sigma}
   \]

6. **Clean and merge**  
   All year-specific data frames are merged into a single dataset for modeling.

---

## 🧠 Spatial Mesh and SPDE Setup

1. **Mesh creation**  
   The mesh is created using `inla.mesh.2d()` based on the convex hull of the sampled coordinates.  
   Parameters:
   - `max.edge`: controls triangle sizes near boundaries vs. interiors
   - `offset`: buffer zone for outer mesh
   - `cutoff`: removes duplicate/close points to reduce mesh complexity

2. **SPDE model**  
   - A projection matrix (`A`) is created to map data points to the mesh.
   - The Matérn covariance model is defined via `inla.spde2.matern()` to capture spatial correlation.

---

## 🧾 Output and Usage

This script sets up the data and spatial model input needed to run spatial regression or INLA-based spatial-temporal models. Once the mesh and projection matrix are created, you can move on to define the full INLA model using `inla()`.

---

## 📁 Files Needed

Place the following in your working directory:
- `WRP-040029-YYYY08.tif` (mesic raster for each year)
- `TenureRasterExport.tif` (land tenure)
- `029ppt_YYYY_08.tif` (precip and temp stacked raster for each year under `ppt_exports/`)

---

## 📌 Notes

- All rasters must be projected into the same CRS (coordinate reference system).
- The script includes comments to help you adjust mesh resolution and spatial extent.
- Make sure you have a working internet connection when installing INLA or older versions of `rgdal`.

---
