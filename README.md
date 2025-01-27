# Helios-
R scripts for analyzing solar flare data

Solar Flare Data Analysis with R
This repository contains R scripts for analyzing solar flare data collected by the RHESSI spacecraft for the years 2004–2005 and 2015–2016. The analysis includes data preprocessing, visualization, and statistical evaluations. Key tasks involve identifying patterns, categorizing flare intensities, and creating visualizations to compare solar activity across years and subgroups.

Features

Data Preprocessing:

Import .csv data files for RHESSI solar flare observations.
Handle corrupted data by correcting specific energy labels.
Subdivide datasets into overlapping time-based subgroups for focused analysis.

Outlier Detection:
Remove outliers using the interquartile range (IQR) method.

Visualization Tools:

Create density plots to analyze data distributions.
Visualize flare intensity using categorized total counts and flux integration methods.
Overlay hotspot visualizations on background solar images using density-based techniques.
Generate time-series GIFs for dynamic hotspot analysis.

Statistical Analysis:

Compare average total counts across time-based subgroups.
Cluster data points are based on flare intensity and energy levels.

Methods:

Method 1: Intensity visualization categorized by total counts.
Method 2: Flux integration-based intensity visualization.
Hotspot visualizations for high-intensity events.

Dependencies

R packages:
dplyr
ggplot2
jpeg
animation

Usage

Clone this repository and ensure the required R packages are installed.
Replace file paths in the scripts with the appropriate paths to your data files and image overlays.
Run the provided scripts to preprocess data, visualize results, and perform statistical analyses.
