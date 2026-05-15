MAG diversity analysis workflow
=================================

Overview
--------
This repository contains an R workflow for alpha- and beta-diversity analysis of metagenome-assembled genome (MAG) abundance profiles. The script calculates alpha-diversity metrics, performs group-level and pairwise statistical tests, generates NMDS plots based on Bray-Curtis dissimilarity, and exports publication-ready PDF figures and Excel/TSV summary tables.

Main script
-----------
- 01_mags_diversity.R: Runs the complete MAG diversity workflow.

Required input files
--------------------
Place the following files in the same working directory as the R script before running the analysis:

1. mags_95_75_10.tsv
   - MAG abundance table.
   - The first column should contain MAG or feature IDs.
   - The remaining columns should be sample IDs.
   - Values should be numeric abundance values, such as TPM, relative abundance, or read-count-derived abundance.

2. Group.xlsx
   - Sample metadata file.
   - Must contain at least two columns:
     - ID: sample ID matching the sample names in mags_95_75_10.tsv.
     - Group: group label used for plotting and statistical comparisons.

R package dependencies
----------------------
The script requires the following R packages:

- vegan
- ggplot2
- patchwork
- readxl
- rstatix
- dplyr
- stringr
- openxlsx

You can install missing packages using:

install.packages(c("vegan", "ggplot2", "patchwork", "readxl", "rstatix", "dplyr", "stringr", "openxlsx"))

Usage
-----
Run the script in R or RStudio from the directory containing the input files:

source("01_mags_diversity.R")

Alternatively, from the command line:

Rscript 01_mags_diversity.R

User-configurable parameters
----------------------------
The main settings are defined near the top of the script:

- MAGS_FILE: MAG abundance table. Default: mags_95_75_10.tsv
- GROUP_XLSX: metadata file. Default: Group.xlsx
- OUT_ROOT: output directory. Default: results_mags_diversity
- GROUP_COL: group column in the metadata file. Default: Group
- ID_COL: sample ID column in the metadata file. Default: ID
- RUN_ALL: whether to run all-group analysis. Default: TRUE
- RUN_PAIRWISE: whether to run all pairwise group comparisons. Default: TRUE
- PERMUTATIONS: number of permutations for PERMANOVA and ANOSIM. Default: 999

Output structure
----------------
The script creates the following output directory:

results_mags_diversity/

Main outputs:

1. results_mags_diversity/ALL/
   - Alpha_diversity_metrics.tsv
   - Fig_Alpha_diversity_4metrics.pdf
   - Fig_Beta_NMDS_BrayCurtis_GroupColor.pdf
   - Stats_ALPHA.txt
   - Stats_BETA.txt
   - Stats_ALL.xlsx

2. results_mags_diversity/PAIRWISE/
   - A separate folder is created for each pairwise comparison.
   - Each folder contains alpha-diversity metrics, alpha-diversity plots, beta-diversity NMDS plots, TXT statistics, and XLSX statistics.

Statistical analyses
--------------------
Alpha diversity:
- Richness: calculated with vegan::specnumber.
- Shannon index: calculated with vegan::diversity(index = "shannon").
- Simpson index: calculated with vegan::diversity(index = "simpson").
- Pielou evenness: calculated as Shannon / log(Richness).
- Global group differences are tested with the Kruskal-Wallis test.
- Pairwise group comparisons are tested with Wilcoxon rank-sum tests and Benjamini-Hochberg correction.

Beta diversity:
- Bray-Curtis dissimilarity is calculated with vegan::vegdist.
- NMDS ordination is calculated with vegan::metaMDS.
- Global and pairwise group differences are tested with PERMANOVA using vegan::adonis2.
- ANOSIM is also reported using vegan::anosim.
- NMDS ellipses are drawn only for groups with at least three samples.

Group ordering and colors
-------------------------
The script uses workflow-aware group ordering and colors. Group labels are parsed to infer production stages and processes:

- F: Farm
- S: Slaughter
- M: Market
- C: Control

For slaughter and market groups, process order is prioritized. For farm and control groups, sampling day is prioritized. Colors are generated from stage-specific base colors with day- and process-specific tints.

Notes for reproducibility
-------------------------
- The random seed is fixed with set.seed(123).
- Default permutation number is 999.
- Make sure the sample IDs in Group.xlsx exactly match the sample names in the MAG abundance table.
- Output PDFs are suitable for downstream editing in Adobe Illustrator or similar vector-graphics software.

Suggested citation in methods
-----------------------------
MAG alpha-diversity metrics were calculated using the vegan R package. Bray-Curtis dissimilarities were used for NMDS ordination and beta-diversity testing. Group-level differences were assessed using Kruskal-Wallis tests for alpha diversity and PERMANOVA/ANOSIM for beta diversity, with pairwise Wilcoxon tests and Benjamini-Hochberg adjustment for multiple comparisons where appropriate.
