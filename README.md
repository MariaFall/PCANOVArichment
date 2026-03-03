# PCANOVArichment (ezPCA)

An R Shiny app designed for bioinformatics analysis of LC/MS and other high-throughput datasets. It provides an interface for performing Principal Component Analysis (PCA), Analysis of Variance (ANOVA) and Enrichment Analysis (WIP).

---

## Features

* **Interactive PCA Visualization:** Generate and customize PCA plots to explore data variance and grouping.
  * **Score Plots:** Visualize sample groupings and variance.
  * **Scree Plots:** Assess the proportion of variance explained by each principal component.
  * **Loadings Plots:** Identify which variables contribute most to the principal components.
* **ANOVA Capabilities:** Perform statistical variance analysis across different sample groups.
* **Enrichment Analysis:** Conduct biological pathway and enrichment analysis on processed data.
* **Meta-Analysis:** Integrate and analyze results across multiple datasets.
* **Flexible Input:** Built primarily for processing LC/MS data but adaptable to various bioinformatical datasets.

## Repo Structure

* `statsLaunch.R`: The main script to launch the R Shiny application.
* `analysis_utils.R`: Utility functions for data processing and statistical analysis.
* `Plotting_utils.R`: Functions dedicated to generating the visualizations.
* `example1.csv` & `example2.csv`: Sample datasets provided to test the application's capabilities.

## Requirements

To run this app locally, you will need R installed on your machine. 

## Usage

1. Launch the app using the ``statsLaunch.R`` script.

2. Upload dataset or use the provided ``example1.csv`` or ``example2.csv`` files to explore the tool.

3. Navigate through the app interface to access the PCA, ANOVA, or Enrichment modules (WIP).

4. Export the generated plots and statistical outputs.

## Example Plot Output:

## Score Plot:


<img width="1505" height="993" alt="f7eedb4e41c427916d634ee16b00a594" src="https://github.com/user-attachments/assets/2f3e82bc-aca7-47d5-a425-43584fdf80fb" />


## Scree Plot:


<img width="1506" height="994" alt="cf2d0da2e8acc9fa561681e27ee07333" src="https://github.com/user-attachments/assets/ef45c17b-0f81-4de2-844a-df5f0c9cd7ea" />


## Loadings Plot:


<img width="1502" height="945" alt="50e4a02ed384aee08b52deee09d81cba" src="https://github.com/user-attachments/assets/2d9398f5-4e97-44a0-a31c-8de8e8a3010e" />
