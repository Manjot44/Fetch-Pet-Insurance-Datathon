# Fetch Pet Insurance Datathon 🦴📈  
End-to-end frequency–severity modelling & pricing framework for **Fetch Pet Insurance**.

> **Goal** — build an interpretable, data-driven rating engine that can forecast future claims costs and return competitive yet adequate premiums for dogs insured by Fetch.

---

## 📂 Repository layout
```
├── Data Preparation/      # cleaning, joins, feature coding, train-test split
│   └── Adding_data.R
├── EDA/                   # exploratory notebooks & graphics
│   ├── Breed Characteristics Clustering.R
│   ├── PCA (final).R
│   └── SA Graphs.R
├── Models/                # candidate models for frequency & severity
│   ├── full glm.R
│   ├── quasi_poisson_glm.R
│   ├── Model EBM with zeroing.R
│   ├── RF.R, XGBoost.R, … (tree & ML algorithms)
│   └── Neural Network.R
├── Pricing/               # rating factor build-out & premium generation
│   ├── Pricing Data Preparation.R
│   └── Pricing Output.R
├── UG15_final_report.pdf  # competition write-up & results
└── 4305 Instructions.Rmd  # reproducible research narrative
```

---

## 🔬 Methodology

### 1 · Data preparation  
* Merges policy, animal-level, breed-trait and socio-economic datasets.  
* Converts skewed continuous traits (e.g. *Energy Level*, *Intensity*) to 5-level ordered factors.  
* Stratified 80/20 train-test split, keeping exposure–claim integrity.  

### 2 · Exploratory analysis & feature engineering  
* **PCA** reduces dimensionality for 30+ census variables.  
* **Hierarchical clustering** groups 400+ breed characteristics into five clinically meaningful segments.  
* Seasonality, policy-age bands and UW-month derived for trend capture.  

### 3 · Modelling strategy  

| Component         | Candidates explored                                                | Final choice                          | Rationale                                                   |
|------------------|---------------------------------------------------------------------|----------------------------------------|-------------------------------------------------------------|
| **Claim frequency** | Poisson / quasi-Poisson GLM, Ridge/Lasso, GAM, RF, Boosting, XGBoost | **Gradient Boosting** (`gbm`) by risk segment | Best deviance, handles non-linearity & interactions         |
| **Claim severity**  | GLM, GAM, RF, NN, **Explainable Boosting Machine (EBM)**          | **EBM + Bühlmann-Straub credibility** | Accuracy **and** interpretability – shape functions + credibility |
| **Pure premium**    | Frequency × Credibility-adjusted Severity                         | —                                      | Actuarially sound, aligns with company loss-ratio targets   |

Key scripts: `Model EBM with zeroing.R`, `Boosting.R`.

### 4 · Pricing engine  
`Pricing Output.R` produces a tidy rating table with base premium, uplift/discount factors and scenario-testing hooks (age, breed, postcode).  

### 5 · Evaluation  
* **RMSE** on held-out policy-level paid loss.  
* **Normalised Gini** to assess ranking power.  
* Lift charts & calibration plots – see `UG15_final_report.pdf`.  

---

## ▶️ Reproducibility

1. **Clone** the repo  
   ```bash
   git clone https://github.com/Manjot44/Fetch-Pet-Insurance-Datathon.git
   cd Fetch-Pet-Insurance-Datathon
   ```

2. **Data**  
   Place the raw CSV extracts supplied by Fetch in `data/raw/` using the same filenames referenced in the scripts (not included here for confidentiality).

3. **R environment**  
   *R 4.3+* with:
   ```r
   install.packages(c(
     "tidyverse","data.table","gbm","randomForest","xgboost",
     "glmnet","mgcv","reticulate","interpret","caret"
   ))
   ```
   The EBM code calls the Python **interpret** package via `reticulate`.

4. **Run pipeline**  
   ```r
   source("Data Preparation/Adding_data.R")     # cleans & splits
   source("EDA/PCA (final).R")                  # optional – visuals
   source("Models/Model EBM with zeroing.R")    # trains severity
   source("Models/Boosting.R")                  # trains frequency
   source("Pricing/Pricing Output.R")           # writes premiums
   ```

   Outputs are saved to `output/` and summarised in the PDF report.

---

## 📊 Results snapshot

| Metric (test set)      | Value       |
|------------------------|-------------|
| RMSE (pure premium)    | **$24.7**    |
| Normalised Gini        | **0.43**     |
| Lift @ top decile      | **1.67×** baseline loss ratio |

(See detailed tables & plots in *UG15_final_report.pdf*.)

---

## 🙏 Acknowledgements

* Fetch Pet Insurance & UNSW Actuarial Society for the 2024 Datathon brief.  
* `interpret` team for the EBM implementation.  
* **ABS** & **RSPCA** for publicly available socioeconomic and breed-trait data.

---


## 📘 Reproducibility Guide

For a detailed walkthrough of the data processing, modelling, and pricing procedures, refer to the [`4305 Instructions.Rmd`](https://github.com/Manjot44/Fetch-Pet-Insurance-Datathon/blob/main/4305%20Instructions.Rmd) file.  
This RMarkdown document provides step-by-step instructions, combining code and commentary to facilitate understanding and replication of the analysis.

### Running the RMarkdown File

1. **Install Required Packages**  
   Ensure that all necessary R packages are installed:

   ```r
   install.packages(c(
     "tidyverse", "data.table", "gbm", "randomForest", "xgboost",
     "glmnet", "mgcv", "reticulate", "interpret", "caret"
   ))
   ```

2. **Set Up Python Dependencies**  
   The project utilises the `interpret` package via Python.  
   Ensure that Python is installed and the `interpret` package is available:

   ```r
   reticulate::py_install("interpret")
   ```

3. **Render the RMarkdown File**  
   Use RStudio or run the following command in R:

   ```r
   rmarkdown::render("4305 Instructions.Rmd")
   ```

This will generate an output document (e.g., HTML or PDF) that encapsulates the entire analysis, including data preparation, EDA, modelling, and pricing strategies.

---

By following the instructions in the `4305 Instructions.Rmd` file, users can replicate the project’s results and gain deeper insights into the methodologies employed.
