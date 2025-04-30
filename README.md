# Fetch Pet Insurance Datathon 🦴📈

A full-stack data science solution to price pet insurance policies using interpretable machine learning.
This project was developed for the 2024 UNSW Actuarial Datathon in partnership with **Fetch Pet Insurance**.

> **Objective**: Build a modular, cluster-based pricing engine that predicts claim frequency and severity, returning competitive and adequate premiums for dogs and cats across Australia.

---

## 📁 Repository Structure
```text
├── Data Preparation/           # Feature engineering and policy-claim linkage
├── EDA/                        # Exploratory analysis (PCA, clustering)
├── Models/                     # GLMs, EBMs, boosting, and neural networks
├── Pricing/                    # Premium generation and rating tables
├── UG15_final_report.pdf       # Final submission
└── 4305 Instructions.Rmd       # Reproducible research walkthrough
```

---

## 🧠 Modelling Approach

### 🔎 Risk Segmentation
Claim conditions were grouped into **3 risk clusters** using k-means:
- **Cluster 1**: Acute, high-severity claims (e.g. gastro, ingestion)
- **Cluster 2**: Moderate-risk care (e.g. routine visits, injuries)
- **Cluster 3**: Chronic low-cost conditions (e.g. allergies, dental)

This segmentation improves both predictive accuracy and explainability.

### 📈 Frequency & Severity Models
For each cluster:
- **Frequency** = Number of expected claims per policy
- **Severity** = Expected cost per claim

Each cluster has:
- `Frequency_Model_c`: Predicts number of claims in cluster *c*
- `Severity_Model_c`: Predicts cost per claim in cluster *c*

**Method**: Explainable Boosting Machine (EBM)
- Additive GAM-like model using shallow boosted trees
- Feature selection: Lasso + Random Forest importance
- Tuned using 5-fold cross-validation

**Severity Credibility Adjustment**:
If cluster data is sparse (e.g., Cluster 2):
```math
\[
\hat{S}_{\text{final}} = z \cdot \hat{S}_{\text{cluster}} + (1 - z) \cdot \hat{S}_{\text{global}}
\]
```
Where *z* is the credibility weight.

### 🧮 Pure Premium Calculation
Final premium is:
\[
\text{Pure Premium} = \sum_{c=1}^3 f_c \cdot s_c
\]
Where *f* = frequency and *s* = credibility-adjusted severity per cluster.

---

## 🔁 Reproducibility
Run the full pipeline by:

1. Cloning the repo:
```bash
git clone https://github.com/Manjot44/Fetch-Pet-Insurance-Datathon.git
```
2. Placing the data extracts in `data/raw/`
3. Installing required packages:
```r
install.packages(c("tidyverse", "data.table", "gbm", "xgboost", "randomForest", "reticulate", "caret"))
reticulate::py_install("interpret")
```
4. Rendering the RMarkdown file:
```r
rmarkdown::render("4305 Instructions.Rmd")
```

---

## 📊 Key Results
| Metric            | Value     |
|------------------|-----------|
| RMSE (final EBM) | 701.56    |
| Gini Index       | 0.566     |

Performance improved significantly over GLMs and GAMs while retaining interpretability.

---

## 📌 Highlights
- Cluster-specific modelling captures risk nuances
- EBM offers a balance of accuracy + explainability
- Credibility adjustment ensures robustness
- PCA + breed traits + socioeconomics used as features

---

## 🙏 Acknowledgements
- UNSW Actuarial Society & Fetch for the case brief
- Microsoft’s `interpret` team for EBM models
- ABS, DogTime, and RSPCA for external datasets
