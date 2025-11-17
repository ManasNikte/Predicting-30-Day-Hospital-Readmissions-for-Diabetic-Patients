# Predicting 30-Day Hospital Readmissions for Diabetic Patients 🏥

> **A Data-Driven Strategy to Reduce Costs and Improve Patient Outcomes**

![Status](https://img.shields.io/badge/Status-Complete-success)
![Language](https://img.shields.io/badge/Language-R%20%7C%20Markdown-blue)
![Domain](https://img.shields.io/badge/Domain-Healthcare%20Analytics-red)

## 📋 Executive Summary

Hospital readmissions are a critical challenge in healthcare, costing the US economy billions annually. For diabetic patients, the risk is particularly acute. 

This project analyzes 10 years of clinical care data (1999-2008) from 130 US hospitals, covering **101,766 patient encounters**. The goal was to move from a reactive care model to a proactive one by identifying high-risk patients *before* discharge.

**The Business Impact:**
* **Problem:** 11.2% of diabetic patients are readmitted within 30 days.
* **Cost:** Each readmission costs an estimated **$10,000 - $15,000**.
* **Solution:** A predictive "Risk Score" model that identifies high-risk patients, enabling targeted intervention and potential savings of millions.

---

## 💡 Key Insights & Discoveries

Our analysis uncovered several counter-intuitive findings that challenge standard hospital protocols:

### 1. The "Short Stay Paradox" 📉
Conventional wisdom suggests short stays indicate "safe, recovered" patients. Our data proves otherwise.
* **Finding:** Patients discharged after 1 day have the lowest readmission risk (8.2%). However, risk **peaks at 13.4%** for patients staying 3 days.
* **Implication:** Patients in the 2-7 day window may be getting discharged prematurely without adequate support.

### 2. The "20-Medication" Tipping Point 💊
* **Finding:** There is a clear threshold for complexity. Patients prescribed **>20 medications** show a volatile spike in readmission rates (jumping to over 16%).
* **Implication:** A simple count of medications can serve as an immediate "Red Flag" for pharmacy review.

### 3. Prior History is the #1 Predictor 🔄
* **Finding:** Clinical factors (like glucose levels) were less predictive than utilization history. The number of inpatient visits and ER trips in the preceding year were the strongest drivers of risk.
* **Implication:** Discharge planning must focus on patient history, not just the current diagnosis.

### 4. The Health Equity Gap ⚖️
* **Finding:** Patients who did *not* receive an HbA1c test had higher readmission rates. Critically, African American and Hispanic patients were tested at lower rates than other groups.
* **Implication:** Standardizing testing protocols is a low-cost way to improve equity and outcomes.

---

## 🛠 Methodology

### Data Source
The "Diabetes 130-US Hospitals for Years 1999-2008" dataset (UCI Machine Learning Repository).
* **Rows:** 101,766
* **Features:** 47 (Demographics, Medications, Lab Tests, Diagnoses)

### Modeling Approach
We employed a multi-model approach to ensure robustness:
1.  **Logistic Regression:** For baseline interpretability.
2.  **Random Forest:** To capture non-linear relationships between medications and risk.
3.  **XGBoost:** For high-performance predictive accuracy.

### The Solution: "Readmission Risk Score"
To make the machine learning results actionable for clinical staff, we distilled the complex model into a transparent scoring system:
* **Score 0-1 (Low Risk):** Standard Discharge.
* **Score 4+ (High Risk):** Requires Intervention (Nurse follow-up, Home Health).
* *Validation:* A score of 4+ correlates to a **21% probability** of readmission (vs 11% average).

