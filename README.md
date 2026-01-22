##  How to Run the Code（just follow the step of this pipline ,my project made by diffierents 8 R script）
To reproduce the analysis, please install the required packages (`tidyverse`, `lubridate`, `randomForest`, `gridExtra`) and run the scripts in the following order:

1.  **Step1_raw_data_Audit.R**: Performs initial quality check on the raw DEFRA data.
2.  **Step2_Data Execution.R**: Fetches OpenMeteo weather data via API and merges it with pollution data.
3.  **Step3_step2dataAudit.R**: Audits the merged dataset for missing values.
4.  **Step4_Cleaning.R**: Applies QA/QC rules (removing negatives and extreme outliers).
5.  **Step5_Feature_Engineering.R**: Calculates wind sectors, seasonal factors, and urban increments.
6.  **Step6_PM2.5_NO2_EDA.R**: Generates exploratory visualizations (diurnal profiles and boxplots).
7.  **Step7_RQ1_OLS.R**: Runs the Linear Regression models for RQ1.
8.  **Step8_RQ2_RF.R**: Runs the Random Forest threshold analysis for RQ2.
