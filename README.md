# Internet use trajectories and long-term health outcomes among middle-aged and older adults in the United States, 2002 to 2019
# Project Descriptions
This project analyzes longitudinal data from the Health and Retirement Study (2002-2019) to investigate how sustained internet usage patterns impact health outcomes in adults 50+. Using trajectory modeling and Cox regression, we identify five distinct internet use patterns and demonstrate that sustained/adopted usage associates with 37-44% lower mortality risk, reduced dementia incidence, and better mental health outcomes compared to non-users. Findings highlight digital engagement as a potential modifiable factor for healthy aging while underscoring persistent digital disparities. Analysis code, methodology, and supplementary materials included.

# Scripts Descriptions
- The file "HRS 01 Data Extraction.R" extract raw data from HRS survey databases for subsequent analysis.
- The file "HRS 02 Data Cleaning.R" conduct data cleaning on the extracted datasets to generate clean, analysis-ready datasets.
- The file "HRS 03 All-cause mortality.R", "HRS 03 Cardiovascular.R", "HRS 03 Disability.R", "HRS 03 Dementia.R", "HRS 03 Depression.R" outline the methodology employed to process and analyze data related to different health outcomes in this project.
- The file "HRS 04 Sensitivity Analysis.R" conduct sensitivity analysis.
- The file "HRS 05 Forest Plot.R" is designed to generate forest plots for model results.
