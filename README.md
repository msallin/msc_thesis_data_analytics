# Waste Study Data Analytics

This repository contains the R code used to analyze the data collected as part of the "Waste Study".
The study is part of my Master of Science thesis.

The "main.r" located in the root of the project is the composition root. Executing this file will install all necessary packages and perform the analysis. All results are written to the "results" directory. The results are either plots (PDF files) or calculation results (text).  The folder "survey" contains all code necessary to read and prepare the raw data (provided as "result.xlsx") for further processing. In the "analysis" folder the actual code to generate the artifacts in the "results" folder is located. The name of the R file contains the research question which is addressed with the code in the file.

The raw data must not be shared with the public and hence it is not part of this repository.
The raw data is located in an Excel workbook consisting of four sheets.

- Demographics: Contains the answers from the initial/demographics survey.
- Daily Survey: Contains the answers from the daily surveys.
- Weekly Survey: Contains the answers from the weekly surveys.
- Final Survey:  Contains the answers from the final/closing survey.
