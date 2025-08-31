# Indirect-Exposure-Index-IEI-
An index to measure the country's exposure to future climate migration

Project Overview
This repository contains R scripts for an academic research project on diaspora network and climate vulnerability, completed as part of a Master’s dissertation.

How to Use
1.	Download raw data
  •	World Bank Global Bilateral Migration Database
  https://databank.worldbank.org/source/global-bilateral-migration

  •	UNPD International Migration Stock (for robustness tests)
  https://www.un.org/development/desa/pd/content/international-migrant-stock

  •	ND-GAIN Country Index (Vulnerability Sub-index)
  https://gain.nd.edu

  ! Data files are not uploaded due to licensing. Please download them manually from the official       sources and place them into local folders before running the scripts.

2.	Create folder structure
  On your computer (e.g., Desktop), prepare the following folders:

  Data Collection/                             #Main Data File
  ├── World Bank Global Bilateral Migration/   # put World Bank Dataset
  │     └── P_Data_Extract_From_Global_Bilateral_Migration.xlsx
  ├── UN International Migrants Stock/         # put UNPN Dataset
  │     └── undesa_pd_2024_ims_stock_by_sex_destination_and_origin.xlsx
  ├── ND-GAIN/                                 # put ND-GAIN Vulnerability Sub-index Dataset
  │     └── vulnerability.csv
  Dissertation Result/                         # put calculation result
  ├── Figures and Tables/                      # put figures and tables in dissertation

Acknowledgements
I would like to thank my supervisor Dr Moritz Marbach for his guidance during this project.

The R scripts in this repository are provided as supplementary material for a Master’s dissertation.  
They are intended for academic and research purposes only.  
Commercial use is not permitted without prior permission.  

