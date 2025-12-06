# Wildlife exclusion and management assessment: a coyote case study

### [Steven Gurney](https://linktr.ee/stevenmgurney), [Austin Messing](), [Selena Creed]()

### Manuscript: Airfield Operations – Wildlife Division, Special Publication No. 25-01. Wayne County Airport Authority, Detroit, MI, USA.

### Censored manuscript available here: [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.17842930.svg)](https://doi.org/10.5281/zenodo.17842930)

### Data: Most data are security sensitive and housed with the Wildlife Division's special-publication repository.

#### Please contact the first author for questions about the code or data: Steven M. Gurney (stevenmgurney@gmail.com)
__________________________________________________________________________________________________________________________________________

## Summary

At Detroit Metropolitan Wayne County Airport (DTW), we are committed to maintaining the highest standards of aviation safety, security, and operational efficiency. In alignment with 14 CFR § 139.337 and DTW’s Wildlife Hazard Management Plan, we recognize that proactive identification and mitigation of wildlife hazards are essential to reducing strike risks and protecting both airfield operations and the traveling public. Coyotes (_Canis latrans_) have emerged as a persistent and adaptable hazard at DTW, capable of breaching perimeter defenses and posing threats to safety, security, and airfield operations. Recognizing the importance of this issue, we (the Wayne County Airport Authority’s Airfield Operations – Wildlife Division) conducted a systematic evaluation of coyote activity, management, and exclusion effectiveness. We applied a comprehensive suite of methods, including relative hazard ranking, strike metric evaluation, integrated data analysis, dig and postmortem surveys, site inspections, camera trapping, and spatial analysis. Our findings identified coyotes as a highly hazardous species at DTW, revealed key perimeter vulnerabilities, and highlighted opportunities to better allocate resources for more effective exclusion. Our results also challenged existing Federal Aviation Administration (FAA) gap-size standards for coyote exclusion and enabled us to develop empirically informed recommendations to improve exclusion practices. Through this work, we established essential baseline metrics and supported the development of centralized databases to inform ongoing and future assessments at DTW. Collectively, these efforts advance our commitment to science-driven, adaptive wildlife hazard management to maintain safe and efficient airfield operations.

## Repository Directory

### [Integrated_Data_Analysis](./Integrated_Data_Analysis): Contains code for integrating, analyzing, and visulizing data.
* [Data_Integration.R](./Integrated_Data_Analysis/Data_Integration.R) - Code to process and integrate data from multiple sources.
* [Plots.R](./Integrated_Data_Analysis/Plots.R) - Code to visualize integrated-data results and produce control chart.

### [Spatial_Analysis](./Spatial_Analysis): Contains code for mapping, summarizing, and ranking sites problematic to exclusion.
* [Spatial_Data.R](./Spatial_Analysis/Spatial_Data.R) - Code to process and visualize landcover data; and calculate percent natural cover per buffered area.
* [Ranking_Summary.R](./Spatial_Analysis/Ranking_Summary.R) - Code to rank priority level of sites problematic to exclusion (based on landcover and perimeter-survey data) and produce summary statistics.

### [Species_Hazard_Ranking](./Species_Hazard_Ranking): Contains code to process strike data and rank relative hazard level of species.
* [Species_Ranking.R](./Species_Hazard_Ranking/Species_Ranking.R) - Code to process data exported from the FAA strike database.
* [Strike_Data_Prep.R](./Species_Hazard_Ranking/Strike_Data_Prep.R) - Code to analyze strike metrics and produce relative hazard results by species.

## Supplemental Information
* Relative [video presentation](https://youtu.be/CAUvHTNuZgo) given to AAAE Wildlife Management and BSC USA Operations and Policy Committee working group.
