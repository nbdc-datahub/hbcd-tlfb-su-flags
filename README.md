# HBCD TLFB and ASSIST Data Cleaning

Overview
--------

This script cleans and processes Timeline Follow-Back (TLFB) and ASSIST V1/V2 data from the Healthy Brain and Child Development (HBCD) study to prepare for downstream analysis of substance use patterns.

The script focuses on harmonizing alcohol, nicotine, cannabis, and opioid indicators using a combination of TLFB data and self-report information from the ASSIST questionnaire.

Author
------

Catherine Psaras, PhD
Last updated: June 27, 2025

Key Features
------------

- Cleans and flags alcohol use using both TLFB and ASSIST responses.
- Derives weekly substance use variables (nicotine, cannabis, opioids) for weeks 3–9.
- Uses ASSIST data as a rescue mechanism to fill in missing TLFB values.
- Creates summary flags (flag1 for weeks 3–7, flag2 for weeks 3–9) for each substance.
- Outputs merged dataset with flags for alcohol, nicotine, cannabis, and opioid use.

Data Sources
------------

File: Timeline Follow-Back (CSV format)
Description: TLFB data (visits: ses-V01 & ses-V02)

File: ASSIST V1 (CSV format)
Description: ASSIST V1 data (visit: ses-V01)

File: ASSISTV2 (CSV format)
Description: ASSIST V2 data (visit: ses-V02)

Note: File paths are currently hardcoded and should be updated based on your environment (lines 16-18).

Dependencies
------------

This script requires the following R packages:

install.packages(c("tidyverse", "data.table"))

library(tidyverse)
library(data.table)

Processing Steps
----------------

1. Load and clean ASSIST V1 and V2
   - Keep only relevant substance use indicators.
   - Filter V1 to exclude visit 2.
   - Filter V2 to include only visit 2 and non-missing responses.

2. Process alcohol TLFB data
   - Extract alcohol flags from TLFB.
   - Merge with ASSIST data.
   - Use self-report responses to impute missing alcohol flags.

3. Derive weekly use variables
   - For nicotine, cannabis, and opioids:
     - Calculate max reported use per week (weeks 3–9).
     - Drop V01 values from weeks 8 and 9 as they are erroneously entered as 0s.

4. Impute missing weekly values using ASSIST data
   - Apply logic based on long-term, during, and end-use ASSIST variables.
   - If ASSIST suggests no use, set missing TLFB values to 0.

5. Create binary weekly flags
   - For each substance and week, generate a _flag variable:
     - 1 = any use that week.
     - 0 = no use.
     - NA = data missing.

6. Generate cumulative indicators
   - Calculate weekly flag totals for:
     - Visit 1 (weeks 3–7)
     - Visit 2 (weeks 3–9)

7. Create final use flags based on consortium benchmarks for use
   - *_flag1 → Visit 1 (e.g., nicotine use on ≥4 weeks or cannabis use on ≥4 weeks, opioid use on ≥2 weeks).
   - *_flag2 → Visit 2 (e.g., nicotine use on ≥4 weeks or cannabis use on ≥4 weeks, opioid use on ≥2 weeks).
   - Alcohol flags are harmonized from both TLFB and ASSIST.

Output
------

The final dataset (final) includes:

- Participant ID: id
- Self-Report Flag V01: *_flag1 (nicotine, cannabis, opioid, alcohol)
- Self-Report Flag V02: *_flag2 (nicotine, cannabis, opioid, alcohol)

These can be used in downstream regression models or descriptive summaries.

Notes
-----

- Opioid indicators are composite measures based on multiple drugs (e.g., oxycodone, heroin).
- Once a particpant has a positive self-report flag at V01, their V02 record is marked as positive regardless 
	of V02 visit completion. The resulting file should be merged with the visit information table to limit data 
	to completed visits.

Contact
-------

For questions or feedback, please contact:

Catherine Psaras  
Email: cpsaras@health.ucsd.edu
