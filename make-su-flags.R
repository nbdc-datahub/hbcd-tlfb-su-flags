# HBCD TLFB and ASSIST Data Cleaning Script
# Author: Catherine Psaras, PhD
# Description: This script prepares and processes Timeline Follow-Back (TLFB) and ASSIST V1/V2 data from the HBCD study.
# The cleaning steps include:
#  - Cleaning alcohol-related TLFB flags
#  - Merging ASSIST data to impute missing TLFB values
#  - Creating derived flags for substance use patterns across weeks 3–9
#  - Generating summary variables for use in downstream analysis
# Last updated: 2025-07-23

# Load required libraries
library(tidyverse)
library(data.table)

# File paths
tlfb_path <- "TLFB.csv"
assistv2_path <- "assistv2.csv"
assistv1_path <- "assistv1.csv"

# Load and clean ASSIST V1
assistv1 <- fread(assistv1_path) %>%
  select(
    particpant_id, session_id,
    pex_bm_assistv1_during__use_001,
    pex_bm_assistv1_during__use_002,
    pex_bm_assistv1_during__use_003,
    pex_bm_assistv1_during__use_006,
    pex_bm_assistv1_during__use_007,
    pex_bm_assistv1_during__use_008,
    pex_bm_assistv1_during__use_009,
    pex_bm_assistv1_lt__use_001,
    pex_bm_assistv1_lt__use_002,
    pex_bm_assistv1_lt__use_003,
    pex_bm_assistv1_lt__use_006,
    pex_bm_assistv1_lt__use_007,
    pex_bm_assistv1_lt__use_008,
    pex_bm_assistv1_lt__use_009
  ) %>%
  filter(session_id != "ses-V02")

# Load and clean ASSIST V2
assistv2 <- fread(assistv2_path) %>%
  filter(session_id == "ses-V02") %>%
  select(
    particpant_id,
    pex_bm_assistv2_end__use_001,
    pex_bm_assistv2_end__use_002,
    pex_bm_assistv2_end__use_003,
    pex_bm_assistv2_end__use_006,
    pex_bm_assistv2_end__use_007,
    pex_bm_assistv2_end__use_008,
    pex_bm_assistv2_end__use_009
  ) %>%
  filter(if_any(starts_with("pex_bm_assistv2_end__use_"), ~ !is.na(.)))

# Load TLFB (alcohol only)
alc <- fread(tlfb_path) %>%
  select(particpant_id, session_id, pex_ch_tlfb_self_report_alcohol) %>%
  mutate(visit_num = case_when(
    session_id == "ses-V01" ~ 1,
    session_id == "ses-V02" ~ 2
  )) %>%
  select(-session_id) %>%
  pivot_wider(names_from = visit_num, values_from = pex_ch_tlfb_self_report_alcohol,
              names_prefix = "alc_flag")

# Merge with ASSIST and apply rescue rules
alc <- alc %>%
  full_join(assistv1, by = "particpant_id") %>%
  full_join(assistv2, by = "particpant_id") %>%
  mutate(
    alc_flag1 = ifelse(is.na(alc_flag1) & 
                         (pex_bm_assistv1_lt__use_002 == 0 | pex_bm_assistv1_during__use_002 == 0), 0, alc_flag1),
    alc_flag2 = ifelse(is.na(alc_flag2) & 
                         (pex_bm_assistv1_lt__use_002 == 0 | pex_bm_assistv1_during__use_002 == 0) &
                         pex_bm_assistv2_end__use_002 == 0, 0, alc_flag2),
    alc_flag2 = ifelse(is.na(alc_flag2) & alc_flag1 == 0 & pex_bm_assistv2_end__use_002 == 0, 0, alc_flag2),
    alc_flag2 = ifelse(alc_flag1 == 1, 1, alc_flag2)
  ) %>%
  select(particpant_id, alc_flag1, alc_flag2)

# Load TLFB full and clean
tlfb <- fread(tlfb_path) %>%
  mutate(num_vis = ave(particpant_id, particpant_id, FUN = length)) %>%
  mutate(across(contains("_wk_08"), ~ ifelse(session_id == "ses-V01", NA, .))) %>%
  mutate(across(contains("_wk_09"), ~ ifelse(session_id == "ses-V01", NA, .)))

#Create new weekly frequency variables for nicotine, cannabis, and opioids
for (i in 3:9) {
  wk_str <- sprintf("0%d", i)
  
  tlfb <- tlfb %>%
    group_by(particpant_id) %>%
    mutate(
      !!paste0("nic_wk", i) := {
        x <- .data[[paste0("pex_ch_tlfb_nic_wk_", wk_str)]]
        if (all(is.na(x))) NA else max(x, na.rm = TRUE)
      },
      !!paste0("thc_wk", i) := {
        x <- .data[[paste0("pex_ch_tlfb_thc_wk_", wk_str)]]
        if (all(is.na(x))) NA else max(x, na.rm = TRUE)
      },
      !!paste0("opd_wk", i) := {
        x <- .data[[paste0("pex_ch_tlfb_opd_wk_", wk_str)]]
        if (all(is.na(x))) NA else max(x, na.rm = TRUE)
      }
    ) %>%
    ungroup()
}

#Limit dataset to only variables required for cleaning
tlfb <- tlfb %>%
  select(
    particpant_id, num_vis,
    nic_wk3, thc_wk3, opd_wk3,
    nic_wk4, thc_wk4, opd_wk4,
    nic_wk5, thc_wk5, opd_wk5,
    nic_wk6, thc_wk6, opd_wk6,
    nic_wk7, thc_wk7, opd_wk7,
    nic_wk8, thc_wk8, opd_wk8,
    nic_wk9, thc_wk9, opd_wk9
  )

#Drop duplicate rows
tlfb <- tlfb %>% distinct()

#Merge in ASSIST data (V01 & V02)
tlfb <- tlfb %>%
  full_join(assistv1, by = "particpant_id") %>%
  full_join(assistv2, by = "particpant_id")

#OPD LT USE
# Define the 4 opioid variables
opioid_lt_vars <- c(
  "pex_bm_assistv1_lt__use_006",
  "pex_bm_assistv1_lt__use_007",
  "pex_bm_assistv1_lt__use_008",
  "pex_bm_assistv1_lt__use_009"
)

# Step 1: Count how many of the 4 variables equal 1 per row, ignoring NAs
tlfb$opd_lt_tot <- rowSums(tlfb[opioid_lt_vars] == 1, na.rm = TRUE)

# Step 2: Set to NA only if all 4 values are missing
tlfb$opd_lt_tot[rowSums(!is.na(tlfb[opioid_lt_vars])) == 0] <- NA

#OPD DURING USE
# Define the 4 opioid variables
opioid_dur_vars <- c(
  "pex_bm_assistv1_during__use_006",
  "pex_bm_assistv1_during__use_007",
  "pex_bm_assistv1_during__use_008",
  "pex_bm_assistv1_during__use_009"
)

# Step 1: Count how many of the 4 variables equal 1 per row, ignoring NAs
tlfb$opd_dur_tot <- rowSums(tlfb[opioid_dur_vars] == 1, na.rm = TRUE)

# Step 2: Set to NA only if all 4 values are missing
tlfb$opd_dur_tot[rowSums(!is.na(tlfb[opioid_dur_vars])) == 0] <- NA


#OPD END USE
# Define the 4 opioid variables
opioid_end_vars <- c(
  "pex_bm_assistv2_end__use_006",
  "pex_bm_assistv2_end__use_007",
  "pex_bm_assistv2_end__use_008",
  "pex_bm_assistv2_end__use_009"
)

# Step 1: Count how many of the 4 variables equal 1 per row, ignoring NAs
tlfb$opd_end_tot <- rowSums(tlfb[opioid_end_vars] == 1, na.rm = TRUE)

# Step 2: Set to NA only if all 4 values are missing
tlfb$opd_end_tot[rowSums(!is.na(tlfb[opioid_end_vars])) == 0] <- NA

#Fill in missing opioid weekly variables with 0s if ASSIST provides appropriate data
for (i in 3:7) {
  wk_var <- paste0("opd_wk", i)
  
  # Condition 1: opd_wk is NA AND opd_lt_tot == 0 AND (opd_dur_tot == 0 OR is.na(opd_dur_tot))
  tlfb[[wk_var]][is.na(tlfb[[wk_var]]) & 
                   tlfb$opd_lt_tot == 0 & 
                   (tlfb$opd_dur_tot == 0 | is.na(tlfb$opd_dur_tot))] <- 0
  
  # Condition 2: opd_wk is NA AND opd_lt_tot is NA AND opd_dur_tot == 0
  tlfb[[wk_var]][is.na(tlfb[[wk_var]]) & 
                   is.na(tlfb$opd_lt_tot) & 
                   tlfb$opd_dur_tot == 0] <- 0
  
  # Condition 3: opd_wk is NA AND opd_lt_tot is 1 AND opd_dur_tot == 0
  tlfb[[wk_var]][is.na(tlfb[[wk_var]]) & 
                   tlfb$opd_lt_tot > 0 & 
                   tlfb$opd_dur_tot == 0] <- 0
  
  
}

for (i in 8:9) {
  wk_var <- paste0("opd_wk", i)
  
  # Condition 4: opd_wk is NA AND opd_end_tot == 0
  tlfb[[wk_var]][is.na(tlfb[[wk_var]]) & 
                   tlfb$opd_end_tot == 0] <- 0
  
  
}

#Fill in missing CANNABIS and NICOTINE weekly variables with 0s if ASSIST provides appropriate data
#CANNABIS
for (i in 3:7) {
  wk_var <- paste0("thc_wk", i)
  
  # Condition 1: thc_wk is NA AND thc_lt_tot == 0 AND (thc_dur_tot == 0 OR is.na(thc_dur_tot))
  tlfb[[wk_var]][is.na(tlfb[[wk_var]]) & 
                   tlfb$pex_bm_assistv1_lt__use_003 == 0 & 
                   (tlfb$pex_bm_assistv1_during__use_003 == 0 | is.na(tlfb$pex_bm_assistv1_during__use_003))] <- 0
  
  # Condition 2: thc_wk is NA AND thc_lt_tot is NA AND thc_dur_tot == 0
  tlfb[[wk_var]][is.na(tlfb[[wk_var]]) & 
                   is.na(tlfb$pex_bm_assistv1_lt__use_003) & 
                   tlfb$pex_bm_assistv1_during__use_003 == 0] <- 0
  
  # Condition 3: thc_wk is NA AND thc_lt_tot is 1 AND thc_dur_tot == 0
  tlfb[[wk_var]][is.na(tlfb[[wk_var]]) & 
                   tlfb$pex_bm_assistv1_lt__use_003 == 1 & 
                   tlfb$pex_bm_assistv1_during__use_003 == 0] <- 0
  
  
}

for (i in 8:9) {
  wk_var <- paste0("thc_wk", i)
  
  # Condition 4: thc_wk is NA AND opd_end_tot == 0
  tlfb[[wk_var]][is.na(tlfb[[wk_var]]) & 
                   tlfb$pex_bm_assistv2_end__use_003 == 0] <- 0
  
  
}

#NICOTINE
for (i in 3:7) {
  wk_var <- paste0("nic_wk", i)
  
  # Condition 1: nic_wk is NA AND nic_lt_tot == 0 AND (nic_dur_tot == 0 OR is.na(nic_dur_tot))
  tlfb[[wk_var]][is.na(tlfb[[wk_var]]) & 
                   tlfb$pex_bm_assistv1_lt__use_001 == 0 & 
                   (tlfb$pex_bm_assistv1_during__use_001 == 0 | is.na(tlfb$pex_bm_assistv1_during__use_001))] <- 0
  
  # Condition 2: nic_wk is NA AND nic_lt_tot is NA AND nic_dur_tot == 0
  tlfb[[wk_var]][is.na(tlfb[[wk_var]]) & 
                   is.na(tlfb$pex_bm_assistv1_lt__use_001) & 
                   tlfb$pex_bm_assistv1_during__use_001 == 0] <- 0
  
  # Condition 3: nic_wk is NA AND nic_lt_tot is 1 AND nic_dur_tot == 0
  tlfb[[wk_var]][is.na(tlfb[[wk_var]]) & 
                   tlfb$pex_bm_assistv1_lt__use_001 == 1 & 
                   tlfb$pex_bm_assistv1_during__use_001 == 0] <- 0
  
  
}

for (i in 8:9) {
  wk_var <- paste0("nic_wk", i)
  
  # Condition 4: nic_wk is NA AND opd_end_tot == 0
  tlfb[[wk_var]][is.na(tlfb[[wk_var]]) & 
                   tlfb$pex_bm_assistv2_end__use_001 == 0] <- 0
  
}

#Create weekly binary flags for all substances
for (i in 3:9) {
  # Variable names
  nic_var <- paste0("nic_wk", i)
  thc_var <- paste0("thc_wk", i)
  opd_var <- paste0("opd_wk", i)
  
  nic_flag <- paste0("nic_wk", i, "_flag")
  thc_flag <- paste0("thc_wk", i, "_flag")
  opd_flag <- paste0("opd_wk", i, "_flag")
  
  # nic flag
  tlfb[[nic_flag]] <- ifelse(is.na(tlfb[[nic_var]]), NA,
                             ifelse(tlfb[[nic_var]] >= 1, 1, 0))
  
  # thc flag
  tlfb[[thc_flag]] <- ifelse(is.na(tlfb[[thc_var]]), NA,
                             ifelse(tlfb[[thc_var]] >= 1, 1, 0))
  
  # opd flag
  tlfb[[opd_flag]] <- ifelse(is.na(tlfb[[opd_var]]), NA,
                             ifelse(tlfb[[opd_var]] >= 1, 1, 0))
}

#Create variables for V01 (3-7) and V02 (8-9) totals
# nic 3–7 and 3–9
tlfb$nic_3_7 <- tlfb$nic_wk3_flag + tlfb$nic_wk4_flag + tlfb$nic_wk5_flag + tlfb$nic_wk6_flag + tlfb$nic_wk7_flag
tlfb$nic_3_9 <- tlfb$nic_wk3_flag + tlfb$nic_wk4_flag + tlfb$nic_wk5_flag + tlfb$nic_wk6_flag + tlfb$nic_wk7_flag + tlfb$nic_wk8_flag + tlfb$nic_wk9_flag

# thc 3–7 and 3–9
tlfb$thc_3_7 <- tlfb$thc_wk3_flag + tlfb$thc_wk4_flag + tlfb$thc_wk5_flag + tlfb$thc_wk6_flag + tlfb$thc_wk7_flag
tlfb$thc_3_9 <- tlfb$thc_wk3_flag + tlfb$thc_wk4_flag + tlfb$thc_wk5_flag + tlfb$thc_wk6_flag + tlfb$thc_wk7_flag + tlfb$thc_wk8_flag + tlfb$thc_wk9_flag

# opd 3–7 and 3–9
tlfb$opd_3_7 <- tlfb$opd_wk3_flag + tlfb$opd_wk4_flag + tlfb$opd_wk5_flag + tlfb$opd_wk6_flag + tlfb$opd_wk7_flag
tlfb$opd_3_9 <- tlfb$opd_wk3_flag + tlfb$opd_wk4_flag + tlfb$opd_wk5_flag + tlfb$opd_wk6_flag + tlfb$opd_wk7_flag + tlfb$opd_wk8_flag + tlfb$opd_wk9_flag

#Create V01 flags
# nic_flag1
tlfb$nic_flag1 <- ifelse(is.na(tlfb$nic_3_7), NA,
                         ifelse(tlfb$nic_3_7 >= 4, 1, 0))

# thc_flag1
tlfb$thc_flag1 <- ifelse(is.na(tlfb$thc_3_7), NA,
                         ifelse(tlfb$thc_3_7 >= 4, 1, 0))

# opd_flag1
tlfb$opd_flag1 <- ifelse(is.na(tlfb$opd_3_7), NA,
                         ifelse(tlfb$opd_3_7 >= 2, 1, 0))

#Create V02 flags
# nic_flag2
tlfb$nic_flag2 <- NA  # start with missing

tlfb$nic_flag2[!is.na(tlfb$nic_3_9) & tlfb$nic_3_9 < 4] <- 0
tlfb$nic_flag2[!is.na(tlfb$nic_3_9) & tlfb$nic_3_9 >= 4] <- 1
tlfb$nic_flag2[is.na(tlfb$nic_3_9) & !is.na(tlfb$nic_3_7) & tlfb$nic_3_7 >= 4] <- 1

# thc_flag2
tlfb$thc_flag2 <- NA  # start with missing

tlfb$thc_flag2[!is.na(tlfb$thc_3_9) & tlfb$thc_3_9 < 4] <- 0
tlfb$thc_flag2[!is.na(tlfb$thc_3_9) & tlfb$thc_3_9 >= 4] <- 1
tlfb$thc_flag2[is.na(tlfb$thc_3_9) & !is.na(tlfb$thc_3_7) & tlfb$thc_3_7 >= 4] <- 1

#opd_flag2
tlfb$opd_flag2 <- NA  # start with missing

tlfb$opd_flag2[!is.na(tlfb$opd_3_9) & tlfb$opd_3_9 < 2] <- 0
tlfb$opd_flag2[!is.na(tlfb$opd_3_9) & tlfb$opd_3_9 >= 2] <- 1
tlfb$opd_flag2[is.na(tlfb$opd_3_9) & !is.na(tlfb$opd_3_7) & tlfb$opd_3_7 >= 2] <- 1

#Merge alcohol indicators
final <- tlfb %>%
  select(particpant_id, ends_with("flag1"), ends_with("flag2")) %>%
  full_join(alc, by = "particpant_id")

