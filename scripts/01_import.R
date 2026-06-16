# =====================================================
# NHANES 2005-2006
# 01_import.R
# =====================================================

library(tidyverse)
library(haven)
library(janitor)
library(dplyr)
library(data.table)
# library(rnhanesdata)

#------------------------------------------------------
# directories
#------------------------------------------------------

root <- "C:/Users/Benyu/Documents/nhanes_project"

raw_dir   <- file.path(root,"data_raw")
clean_dir <- file.path(root,"data_clean")

#------------------------------------------------------
# demographics
#------------------------------------------------------

demo <- read_xpt(
  file.path(raw_dir,"DEMO_D.XPT")
)

#------------------------------------------------------
# examination
#------------------------------------------------------

bmx <- read_xpt(
  file.path(raw_dir,"BMX_D.XPT")
)

bpx <- read_xpt(
  file.path(raw_dir,"BPX_D.XPT")
)

#------------------------------------------------------
# laboratory
#------------------------------------------------------

cbc <- read_xpt(
  file.path(raw_dir,"CBC_D.XPT")
)

crp <- read_xpt(
  file.path(raw_dir,"CRP_D.XPT")
)

biopro <- read_xpt(
  file.path(raw_dir,"BIOPRO_D.XPT")
)

glu <- read_xpt(
  file.path(raw_dir,"GLU_D.XPT")
)

ghb <- read_xpt(
  file.path(raw_dir,"GHB_D.XPT")
)

hdl <- read_xpt(
  file.path(raw_dir,"HDL_D.XPT")
)

tchol <- read_xpt(
  file.path(raw_dir,"TCHOL_D.XPT")
)

cot <- read_xpt(
  file.path(raw_dir,"COT_D.XPT")
)

#------------------------------------------------------
# activity monitor
#------------------------------------------------------

pax <- readRDS("data_raw/nhanes-2026.rda")

# include valid wear time
pax_clean <- pax %>%
  filter(INCLUDE == 1)

# step volume
pax_clean <- pax_clean %>%
  mutate(
    steps = STEPS
  )

# MVPA volume
pax_clean <- pax_clean %>%
  mutate(
    mvpa = MVPA_MIN,
    active = ACTIVE_MIN
  )

# sedentary exposure
pax_clean <- pax_clean %>%
  mutate(
    sedentary = SED_MIN,
    sedentary_pct = SED_PERCENT
  )

# peak intensity using 5 min counts because it is less noisy
pax_clean <- pax_clean %>%
  mutate(
    peak_intensity_1min = MAX_1MIN_COUNTS,
    peak_intensity_5min = MAX_5MIN_COUNTS
  )

# log intensity for regression
pax_clean <- pax_clean %>%
  mutate(
    log_peak_intensity = log(MAX_5MIN_COUNTS + 1)
  )


# volume adjusted intensity
int_model <- lm(log_peak_intensity ~ log(steps + 1), data = pax_clean)

pax_clean$intensity_adj <- resid(int_model)

# MVPA proportion of activity
pax_clean <- pax_clean %>%
  mutate(
    mvpa_ratio = MVPA_MIN / (VALID_MIN / 60)
  )

# sedentary fragmentation
pax_clean <- pax_clean %>%
  mutate(
    sed_break_rate = SED_BREAKS / (SED_MIN + 1)
  )

#intensity gradient
pax_clean <- pax_clean %>%
  mutate(
    intensity_gradient = MAX_5MIN_COUNTS / MAX_30MIN_COUNTS
  )

pax <- pax_clean %>%
  select(
    SEQN,
    steps,
    mvpa,
    active,
    sedentary_pct,
    peak_intensity_5min,
    log_peak_intensity,
    intensity_adj,
    mvpa_ratio,
    sed_break_rate
  )
names(pax) <- toupper(names(pax))




# pax <- read.csv("data_raw/nhanes-2026-06-16.csv")

# 1. Process Intensity Data
# Load file from your local downloads directory
# load("data_raw/PAXINTEN_D.rda")
# 
# # 2. Extract specific minute-level column names for calculation
# min_cols <- grep("^MIN",names(PAXINTEN_D),value = TRUE)
# 
# # 3. Separate the IDs from the massive data matrix to prevent structural errors
# metadata <- PAXINTEN_D[, c("SEQN", "WEEKDAY")]
# data_matrix <- as.matrix(PAXINTEN_D[, min_cols])
# 
# # 4. Drop the original massive dataframe to clear RAM space immediately
# rm(PAXINTEN_D)
# gc()
# 
# 
# # 5. Run lightning-fast matrix calculations
# Total_Activity_Volume  <- rowSums(data_matrix, na.rm = TRUE)
# MVPA_Intensity_Minutes <- rowSums(data_matrix >= 2020, na.rm = TRUE)
# Sedentary_Minutes      <- rowSums(data_matrix < 100, na.rm = TRUE)
# Daily_Wear_Minutes     <- rowSums(!is.na(data_matrix))
# 
# 
# # 6. Drop the matrix now that math is done
# rm(data_matrix)
# gc()
# 
# 
# # 7. Bind results back to metadata and build your clean summary
# 
# final_accelerometer_metrics <- metadata %>%
#   mutate(
#     Total_Activity_Volume  = Total_Activity_Volume,
#     MVPA_Intensity_Minutes = MVPA_Intensity_Minutes,
#     Sedentary_Minutes      = Sedentary_Minutes,
#     Daily_Wear_Minutes     = Daily_Wear_Minutes
#   ) %>%
#   # Filter for valid days (Worn for at least 10 hours / 600 minutes)
#   filter(Daily_Wear_Minutes >= 600) %>%
#   # Aggregate to participant level averages
#   group_by(SEQN) %>%
#   summarise(
#     Mean_Daily_Volume = mean(Total_Activity_Volume, na.rm = TRUE),
#     Mean_Daily_MVPA_Min = mean(MVPA_Intensity_Minutes, na.rm = TRUE),
#     Mean_Daily_Sedentary_Min = mean(Sedentary_Minutes, na.rm = TRUE),
#     Number_of_Valid_Days = n()
#   ) %>%
#   # Keep only reliable participant profiles (>= 4 valid days)
#   filter(Number_of_Valid_Days >= 4)
# 
# # Print a preview of your successful dataset!
# head(final_accelerometer_metrics)
# 
# # NEED THIS
# # SEQN
# # PAXDAY
# # PAXSTEP
# # PAXINTEN
# # PAXSTAT
# # PAXCAL
# 
# 
# # 2. Process Step Data
# load("C:/Users/Benyu/Downloads/PAXSTEP_D.rda")
# 
# step_summary <- PAXSTEP_D %>%
#   select(SEQN, WEEKDAY, starts_with("MIN")) %>%
#   rowwise() %>%
#   mutate(
#     Total_Steps = sum(c_across(starts_with("MIN")), na.rm = TRUE)
#   ) %>%
#   ungroup() %>%
#   select(SEQN, WEEKDAY, Total_Steps)
# 
# rm(PAXSTEP_D)
# gc()
# 
# 
# # 3. Combine and Aggregate to Participant Level
# final_accelerometer_data <- left_join(step_summary, intensity_summary, by = c("SEQN", "WEEKDAY"))
# 
# participant_level_data <- final_accelerometer_data %>%
#   group_by(SEQN) %>%
#   summarise(
#     Mean_Daily_Steps = mean(Total_Steps, na.rm = TRUE),
#     Mean_Daily_MVPA = mean(MVPA_Minutes, na.rm = TRUE),
#     Mean_Total_Counts = mean(Total_Activity_Counts, na.rm = TRUE),
#     Valid_Days_Count = n()
#   )
# 
# # 4. Attach Covariates
# load("C:/Users/Benyu/Downloads/Covariate_D.rda")
# 
# my_nhanes_dataset <- Covariate_D %>%
#   select(SEQN, RIDAGEYR, RIAGENDR, bmxbmi) %>%
#   left_join(participant_level_data, ., by = "SEQN")
# 
# # Save your lightweight summary dataset as a CSV or RDS
# write.csv(my_nhanes_dataset, "nhanes_processed_summary.csv", row.names = FALSE)
# print("Complete! Summary metrics extracted successfully.")
# 

#------------------------------------------------------
# inspect
#------------------------------------------------------

names(demo)
names(bmx)
names(cbc)
names(crp)
names(cot)
names(pax)
#------------------------------------------------------
# save imported files
#------------------------------------------------------

save(
  demo,bmx,bpx,
  cbc,crp,biopro,
  glu,ghb,hdl,tchol,
  cot,pax,
  file=file.path(clean_dir,"01_imported_data.RData")
)