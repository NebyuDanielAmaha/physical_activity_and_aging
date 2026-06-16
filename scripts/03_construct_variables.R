# =====================================================
# NHANES 2005–2006
# 03_construct_variables.R
# =====================================================

#------------------------------------------------------
# load merged dataset
#------------------------------------------------------

analytic <- readRDS("data_clean/02_analytic_merged.rds")

setDT(analytic)

#------------------------------------------------------
# 1. DEMOGRAPHIC VARIABLES
#------------------------------------------------------

analytic[, age := RIDAGEYR]

analytic[, sex := factor(RIAGENDR,
                         levels = c(1,2),
                         labels = c("Male","Female"))]

analytic[, race := factor(RIDRETH1)]

analytic[, education := RIDRETH1]   # replace later if you merge DMDEDUC2

analytic[, income := INDHHINC]      # check availability in your DEMO file

#------------------------------------------------------
# 2. BMI + OBESITY CATEGORIES
#------------------------------------------------------

analytic[, bmi := BMXBMI]

analytic[, bmi_cat :=
           fifelse(bmi < 18.5, "Underweight",
                   fifelse(bmi < 25, "Normal",
                           fifelse(bmi < 30, "Overweight", "Obese")))]

#------------------------------------------------------
# 3. BLOOD PRESSURE (mean SBP/DBP)
#------------------------------------------------------

bp_cols <- names(analytic)[grepl("^BPXSY|^BPXDI", names(analytic))]

analytic[, sbp := rowMeans(.SD, na.rm = TRUE),
         .SDcols = grep("^BPXSY", names(analytic), value = TRUE)]

analytic[, dbp := rowMeans(.SD, na.rm = TRUE),
         .SDcols = grep("^BPXDI", names(analytic), value = TRUE)]

analytic[, hypertension :=
           ifelse(sbp >= 140 | dbp >= 90, 1, 0)]

#------------------------------------------------------
# 4. INFLAMMATION (CRP)
#------------------------------------------------------

analytic[, crp := LBXCRP]

analytic[, log_crp := log(crp + 0.01)]

#------------------------------------------------------
# 5. LIPIDS
#------------------------------------------------------

analytic[, hdl := LBDHDD]

analytic[, tchol := LBXTC]

analytic[, dyslipidemia :=
           ifelse(hdl < 40 | tchol >= 240, 1, 0)]

#------------------------------------------------------
# 6. GLYCEMIA
#------------------------------------------------------

analytic[, glucose := LBXGLU]

analytic[, hba1c := LBXGH]

analytic[, diabetes :=
           ifelse(glucose >= 126 | hba1c >= 6.5, 1, 0)]

#------------------------------------------------------
# 7. SMOKING (COTININE)
#------------------------------------------------------

analytic[, cotinine := LBXCOT]

analytic[, smoking :=
           fifelse(cotinine > 10, "Current smoker",
                   fifelse(cotinine > 0.05, "Passive smoker", "Non-smoker"))]

analytic[, log_cotinine := log(cotinine + 0.01)]


#------------------------------------------------------
# 8. ACCELEROMETER VARIABLES (FROM YOUR PAX OBJECT)
#------------------------------------------------------

analytic <- analytic %>%
  mutate(
    steps = coalesce(STEPS.x, STEPS.y, steps),
    mvpa  = coalesce(MVPA.x, MVPA.y),
    active = coalesce(ACTIVE.x, ACTIVE.y),
    sedentary_pct = coalesce(SEDENTARY_PCT.x, SEDENTARY_PCT.y),
    peak_intensity = coalesce(PEAK_INTENSITY_5MIN.x, PEAK_INTENSITY_5MIN.y),
    intensity_adj = coalesce(INTENSITY_ADJ.x, INTENSITY_ADJ.y),
    log_intensity = coalesce(LOG_PEAK_INTENSITY.x, LOG_PEAK_INTENSITY.y)
  )

analytic <- analytic %>%
  select(-ends_with(".x"), -ends_with(".y"))


# # merge cleaned pax exposures
# analytic <- merge(analytic, pax, by = "SEQN", all.x = TRUE)
# 
# # ensure consistent naming
# analytic[, steps := STEPS.x]
# analytic[, mvpa := MVPA_MIN]
# analytic[, sedentary := SED_MIN]
# 
# # derived PA ratios
# analytic[, mvpa_ratio := MVPA_MIN / (VALID_MIN + 1)]
# 
# analytic[, sedentary_pct := SED_PERCENT]
# 
# analytic[, intensity_log := log(MAX_5MIN_COUNTS + 1)]
# 
# # intensity adjusted for volume (key variable for your PhD paper)
# model_int <- lm(intensity_log ~ log(steps + 1), data = analytic)
# analytic[, intensity_adj := resid(model_int)]
# 
# # fragmentation
# analytic[, sed_break_rate := SED_BREAKS / (SED_MIN + 1)]

#------------------------------------------------------
# 9. STANDARDIZED VARIABLES (FOR FARAWAY MODELS)
#------------------------------------------------------

analytic <- analytic %>%
  mutate(
    steps_z = scale(steps),
    mvpa_z = scale(mvpa),
    intensity_z = scale(log_intensity)
  )



analytic[, z_steps := scale(steps)]
analytic[, z_mvpa := scale(mvpa)]
analytic[, z_sedentary := scale(sedentary)]
analytic[, z_intensity := scale(intensity_adj)]

#------------------------------------------------------
# 10. FINAL ANALYTIC FILTERS
#------------------------------------------------------

analytic <- analytic[
  !is.na(age) &
    !is.na(bmi) &
    !is.na(steps)
]

# optional: remove extreme accelerometer values
analytic <- analytic[steps > 0 & steps < quantile(steps, 0.99, na.rm = TRUE)]

#------------------------------------------------------
# 11. SAVE FINAL ANALYTIC DATASET
#------------------------------------------------------

saveRDS(analytic,
        "data_clean/03_constructed_variables.rds")

# preview
glimpse(analytic)