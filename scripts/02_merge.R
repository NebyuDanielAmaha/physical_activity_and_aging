# =====================================================
# 02_merge.R
# =====================================================

load(
  file.path(
    "data_clean",
    "01_imported_data.RData"
  )
)

analytic <- demo %>%
  left_join(bmx,by="SEQN") %>%
  left_join(bpx,by="SEQN") %>%
  left_join(cbc,by="SEQN") %>%
  left_join(crp,by="SEQN") %>%
  left_join(biopro,by="SEQN") %>%
  left_join(glu,by="SEQN") %>%
  left_join(ghb,by="SEQN") %>%
  left_join(hdl,by="SEQN") %>%
  left_join(tchol,by="SEQN") %>%
  left_join(cot,by="SEQN") %>%
  left_join(pax,by="SEQN")

dim(analytic)

saveRDS(
  analytic,
  "data_clean/02_analytic_merged.rds"
)