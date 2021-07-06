# SAVE TABLES ------------------------------------------------------------------

# save npar_t_tests_res
write.table(npar_t_tests_res,
  sep = ",",
  file = "./outputs/tables/TabS1.csv",
  quote = FALSE, row.names = TRUE
)

# SAVE FIGURES -----------------------------------------------------------------

# save fig_pval_relef
ggsave(
  filename = paste0("./outputs/figures/pdf/", "FigS1", ".pdf"),
  plot = fig_pval_relef,
  width = width_factor * 105, height = height_factor * 74.25, units = "mm"
)
ggsave(
  filename = paste0("./outputs/figures/png/", "FigS1", ".png"),
  plot = fig_pval_relef,
  width = width_factor * 105, height = height_factor * 74.25, units = "mm"
)
ggsave(
  filename = paste0("./outputs/figures/jpg_low-quality/", "FigS1", ".jpg"),
  plot = fig_pval_relef,
  width = width_factor * 105, height = height_factor * 74.25,
  units = "mm", dpi = 100
)