# save tables --------------------------------------------------------------

# save desc_data
write.table(desc_data,
  sep = ",",
  file = "./outputs/tables/TabS1.csv",
  quote = FALSE, col.names = FALSE
)

# save desc_stat
write.table(desc_stat,
  sep = ",",
  file = "./outputs/tables/TabS2.csv",
  quote = FALSE, row.names = FALSE
)

# save desc_cor
write.table(desc_cor,
  sep = ",",
  file = "./outputs/tables/TabS3.csv",
  quote = FALSE, row.names = TRUE
)

# save auc
write.table(auc,
  sep = ",",
  file = "./outputs/tables/TabS4.csv",
  quote = FALSE, col.names = FALSE
)

# save plots ---------------------------------------------------------------

# save plot p_cor
ggsave(
  filename = paste0("./outputs/figures/pdf/", "FigS1", ".pdf"),
  plot = p_cor,
  width = width_factor * 105, height = height_factor * 74.25, units = "mm"
)
ggsave(
  filename = paste0("./outputs/figures/png/", "FigS1", ".png"),
  plot = p_cor,
  width = width_factor * 105, height = height_factor * 74.25, units = "mm"
)
ggsave(
  filename = paste0("./outputs/figures/jpg_low-quality/", "FigS1", ".jpg"),
  plot = p_cor,
  width = width_factor * 105, height = height_factor * 74.25,
  units = "mm", dpi = 100
)

# save plot p_rf_tree
ggsave(
  filename = paste0("./outputs/figures/pdf/", "FigS2", ".pdf"),
  plot = p_rf_tree,
  width = width_factor * 105, height = height_factor * 74.25, units = "mm"
)
ggsave(
  filename = paste0("./outputs/figures/png/", "FigS2", ".png"),
  plot = p_rf_tree,
  width = width_factor * 105, height = height_factor * 74.25, units = "mm"
)
ggsave(
  filename = paste0("./outputs/figures/jpg_low-quality/", "FigS2", ".jpg"),
  plot = p_rf_tree,
  width = width_factor * 105, height = height_factor * 74.25,
  units = "mm", dpi = 100
)

# save plot p_choose_imp_1
ggsave(
  filename = paste0("./outputs/figures/pdf/", "FigS3", ".pdf"),
  plot = p_choose_imp_1,
  width = width_factor * 105, height = height_factor * 74.25, units = "mm"
)
ggsave(
  filename = paste0("./outputs/figures/png/", "FigS3", ".png"),
  plot = p_choose_imp_1,
  width = width_factor * 105, height = height_factor * 74.25, units = "mm"
)
ggsave(
  filename = paste0("./outputs/figures/jpg_low-quality/", "FigS3", ".jpg"),
  plot = p_choose_imp_1,
  width = width_factor * 105, height = height_factor * 74.25,
  units = "mm", dpi = 100
)

# save plot p_choose_imp_2
ggsave(
  filename = paste0("./outputs/figures/pdf/", "FigS4", ".pdf"),
  plot = p_choose_imp_2,
  width = width_factor * 105, height = height_factor * 74.25, units = "mm"
)
ggsave(
  filename = paste0("./outputs/figures/png/", "FigS4", ".png"),
  plot = p_choose_imp_2,
  width = width_factor * 105, height = height_factor * 74.25, units = "mm"
)
ggsave(
  filename = paste0("./outputs/figures/jpg_low-quality/", "FigS4", ".jpg"),
  plot = p_choose_imp_2,
  width = width_factor * 105, height = height_factor * 74.25,
  units = "mm", dpi = 100
)

# save plot p_imp
ggsave(
  filename = paste0("./outputs/figures/pdf/", "FigA1", ".pdf"),
  plot = p_imp,
  width = width_factor * 105, height = height_factor * 74.25, units = "mm"
)
ggsave(
  filename = paste0("./outputs/figures/png/", "FigA1", ".png"),
  plot = p_imp,
  width = width_factor * 105, height = height_factor * 74.25, units = "mm"
)
ggsave(
  filename = paste0("./outputs/figures/jpg_low-quality/", "FigA1", ".jpg"),
  plot = p_imp, width = width_factor * 105, height = height_factor * 74.25,
  units = "mm", dpi = 100
)

# save plot p_min_depth
ggsave(
  filename = paste0("./outputs/figures/pdf/", "FigA2", ".pdf"),
  plot = p_min_depth,
  width = width_factor * 105, height = height_factor * 74.25, units = "mm"
)
ggsave(
  filename = paste0("./outputs/figures/png/", "FigA2", ".png"),
  plot = p_min_depth,
  width = width_factor * 105, height = height_factor * 74.25, units = "mm"
)
ggsave(
  filename = paste0("./outputs/figures/jpg_low-quality/", "FigA2", ".jpg"),
  plot = p_min_depth,
  width = width_factor * 105, height = height_factor * 74.25,
  units = "mm", dpi = 100
)

# save plot p_interaction
ggsave(
  filename = paste0("./outputs/figures/pdf/", "FigA3", ".pdf"),
  plot = p_interaction,
  width = width_factor * 105, height = height_factor * 74.25, units = "mm"
)
ggsave(
  filename = paste0("./outputs/figures/png/", "FigA3", ".png"),
  plot = p_interaction,
  width = width_factor * 105, height = height_factor * 74.25, units = "mm"
)
ggsave(
  filename = paste0("./outputs/figures/jpg_low-quality/", "FigA3", ".jpg"),
  plot = p_interaction,
  width = width_factor * 105, height = height_factor * 74.25,
  units = "mm", dpi = 100
)