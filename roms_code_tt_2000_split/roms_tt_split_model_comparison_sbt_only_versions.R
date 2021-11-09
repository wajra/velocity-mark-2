library(tidyverse)
library(ggplot2)
library(gridExtra)
library(cowplot)

# Following models to be compared
# Null
# T + O
# T + MI
# T + S
# T + Z
# T + O + Z
# T + O + S
# T + O + T:O
# T + O + MI
# T + O + MI + S + Z
# T + O + T:O + S + Z

# Null model
null_model <- read.csv("roms_tt_split_output/model_diagnostics/jb_modeldiag_Nov2017_GMEXdrop_bottom_variables_model_tt_split_2021_04_10.csv")

# T + O
sbt_o2_model <- read.csv("roms_tt_split_output/model_diagnostics/jb_modeldiag_Nov2017_GMEXdrop_sbt_dissolved_oxygen_2021_11_09.csv")

# T + MI
sbt_mi_model <- read.csv("roms_tt_split_output/model_diagnostics/jb_modeldiag_Nov2017_GMEXdrop_sbt_mi_2021_11_08.csv")

# T + S
sbt_salinity_model <- read.csv("roms_tt_split_output/model_diagnostics/jb_modeldiag_Nov2017_GMEXdrop_sbt_salinity_2021_11_08.csv")

# T + Z
sbt_zooplankton_model <- read.csv("roms_tt_split_output/model_diagnostics/jb_modeldiag_Nov2017_GMEXdrop_sbt_zooplankton_2021_11_08.csv")

# T + O + Z
sbt_oxygen_zooplankton_model <- read.csv("roms_tt_split_output/model_diagnostics/jb_modeldiag_Nov2017_GMEXdrop_sbt_dissolved_oxygen_zplk_2021_11_09.csv")

# T + O + S
sbt_oxygen_salinity_model <- read.csv("roms_tt_split_output/model_diagnostics/jb_modeldiag_Nov2017_GMEXdrop_sbt_dissolved_oxygen_salinity_2021_11_09.csv")

# T + O + T:O
sbt_oxygen_interaction_model <- read.csv("roms_tt_split_output/model_diagnostics/jb_modeldiag_Nov2017_GMEXdrop_sbt_interaction_factor_o2_2021_11_08.csv")

# T + O + MI
sbt_oxygen_mi_model <- read.csv("roms_tt_split_output/model_diagnostics/jb_modeldiag_Nov2017_GMEXdrop_sbt_mi_o2_2021_11_09.csv")

# T + O + MI + S + Z
full_model_with_mi <- read.csv("roms_tt_split_output/model_diagnostics/jb_modeldiag_Nov2017_GMEXdrop_full_model_with_only_sbt_2021_11_09.csv")

# T + O + T:O + S + Z
full_model_with_interaction <- read.csv("roms_tt_split_output/model_diagnostics/jb_modeldiag_Nov2017_GMEXdrop_full_model_o2_interaction_with_only_sbt_2021_11_09.csv")

# Now let's introduce a new column to each of the 4 dataframes
null_model <- add_column(null_model, model_name = 'Null', .after = 1)
sbt_o2_model <- add_column(sbt_o2_model, model_name = 'T+O', .after = 1)
sbt_mi_model <- add_column(sbt_mi_model, model_name = 'T+MI', .after = 1)
sbt_salinity_model <- add_column(sbt_salinity_model, model_name = 'T+S', .after = 1)
sbt_zooplankton_model <- add_column(sbt_zooplankton_model, model_name = 'T+Z', .after = 1)
sbt_oxygen_zooplankton_model <- add_column(sbt_oxygen_zooplankton_model, model_name = 'T+O+Z', .after = 1)
sbt_oxygen_salinity_model <- add_column(sbt_oxygen_salinity_model, model_name = 'T+O+S', .after = 1)
sbt_oxygen_interaction_model <- add_column(sbt_oxygen_interaction_model, model_name = 'T+O+T:O', .after = 1)
sbt_oxygen_mi_model <- add_column(sbt_oxygen_mi_model, model_name = 'T+O+MI', .after = 1)
full_model_with_mi <- add_column(full_model_with_mi, model_name= 'T+O+MI+S+Z', .after=1)
full_model_with_interaction <- add_column(full_model_with_interaction, model_name= 'T+O+T:O+S+Z', .after=1)



combination_comparison_df_gmex <- rbind(null_model,
                                        sbt_o2_model,
                                        sbt_mi_model,
                                        sbt_salinity_model,
                                        sbt_zooplankton_model,
                                        sbt_oxygen_zooplankton_model,
                                        sbt_oxygen_salinity_model,
                                        sbt_oxygen_interaction_model,
                                        sbt_oxygen_mi_model,
                                        full_model_with_mi,
                                        full_model_with_interaction)

# Reorder everything
combination_comparison_df_gmex$model_name <- 
  factor(combination_comparison_df_gmex$model_name,
         levels = c("T+O+T:O+S+Z","T+O+MI+S+Z","T+O+MI", "T+O+T:O", "T+O+S",
                    "T+O+Z", "T+Z", "T+S","T+MI", "T+O", "Null"))



aic_pres_plot <- combination_comparison_df_gmex %>%
  ggplot( aes(x=model_name, y=pres.aic.tt)) +
  geom_bar(stat="identity", fill="black", alpha=.6, width=.4) +
  coord_flip(ylim=c(8500,10700), xlim=c(1,14)) +
  xlab("") +
  ylab("AIC") +
  geom_text(x=12.5, y=8500, label='(a)', size=6) +
  theme_bw() +
  theme(text=element_text(size=18))
# TSS Plot
tss_plot <- combination_comparison_df_gmex %>%
  ggplot( aes(x=model_name, y=tss.tt)) +
  geom_bar(stat="identity", fill="black", alpha=.6, width=.4) +
  coord_flip(ylim=c(0.11,0.18), xlim=c(1,14)) +
  xlab("") +
  ylab("TSS") +
  geom_text(x=12.5, y=0.11, label='(b)', size=6) +
  theme_bw() +
  theme(text=element_text(size=18))

combination_variables_plot <- grid.arrange(aic_pres_plot, tss_plot, nrow=2)
ggsave("roms_tt_split_output/figures/model_comparisons_pres_abs_with_only_sbt.png", combination_variables_plot,
       width= 6, height=6, units="in")
