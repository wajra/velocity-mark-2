library(tidyverse)
library(ggplot2)
library(gridExtra)
library(cowplot)
# Null model
null_model <- read.csv('roms_model_output_ver_1/jb_modeldiag_Nov2017_GMEXdrop_null_model_2020_08_20.csv')

# Classic model
classic_model <- read.csv('roms_model_output_ver_1/jb_modeldiag_Nov2017_GMEXdrop_classic_combination_model_2020_08_28.csv')

# O2 classic model
o2_combination_model <- read.csv('roms_model_output_ver_1/jb_modeldiag_Nov2017_GMEXdrop_o2_combination_model_2020_08_28.csv')

# Salinity classic model
salinity_classic_model <- read.csv('roms_model_output_ver_1/jb_modeldiag_Nov2017_GMEXdrop_salinity_combination_model_2020_08_28.csv')

# Zooplankton classic model
zooplankton_classic_model <- read.csv('roms_model_output_ver_1/jb_modeldiag_Nov2017_GMEXdrop_zooplankton_combination_model_2020_08_28.csv')

# Salinity O2 combination model
salinity_o2_combination_model <- read.csv('roms_model_output_ver_1/jb_modeldiag_Nov2017_GMEXdrop_o2_salinity_combination_model_2020_08_28.csv')

# O2 zooplankton model
o2_zooplankton_model <- read.csv('roms_model_output_ver_1/jb_modeldiag_Nov2017_GMEXdrop_o2_zooplankton_combination_model_2020_08_28.csv')

# Interaction model
interaction_model <- read.csv('roms_model_output_ver_1/jb_modeldiag_Nov2017_GMEXdrop_o2_sbt_interaction_combination_model_2020_08_28.csv')

# MI model
mi_model <- read.csv('roms_model_output_ver_1/jb_modeldiag_Nov2017_GMEXdrop_metabolic_index_smoother_model_2020_12_24.csv')

"
null_model <- add_column(null_model, model_name = 'Null model', .after = 1)
classic_model <- add_column(classic_model, model_name = 'Temperature model', .after = 1)
o2_combination_model <- add_column(o2_combination_model, model_name = 'Temperature + DO', .after = 1)
salinity_classic_model <- add_column(salinity_classic_model, model_name = 'Temperature + Salinity', .after = 1)
zooplankton_classic_model <- add_column(zooplankton_classic_model, model_name = 'Temperature + Zooplankton', .after = 1)
salinity_o2_combination_model <- add_column(salinity_o2_combination_model, model_name = 'Temperature + DO + Salinity', .after = 1)
o2_zooplankton_model <- add_column(o2_zooplankton_model, model_name = 'Temperature + DO + Zooplankton', .after = 1)
interaction_model <- add_column(interaction_model, model_name = 'Temperature + DO + s(Temperature,DO)', .after = 1)
mi_model <- add_column(mi_model, model_name = 'Temperature + DO + MI', .after = 1)
"

# Now let's introduce a new column to each of the 4 dataframes
null_model <- add_column(null_model, model_name = 'Null', .after = 1)
classic_model <- add_column(classic_model, model_name = 'T', .after = 1)
o2_combination_model <- add_column(o2_combination_model, model_name = 'T+O', .after = 1)
salinity_classic_model <- add_column(salinity_classic_model, model_name = 'T+S', .after = 1)
zooplankton_classic_model <- add_column(zooplankton_classic_model, model_name = 'T+Z', .after = 1)
o2_zooplankton_model <- add_column(o2_zooplankton_model, model_name = 'T+O+Z', .after = 1)
salinity_o2_combination_model <- add_column(salinity_o2_combination_model, model_name = 'T+O+S', .after = 1)
interaction_model <- add_column(interaction_model, model_name = 'T:O', .after = 1)
mi_model <- add_column(mi_model, model_name = 'T+O+MI', .after = 1)


combination_comparison_df_gmex <- rbind(null_model,
                                        classic_model,
                                        o2_combination_model,
                                        salinity_classic_model,
                                        zooplankton_classic_model,
                                        o2_zooplankton_model,
                            salinity_o2_combination_model,
                            interaction_model,
                            mi_model)

# Reorder everything
combination_comparison_df_gmex$model_name <- 
  factor(combination_comparison_df_gmex$model_name,
         levels = c("T+O+MI", "T:O", "T+O+S",
                    "T+O+Z", "T+Z", "T+S", "T+O", "T", "Null"))


# Now get the minimum for each metric and then subtract other model values from it
# 1 - auc
# auc_min <- min(combination_comparison_df_gmex$auc, na.rm = TRUE)
# combination_comparison_df_gmex$auc <- combination_comparison_df_gmex$auc - auc_min

# AUC 
# The older color
# #f68060
auc_plot <- combination_comparison_df_gmex %>%
  ggplot( aes(x=model_name, y=auc)) +
  geom_bar(stat="identity", fill="black", alpha=.6, width=.4) +
  coord_flip(ylim=c(0.75,0.95), xlim=c(1,11)) +
  xlab("") +
  ylab("AUC") +
  geom_text(x=10.5, y=0.75, label='(a)') +
  theme_bw() +
  theme(text=element_text(size=16))


# TSS

tss_plot <- combination_comparison_df_gmex %>%
  ggplot( aes(x=model_name, y=tss)) +
  geom_bar(stat="identity", fill="black", alpha=.6, width=.4) +
  coord_flip(ylim=c(0.1,0.2), xlim=c(1,11)) +
  xlab("") +
  ylab("TSS") +
  geom_text(x=10.5, y=0.1, label='(b)') +
  theme_bw() +
  theme(text=element_text(size=16))


# Kappa

kappa_plot <- combination_comparison_df_gmex %>%
  ggplot( aes(x=model_name, y=kappa)) +
  geom_bar(stat="identity", fill="black", alpha=.6, width=.4) +
  coord_flip(ylim=c(0.1,.275), xlim=c(1,11)) +
  xlab("") +
  ylab("Kappa") +
  geom_text(x=10.5, y=0.1, label='(c)') +
  theme_bw() +
  theme(text=element_text(size=16))


# Accuracy

acc_plot <- combination_comparison_df_gmex %>%
  ggplot( aes(x=model_name, y=acc)) +
  geom_bar(stat="identity", fill="black", alpha=.6, width=.4) +
  coord_flip(ylim=c(0.6,0.81), xlim=c(1,11)) +
  xlab("") +
  ylab("Accuracy") +
  geom_text(x=10.5, y=0.6, label='(d)') +
  theme_bw() +
  theme(text=element_text(size=16))




# Sensitivity
sens_plot <- combination_comparison_df_gmex %>%
  ggplot( aes(x=model_name, y=sens)) +
  geom_bar(stat="identity", fill="black", alpha=.6, width=.4) +
  coord_flip(ylim=c(0.875, 0.91), xlim=c(1,11)) +
  xlab("") +
  ylab("Sensitivity") +
  geom_text(x=10.5, y=0.875, label='(e)') +
  theme_bw() +
  theme(text=element_text(size=16))


# model_statistics_plot <- grid.arrange(auc_plot, tss_plot,
#                                      kappa_plot, acc_plot,
#                                      sens_plot,
#                                      nrow=3, ncol=2)

#ggsave("roms_figures_output_ver_1/model_presence_comparisons_combination_02_20.png",
#       model_statistics_plot, width=8, height= 5, units=c("in"))

# Now we need to do the same for abundance
# The metrics for abundance are dev.biomass and r2.all (For tt split r2.biomass)

#abundance_statistic_plot <- grid.arrange(biomass_dev_plot, r2_all,
#                                         nrow=1, ncol=2)


#ggsave("roms_figures_output_ver_1/model_abundance_comparisons_combination_01_06.png", abundance_statistic_plot,
#       width= 10.5, height=6, units="in")

write.csv(combination_comparison_df_gmex, file=paste("roms_model_output_ver_1/combination_model_comparisons_01_06.csv"))


# New plots
# Color for abundance - #99ff99
# Have to recolor the abundance plots
biomass_dev_plot <- combination_comparison_df_gmex %>%
  ggplot( aes(x=model_name, y=dev.biomass)) +
  geom_bar(stat="identity", fill="black", alpha=.6, width=.4) +
  coord_flip(ylim=c(0.5,0.82), xlim=c(1,11)) +
  xlab("") +
  ylab("Deviance") +
  geom_text(x=10.5, y=0.5, label='(f)') +
  theme_bw() +
  theme(text=element_text(size=16))

#theme(axis.text.y = element_blank())


r2_all <- combination_comparison_df_gmex %>%
  ggplot( aes(x=model_name, y=r2.all)) +
  geom_bar(stat="identity", fill="black", alpha=.6, width=.4) +
  coord_flip(ylim=c(0.00001,0.0025), xlim=c(1,11)) +
  xlab("") +
  ylab("R2 - Biomass") +
  geom_text(x=10.5, y=0.00001, label='(g)') +
  theme_bw() +
  theme(text=element_text(size=16))


combination_variables_plot <- grid.arrange(auc_plot, tss_plot,
             kappa_plot, acc_plot,
             sens_plot, biomass_dev_plot, r2_all,
             nrow=4, ncol=2)

# Let's try cowplot
# Example - https://stackoverflow.com/questions/36198451/specify-widths-and-heights-of-plots-with-grid-arrange
'''
plot_grid(auc_plot, tss_plot,
          kappa_plot, acc_plot,
          sens_plot, biomass_dev_plot, r2_all,
          align = "v", nrow = 4, rel_widths = c(1/2,1/2))
'''
ggsave("roms_figures_output_ver_1/model_abundance_comparisons_combination_total_ver_8.png", combination_variables_plot,
       width= 10, height=8, units="in")
