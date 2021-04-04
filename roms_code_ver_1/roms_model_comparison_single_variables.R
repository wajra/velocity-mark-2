library(tidyverse)
library(ggplot2)
library(gridExtra)

# Null model
null_model <- read.csv(paste0("roms_model_output_ver_1/jb_modeldi",
                       "ag_Nov2017_GMEXdrop_null_model_2020_08_20.csv"))

# Salinity
salinity_model <- read.csv('roms_model_output_ver_1/jb_modeldiag_Nov2017_GMEXdrop_salinity_model_2020_08_26.csv')

# SST max
sst_max_model <- read.csv('roms_model_output_ver_1/jb_modeldiag_Nov2017_GMEXdrop_sst_max_model_2020_08_28.csv')

# SBT max
sbt_max_model <- read.csv('roms_model_output_ver_1/jb_modeldiag_Nov2017_GMEXdrop_sbt_max_model_2020_08_28.csv')

# SBT min
sbt_min_model <- read.csv('roms_model_output_ver_1/jb_modeldiag_Nov2017_GMEXdrop_sbt_min_model_2020_08_28.csv')

# SST seasonal mean
sst_seasonal_model <- read.csv('roms_model_output_ver_1/jb_modeldiag_Nov2017_GMEXdrop_sst_seasonal_model_2020_08_28.csv')

# O2 seasonal mean
o2_seasonal_model <- read.csv('roms_model_output_ver_1/jb_modeldiag_Nov2017_GMEXdrop_o2_seasonal_model_2020_08_28.csv')

# Small zooplankton
sm_zplk_model <- read.csv('roms_model_output_ver_1/jb_modeldiag_Nov2017_GMEXdrop_sm_zplk_seasonal_model_2020_08_28.csv')

# SBT seasonal
sbt_seasonal_model <- read.csv('roms_model_output_ver_1/jb_modeldiag_Nov2017_GMEXdrop_sbt_seasonal_model_2020_08_28.csv')

# Metabolic index
mi_model <- read.csv('roms_model_output_ver_1/jb_modeldiag_Nov2017_GMEXdrop_mi_run_2021_01_05.csv')


# Now let's introduce a new column to each of the 4 dataframes
null_model <- add_column(null_model, model_name = 'Null',
                         .after = 1)
salinity_model <- add_column(salinity_model, model_name = 'S',
                             .after = 1)
sst_max_model <- add_column(sst_max_model, model_name = 'Max.SST',
                            .after = 1)
sbt_max_model <- add_column(sbt_max_model, model_name = 'Max.SBT',
                            .after = 1)
sbt_min_model <- add_column(sbt_min_model, model_name = 'Min.SBT',
                            .after = 1)
sst_seasonal_model <- add_column(sst_seasonal_model,
                                 model_name = 'SST', .after = 1)
o2_seasonal_model <- add_column(o2_seasonal_model,
                                model_name = 'O', .after = 1)
sm_zplk_model <- add_column(sm_zplk_model,
                            model_name = 'Z', .after = 1)
sbt_seasonal_model <- add_column(sbt_seasonal_model,
                                 model_name = 'SBT', .after = 1)
mi_model <- add_column(mi_model,
                       model_name='MI', .after= 1)

comparison_df_gmex <- rbind(null_model, salinity_model, sst_max_model,
                            sbt_max_model, sbt_min_model, sst_seasonal_model,
                            sbt_seasonal_model, o2_seasonal_model,
                            sm_zplk_model, mi_model)
comparison_df_gmex$model_name <- 
  factor(comparison_df_gmex$model_name,
         levels = c("MI", "Z", "O",
                    "SBT", "SST", "Min.SBT", "Max.SBT", "Max.SST","S", "Null"))

'''
auc_plot <- combination_comparison_df_gmex %>%
  ggplot( aes(x=model_name, y=auc)) +
  geom_bar(stat="identity", fill="black", alpha=.6, width=.4) +
  coord_flip(ylim=c(0.75,0.95)) +
  xlab("") +
  ylab("(a) AUC") +
  theme_bw() +
  theme(text=element_text(size=14))
'''

# AUC 

auc_plot <- comparison_df_gmex %>%
  ggplot( aes(x=model_name, y=auc)) +
  geom_bar(stat="identity", fill="black", alpha=.6, width=.4) +
  coord_flip(ylim=c(0.80,0.88), xlim=c(1,12)) +
  xlab("") +
  ylab("AUC") +
  geom_text(x=11.5, y=0.8, label='(a)') +
  theme_bw() +
  theme(text=element_text(size=14))


# TSS

tss_plot <- comparison_df_gmex %>%
  ggplot( aes(x=model_name, y=tss)) +
  geom_bar(stat="identity", fill="black", alpha=.6, width=.4) +
  coord_flip(ylim=c(0.11,0.16), xlim=c(1,12)) +
  xlab("") +
  ylab("TSS") +
  geom_text(x=11.5, y=0.11, label='(b)') +
  theme_bw() +
  theme(text=element_text(size=14))


# Kappa

kappa_plot <- comparison_df_gmex %>%
  ggplot( aes(x=model_name, y=kappa)) +
  geom_bar(stat="identity", fill="black", alpha=.6, width=.4) +
  coord_flip(ylim=c(0.13,0.21), xlim=c(1,12)) +
  xlab("") +
  ylab("Kappa") +
  geom_text(x=11.5, y=0.13, label='(c)') +
  theme_bw() +
  theme(text=element_text(size=14))


# Accuracy

acc_plot <- comparison_df_gmex %>%
  ggplot( aes(x=model_name, y=acc)) +
  geom_bar(stat="identity", fill="black", alpha=.6, width=.4) +
  coord_flip(ylim=c(0.65,0.76), xlim=c(1,12)) +
  xlab("") +
  ylab("Accuracy") +
  geom_text(x=11.5, y=0.65, label='(d)') +
  theme_bw() +
  theme(text=element_text(size=14))



# Sensitivity
sens_plot <- comparison_df_gmex %>%
  ggplot( aes(x=model_name, y=sens)) +
  geom_bar(stat="identity", fill="black", alpha=.6, width=.4) +
  coord_flip(ylim=c(0.86,0.91), xlim=c(1,12)) +
  xlab("") +
  ylab("Sensitivity") +
  geom_text(x=11.5, y=0.86, label='(e)') +
  theme_bw() +
  theme(text=element_text(size=14))



#model_statistics_plot <- grid.arrange(auc_plot, tss_plot, 
#                                      kappa_plot, acc_plot,
#                                      sens_plot,
#                                      nrow=3, ncol=2)

#ggsave("roms_figures_output_ver_1/model_presence_comparisons_single_2021_01_06.png", model_statistics_plot)



# Now we need to do the same for abundance
# The metrics for abundance are dev.biomass and r2.all (For tt split r2.biomass)


biomass_dev_plot <- comparison_df_gmex %>%
  ggplot( aes(x=model_name, y=dev.biomass)) +
  geom_bar(stat="identity", fill="black", alpha=.6, width=.4) +
  coord_flip(ylim=c(0.53,0.72), xlim=c(1,12)) +
  xlab("") +
  ylab("Deviance") +
  geom_text(x=11.5, y=0.53, label='(f)') +
  theme_bw() +
  theme(text=element_text(size=14))



r2_all <- comparison_df_gmex %>%
  ggplot( aes(x=model_name, y=r2.all)) +
  geom_bar(stat="identity", fill="black", alpha=.6, width=.4) +
  coord_flip(ylim=c(0,0.00018), xlim=c(1,12)) +
  xlab("") +
  ylab("R2 - Biomass") +
  geom_text(x=11.5, y=7.4e-07, label='(g)') +
  theme_bw() +
  theme(text=element_text(size=14))


# abundance_statistic_plot <- grid.arrange(biomass_dev_plot, r2_all,
#                                      nrow=1, ncol=2)


# ggsave("roms_figures_output_ver_1/model_abundance_comparisons_single_2021_01_06.png", abundance_statistic_plot,
#       width=10, height=8, units="in")

write.csv(comparison_df_gmex, file=paste("roms_model_output_ver_1/single_model_comparisons_2021_04_03.csv"))

single_variables_plot <- grid.arrange(auc_plot, tss_plot,
                                      kappa_plot, acc_plot,
                                      sens_plot, biomass_dev_plot, r2_all,
                                      nrow=4, ncol=2)

ggsave("roms_figures_output_ver_1/model_abundance_comparisons_single_total_ver_8.png", single_variables_plot,
       width=10, height=8, units="in")