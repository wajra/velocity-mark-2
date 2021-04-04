library(tidyverse)
library(ggplot2)
library(gridExtra)

# Normal random Null model
normal_null_model <- read.csv(paste0("roms_model_output_ver_1/jb_modeldi",
    "ag_Nov2017_GMEXdrop_random_null_model_2020_09_21.csv"))

# Uniform random null model
uniform_null_model <- read.csv(paste0("roms_model_output_ver_1/jb_modeldi",
    "ag_Nov2017_GMEXdrop_uniform_null_model_2020_09_22.csv"))

# Mean null model
# mean_null_model <- read.csv(paste0("roms_model_output_ver_1/jb_modeldi",
#                                    "ag_Nov2017_GMEXdrop_mean_null_model_2020_09_22.csv"))

# Now let's introduce a new column to each of the 4 dataframes
normal_null_model <- add_column(normal_null_model, model_name = 'Normal model', .after = 1)
uniform_null_model <- add_column(uniform_null_model, model_name = 'Uniform model', .after = 1)
# mean_null_model <- add_column(mean_null_model, model_name = 'Mean model', .after = 1)

comparison_df_gmex <- rbind(normal_null_model, uniform_null_model)

# AUC 

auc_plot <- comparison_df_gmex %>%
  ggplot( aes(x=model_name, y=auc)) +
  geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4) +
  coord_flip() +
  xlab("") +
  theme_bw()

# TSS

tss_plot <- comparison_df_gmex %>%
  ggplot( aes(x=model_name, y=tss)) +
  geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4) +
  coord_flip() +
  xlab("") +
  theme_bw()

# Kappa

kappa_plot <- comparison_df_gmex %>%
  ggplot( aes(x=model_name, y=kappa)) +
  geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4) +
  coord_flip() +
  xlab("") +
  theme_bw()

# Accuracy

acc_plot <- comparison_df_gmex %>%
  ggplot( aes(x=model_name, y=acc)) +
  geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4) +
  coord_flip() +
  xlab("") +
  theme_bw()

model_statistics_plot <- grid.arrange(auc_plot, tss_plot, kappa_plot, acc_plot, nrow=2, ncol=2)

ggsave("roms_figures_output_ver_1/model_comparisons_new_null_models.png", model_statistics_plot)

write.csv(comparison_df_gmex, file=paste("roms_model_output_ver_1/null_model_comparisons.csv"))