# Visualizing Interactions - Part III
# Reading the output from Part I and then plotting them together
# Author - Jeewantha Bandara (mailto:jeewantha.bandara@rutgers.edu) 
# Research Group - Pinsky Lab, Rutgers University (https://pinsky.marine.rutgers.edu)
# Following Advanced R style guide - http://adv-r.had.co.nz/Style.html


library(tidyverse)


abundance_predictions_df <- read_csv("visualizing_interactions/abundance_predictions.csv")
abundance_se_predictions_df <- read_csv("visualizing_interactions/abundance_se_predictions.csv")
presence_predictions_df <- read_csv("visualizing_interactions/presence_predictions.csv")
presence_se_predictions_df <- read_csv("visualizing_interactions/presence_se_predictions.csv")

abundance_predictions_df$O2.seasonal <- abundance_predictions_df$O2.seasonal*32*1000/0.9766
abundance_se_predictions_df$O2.seasonal <- abundance_se_predictions_df$O2.seasonal*32*1000/0.9766
presence_predictions_df$O2.seasonal <- presence_predictions_df$O2.seasonal*32*1000/0.9766
presence_se_predictions_df$O2.seasonal <- presence_se_predictions_df$O2.seasonal*32*1000/0.9766

abundance_plot <- ggplot(abundance_predictions_df, 
                         aes(SBT.seasonal, O2.seasonal, fill= Abundance)) + 
  theme_classic() + 
  theme(axis.title = element_text(size = 14),
        axis.text=element_text(size=10),
        legend.title = element_text(size=12),
        legend.text = element_text(size=10)) +
  labs(fill="Biomass") + 
  xlab("") + ylab("Dissolved Oxygen (mg/L)") +
  geom_tile()+
  scale_fill_gradient(low="white", high="blue") +
  annotate("text", x = 3, y = 10, label = "(a)")

# annotate("text", x = 3, y = 3.5e-04, label = "(a)")

abundance_se_plot <- ggplot(abundance_se_predictions_df, 
                            aes(SBT.seasonal, O2.seasonal, fill= SE)) + 
  theme_classic() + 
  theme(axis.title = element_text(size = 14),
        axis.text=element_text(size=10),
        legend.title = element_text(size=12),
        legend.text = element_text(size=10)) +
  labs(fill="SE") + 
  xlab("") + ylab("") +
  geom_tile()+
  scale_fill_gradient(low="white", high="red") +
  annotate("text", x = 3, y = 10, label = "(b)")


presence_plot <- ggplot(presence_predictions_df, 
                        aes(SBT.seasonal, O2.seasonal, fill= Presence)) + 
  theme_classic() + 
  theme(axis.title = element_text(size = 14),
        axis.text=element_text(size=10),
        legend.title = element_text(size=12),
        legend.text = element_text(size=10)) +
  labs(fill="Presence") + 
  xlab("Temperature (C)") + ylab("Dissolved Oxygen (mg/L)") +
  geom_tile() +
  scale_fill_gradient(low="white", high="blue") +
  annotate("text", x = 3, y = 10, label = "(c)")

presence_se_plot <- ggplot(presence_se_predictions_df, 
                           aes(SBT.seasonal, O2.seasonal, fill= Presence.SE)) + 
  theme_classic() + 
  theme(axis.title = element_text(size = 14),
        axis.text=element_text(size=10),
        legend.title = element_text(size=12),
        legend.text = element_text(size=10)) +
  labs(fill="SE") + 
  xlab("Temperature (C)") + ylab("") +
  geom_tile() +
  scale_fill_gradient(low="white", high="red") +
  annotate("text", x = 3, y = 10, label = "(d)")

png("roms_figures_output_ver_1/collective_plot_predictive_ver_3.png", width=6, height=6, units="in", res=300)
gridExtra::grid.arrange(abundance_plot, abundance_se_plot, presence_plot, presence_se_plot, nrow=2)
dev.off()
