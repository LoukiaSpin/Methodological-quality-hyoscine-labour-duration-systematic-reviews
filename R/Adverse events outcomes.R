#*******************************************************************************
#*
#*
#*                    Distribution of Outcome characteristics                    
#*                    <Maternal and neonatal adverse events>                                                                                                      
#*       
#* Author: Loukia M. Spineli
#* Date: August 2025      
#*******************************************************************************      


## Load libraries ----
list.of.packages <- c("forstringr", "ggplot2", "dplyr", "ggpubr")
lapply(list.of.packages, require, character.only = TRUE); rm(list.of.packages)


## Load datasets ----
# Participant characteristics
load("./data/outcome.RData")


## Prepare datasets ----
# First author with publication year
outcome$`First author` <- paste(outcome$`First author`, outcome$Year)

# Maternal adverse events (AE)
maternal_ae <- outcome[, c(2, 19:31)]

# Neonatal adverse events (AE)
neonatal_ae <- outcome[, c(2, 33:39)]


## Turn *maternal adverse events * into long format ----
# Re-order the whole datasetd by year and alphabetically
maternal_ae <- maternal_ae[order(str_right(maternal_ae$`First author`, 4), maternal_ae$`First author`), ]

# Calculate the frequency of each characteristic
maternal_ae_new <- colSums(!is.na(maternal_ae))

# Sort columns by their frequency
maternal_ae <- maternal_ae[ , order(maternal_ae_new, decreasing = TRUE)]

# Data-frame
maternal_ae_long <- data.frame(review = rep(maternal_ae[, 1], length(colnames(maternal_ae)[-1])),
                               variable = rep(colnames(maternal_ae)[-1], each = dim(maternal_ae)[1]),
                               values = unname(unlist(c(maternal_ae[, -1]))))

# Shorten the name of some variables
maternal_ae_long_rec <- maternal_ae_long %>%
  mutate(variable  = recode(variable , 
                            "Postpartum hemorrhage rate" = "Postpartum\nhaemorrhage",
                            "Blood loss after delivery" = "Blood loss\nafter delivery",
                            "Blood transfusion" = "Blood\ntransfusion",
                            "Maternal tachycardia" = "Maternal\ntachycardia",
                            "Maternal mouth dryness" = "Mouth\ndryness",
                            "Maternal headache" = "Maternal\nheadache",
                            "Maternal nausea" = "Maternal\nnausea",
                            "Maternal vomiting" = "Maternal\nvomiting",
                            "Maternal dizziness" = "Maternal\ndizziness",
                            "Maternal giddiness" = "Maternal\ngiddiness",
                            "Maternal face flushing" = "Maternal\nface flushing",
                            "Urinary retention" = "Urinary\nretention",
                            "Cervical laceration" = "Cervical\nlaceration"),
         values_new = recode(values,
                             "primary" = "P",
                             "secondary" = "S",
                             "unspecified" = "U"))

# Include an indicator on whether a characteristics was reported or not
maternal_ae_long_rec$indicator <- ifelse(is.na(maternal_ae_long_rec$values_new), "No", "Yes")


## Turn *neonatal adverse events * into long format ----
# Re-order the whole datasetd by year and alphabetically
neonatal_ae <- neonatal_ae[order(str_right(neonatal_ae$`First author`, 4), neonatal_ae$`First author`), ]

# Calculate the frequency of each characteristic
neonatal_ae_new <- colSums(!is.na(neonatal_ae))

# Sort columns by their frequency
neonatal_ae <- neonatal_ae[ , order(neonatal_ae_new, decreasing = TRUE)]

# Data-frame
neonatal_ae_long <- data.frame(review = rep(neonatal_ae[, 1], length(colnames(neonatal_ae)[-1])),
                               variable = rep(colnames(neonatal_ae)[-1], each = dim(neonatal_ae)[1]),
                               values = unname(unlist(c(neonatal_ae[, -1]))))

# Shorten the name of some variables
neonatal_ae_long_rec <- neonatal_ae_long %>%
  mutate(values_new = recode(values,
                             "primary" = "P",
                             "secondary" = "S",
                             "unspecified" = "U"))

# Include an indicator on whether a characteristics was reported or not
neonatal_ae_long_rec$indicator <- ifelse(is.na(neonatal_ae_long_rec$values_new), "No", "Yes")


## Plot maternal adverse events ----
maternal_ae_plot <- 
  ggplot(maternal_ae_long_rec, 
         aes(x = variable, 
             y = factor(review, levels = unique(review)),
             fill = indicator)) +
  geom_point(aes(col = factor(values, levels = c("secondary", "unspecified"))),
             size = 6,
             shape = 21,
             stroke = 3,
             alpha = 0.6) +
  scale_fill_manual(breaks = c("Yes", "No"),
                    values = c("#009E73", "#D55E00")) +
  scale_colour_manual(breaks = c("secondary", "unspecified"),
                      values = c("grey45", "white"),
                      na.value = "white",
                      labels = c("Secondary", "Unspecified")) +
  #geom_text(aes(x = variable, 
  #              y = factor(review, levels = unique(review)),
  #              label = values),
  #          size = 4,
  #          vjust = -0.01,
  #          hjust = 0.5) +
  geom_label(data = data.frame(n = sort(maternal_ae_new[-1], decreasing = TRUE),
                               variable = unique(maternal_ae_long_rec$variable)),
             aes(x = variable, 
                 y = 1.1,
                 label = n),
             hjust = 0.5,
             vjust = 2.2, 
             colour = "black", 
             fontface = "bold",
             size = 3,
             inherit.aes = FALSE) +
  facet_grid(cols = vars(factor(variable, levels = unique(variable))), 
             scales = "free") +
  labs(x = "",
       y = "",
       fill = "Characteristic was reported",
       col = "Type of outcome") + 
  theme_classic() +
  ggtitle("Maternal adverse events") +
  guides(fill = guide_legend(override.aes = list(size = 6, stroke = 1.8, col = "white"), order = 1),
         colour = guide_legend(override.aes = list(size = 6, stroke = 1.8))) + 
  theme(plot.title = element_text(size = 15, face = "bold"),
        axis.title = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "bottom",
        legend.margin = margin(t = -10),
        legend.title = element_text(size = 15, face = "bold"),
        legend.text = element_text(size = 15),
        strip.text = element_text(size = 12.0, face = "bold"))


## Plot neonatal adverse events ----
neonatal_ae_plot <- 
  ggplot(neonatal_ae_long_rec, 
         aes(x = variable, 
             y = factor(review, levels = unique(review)),
             fill = indicator)) +
  geom_point(aes(col = factor(values, levels = c("secondary", "unspecified"))),
             size = 6,
             shape = 21,
             stroke = 3,
             alpha = 0.6) +
  scale_fill_manual(breaks = c("Yes", "No"),
                    values = c("#009E73", "#D55E00")) +
  scale_colour_manual(breaks = c("secondary", "unspecified"),
                      values = c("grey45", "white"),
                      na.value = "white",
                      labels = c("Secondary", "Unspecified")) +
  #geom_text(aes(x = variable, 
  #              y = factor(review, levels = unique(review)),
  #              label = values),
  #          size = 4,
  #          vjust = -0.01,
  #          hjust = 0.5) +
  geom_label(data = data.frame(n = sort(neonatal_ae_new[-1], decreasing = TRUE),
                               variable = unique(neonatal_ae_long_rec$variable)),
             aes(x = variable, 
                 y = 1.1,
                 label = n),
             hjust = 0.5,
             vjust = 2.2, 
             colour = "black", 
             fontface = "bold",
             size = 3,
             inherit.aes = FALSE) +
  facet_grid(cols = vars(factor(variable, levels = unique(variable))), 
             scales = "free") +
  labs(x = "",
       y = "",
       fill = "Characteristic was reported") + 
  theme_classic() +
  ggtitle("Neonatal adverse events") +
  guides(fill = guide_legend(override.aes = list(size = 6, stroke = 1.8, col = "white"), order = 1),
         colour = guide_legend(override.aes = list(size = 6, stroke = 1.8))) + 
  theme(plot.title = element_text(size = 15, face = "bold"),
        axis.title = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "bottom",
        legend.margin = margin(t = -10),
        legend.title = element_text(size = 15, face = "bold"),
        legend.text = element_text(size = 15),
        strip.text = element_text(size = 12.0, face = "bold"))


## Bring together ----
tiff("./Figures/Supplementary Figure 4.tiff", 
     height = 28, 
     width = 45, 
     units = "cm", 
     compression = "lzw", 
     res = 300)
ggarrange(maternal_ae_plot, neonatal_ae_plot,
          nrow = 2,
          labels = c("a)", "b)"),
          common.legend = TRUE,
          legend = "bottom")
dev.off()
