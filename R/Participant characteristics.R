#*******************************************************************************
#*
#*
#*                  Distribution of Participant characteristics                                               
#*       
#* Author: Loukia M. Spineli
#* Date: August 2025      
#*******************************************************************************      


## Load libraries ----
list.of.packages <- c("forstringr", "ggplot2", "dplyr", "ggpubr")
lapply(list.of.packages, require, character.only = TRUE); rm(list.of.packages)


## Load datasets ----
# Participant characteristics
load("./data/participant.RData")


## Prepare datasets ----
# First author with publication year
participant$`First author` <- paste(participant$`First author`, participant$Year)

# Inclusion criteria
inclusion_criteria <- participant[, c(2, 7:17)]

# Exclusion criteria
exclusion_criteria <- participant[, c(2, 18:26)]


## Turn *inclusion criteria* into long format ----
# Re-order the whole datasetd by year and alphabetically
inclusion_criteria <- inclusion_criteria[order(str_right(inclusion_criteria$`First author`, 4), inclusion_criteria$`First author`), ]

# Calculate the frequency of each characteristic
inclusion_new <- colSums(!is.na(inclusion_criteria))

# Sort columns by their frequency
inclusion_criteria <- inclusion_criteria[ , order(inclusion_new, decreasing = TRUE)]

# Data-frame
inclusion_long <- data.frame(review = rep(inclusion_criteria[, 1], length(colnames(inclusion_criteria)[-1])),
                             variable = rep(colnames(inclusion_criteria)[-1], each = dim(inclusion_criteria)[1]),
                             values = unname(unlist(c(inclusion_criteria[, -1]))))

# Add whether the information was found in Methods (methods = 1)
inclusion_long$methods <- ifelse(grepl("*", inclusion_long$values, fixed = TRUE), 0, 1) 

# Remove asterix at the end
inclusion_long$values_new <- gsub("*", "", inclusion_long$values, fixed = TRUE)

# Shorten the name of some variables
inclusion_long_rec <- inclusion_long %>%
  mutate(variable  = recode(variable , 
                            "Pregnancy type" = "Singleton\npregnancy",
                            "Gestation delivery" = "Gestational\nage",
                            "Fetal positioning" = "Vertex fetal\npositioning",
                            "Membrane status" = "Membranes\nintact",
                            "Labour onset" = "Labour\nonset",
                            "Labour induction" = "Labour\ninduction",
                            "Labour phase" = "Labour\nphase",
                            "Uterine contractions" = "Uterine\ncontractions",
                            "Cervical dilatation" = "Cervical\ndilatation",
                            "Labour management" = "Labour\nmanagement",
                            "Pregnancy risk" = "Pregnancy\nrisk"),
         values_new = recode(values_new,
                             "nullipara & multipara" = "nullipara &\nmultipara",
                             "spontaneous or induced" = "spontaneous\nor induced",
                             "regular" = "",
                             "singleton" = "",
                             "intact" = "",
                             "yes" = "",
                             "vertex" = "",
                             "active & expectant" = "active &\nexpectant",
                             "low- & high-risk" = "low- &\nhigh-risk"))

# Include an indicator on whether a characteristics was reported or not
inclusion_long_rec$indicator <- ifelse(is.na(inclusion_long_rec$values_new), "No", "Yes")


## Turn *exclusion criteria* into long format ----
# Re-order the whole datasetd by year and alphabetically
exclusion_criteria <- exclusion_criteria[order(str_right(exclusion_criteria$`First author`, 4), exclusion_criteria$`First author`), ]

# Calculate the frequency of each characteristics
exclusion_new <- colSums(!is.na(exclusion_criteria))

# Sort columns by their frequency
exclusion_criteria <- exclusion_criteria[ , order(exclusion_new, decreasing = TRUE)]

# Data-frame
exclusion_long <- data.frame(review = rep(exclusion_criteria[, 1], length(colnames(exclusion_criteria)[-1])),
                             variable = rep(colnames(exclusion_criteria)[-1], each = dim(exclusion_criteria)[1]),
                             values = unname(unlist(c(exclusion_criteria[, -1]))))

# Add whether the information was found in Methods (methods = 1)
exclusion_long$methods <- ifelse(grepl("*", exclusion_long$values, fixed = TRUE), 0, 1) 

# Remove asterix at the end
exclusion_long$values_new <- gsub("*", "", exclusion_long$values, fixed = TRUE)

# Shorten the name of some variables
exclusion_long_rec <- exclusion_long %>%
  mutate(variable  = recode(variable , 
                           "Previous uterine surgery" = "Previous uterine\nsurgery",
                           "Vaginal delivery contraindications" = "Vaginal delivery\ncontraindications",
                           "Previous caesarian delivery" = "Previous caesarian\ndelivery",
                           "Pregnancy-induced illness" = "Pregnancy-induced\nillness",
                           "Other placenta issues" = "Other placental\nconditions"))

# Include an indicator on whether a characteristics was reported or not
exclusion_long_rec$indicator <- ifelse(is.na(exclusion_long_rec$values_new), "No", "Yes")

# Plot inclusion criteria
inclusion <- 
  ggplot(inclusion_long_rec, 
         aes(x = variable, 
             y = factor(review, levels = unique(review)),
             fill = indicator)) +
  geom_point(aes(col = factor(methods, levels = c(1, 0))),
             size = 6,
             shape = 21,
             stroke = 3,
             alpha = 0.6) +
  scale_fill_manual(breaks = c("Yes", "No"),
                    values = c("#009E73", "#D55E00")) +
  scale_colour_manual(breaks = c("0", "1"),
                      values = c("grey45", "white"),
                      labels = c("Elsewhere", "Methods section")) +
  geom_text(aes(x = variable, 
                y = factor(review, levels = unique(review)),
                label = values_new),
            size = 4,
            fontface = "bold",
            vjust = -0.08,
            hjust = 0.5,
            lineheight = 0.7) +
  geom_label(data = data.frame(n = sort(inclusion_new[-1], decreasing = TRUE),
                               variable = unique(inclusion_long_rec$variable)),
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
       col = "Information found in") + 
  theme_classic() +
  ggtitle("Inclusion criteria") +
  guides(fill = guide_legend(override.aes = list(size = 6, stroke = 1.8, col = "white"), order = 1),
         colour = guide_legend(override.aes = list(size = 6, stroke = 1.8))) + 
  theme(plot.title = element_text(size = 15, face = "bold"),
        axis.text.y = element_text(size = 15),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "bottom",
        legend.margin = margin(t = -10),
        legend.title = element_text(size = 15, face = "bold"),
        legend.text = element_text(size = 15),
        strip.text = element_text(size = 12.0, face = "bold"))

# Plot exclusion criteria
exclusion <-
  ggplot(exclusion_long_rec, 
         aes(x = variable, 
             y = factor(review, levels = unique(review)),
             fill = indicator)) +
  geom_point(size = 6,
             shape = 21,
             col = "white",
             alpha = 0.6) +
  scale_fill_manual(breaks = c("Yes", "No"),
                    values = c("#009E73", "#D55E00")) +
  geom_label(data = data.frame(n = sort(exclusion_new[-1], decreasing = TRUE),
                               variable = unique(exclusion_long_rec$variable)),
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
  ggtitle("Exclusion criteria") +
  theme(plot.title = element_text(size = 15, face = "bold"),
        axis.text.y = element_text(size = 15),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "bottom",
        legend.margin = margin(t = -10),
        legend.title = element_text(size = 15, face = "bold"),
        legend.text = element_text(size = 15),
        strip.text = element_text(size = 12.0, face = "bold"))


## Bring together ----
tiff("./Figures/Figure 3.tiff", 
     height = 28, 
     width = 45, 
     units = "cm", 
     compression = "lzw", 
     res = 300)
ggarrange(inclusion, exclusion,
          nrow = 2,
          labels = c("a)", "b)"),
          common.legend = TRUE,
          legend = "bottom")
dev.off()
