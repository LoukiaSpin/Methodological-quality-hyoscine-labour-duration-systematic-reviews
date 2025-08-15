#*******************************************************************************
#*
#*
#*           Distribution of Intervention-Comparator characteristics                                                                         
#*       
#* Author: Loukia M. Spineli
#* Date: August 2025      
#*******************************************************************************      


## Load libraries ----
list.of.packages <- c("forstringr", "ggplot2", "dplyr", "ggpubr")
lapply(list.of.packages, require, character.only = TRUE); rm(list.of.packages)


## Load datasets ----
# Intervention (HBB) characteristics
load("./data/intervention.RData")

# Comparator characteristics
load("./data/comparator.RData")


## Prepare datasets ----
# First author with publication year - Intervention dataset
intervention$`First author` <- paste(intervention$`First author`, intervention$Year)

# First author with publication year - Comparator dataset
comparator$`First author` <- paste(comparator$`First author`, comparator$Year)

# Intervention with only the characteristics
intervention_short <- intervention[, c(2, 7:11)]

# Comparator with only the characteristics
comparator_short <- comparator[, c(2, 7, 9, 10, 13:15)]


## Turn *intervention* into long format ----
# Re-order the whole datasetd by year and alphabetically
intervention_short <- intervention_short[order(str_right(intervention_short$`First author`, 4), intervention_short$`First author`), ]

# Calculate the frequency of each characteristic
intervention_new <- colSums(!is.na(intervention_short))

# Sort columns by their frequency
intervention_short <- intervention_short[ , order(intervention_new, decreasing = TRUE)]

# Data-frame
intervention_long <- data.frame(review = rep(intervention_short[, 1], length(colnames(intervention_short)[-1])),
                                variable = rep(colnames(intervention_short)[-1], each = dim(intervention_short)[1]),
                                values = unname(unlist(c(intervention_short[, -1]))))

# Add whether the information was found in Methods (methods = 1)
intervention_long$methods <- ifelse(grepl("*", intervention_long$values, fixed = TRUE), 0, 1) 

# Remove asterix at the end
intervention_long$values_new <- gsub("*", "", intervention_long$values, fixed = TRUE)

# Include an indicator on whether a characteristics was reported or not
intervention_long$indicator <- ifelse(is.na(intervention_long$values_new), "No", "Yes")

# Shorten the name of some variables
intervention_long <- intervention_long %>%
  mutate(variable  = recode(variable , 
                            "Administration route" = "Administration\nroute",
                            "HBB allergy" = "No HBB allergy"),
         values_new = recode(values_new,
                             "no" = " "))


## Turn *comparator* into long format ----
# Re-order the whole datasetd by year and alphabetically
comparator_short <- comparator_short[order(str_right(comparator_short$`First author`, 4), comparator_short$`First author`), ]

# Calculate the frequency of each characteristic
comparator_new <- colSums(!is.na(comparator_short))

# Sort columns by their frequency
comparator_short <- comparator_short[ , order(comparator_new, decreasing = TRUE)]

# Data-frame
comparator_long <- data.frame(review = rep(comparator_short[, 1], length(colnames(comparator_short)[-1])),
                              variable = rep(colnames(comparator_short)[-1], each = dim(comparator_short)[1]),
                              values = unname(unlist(c(comparator_short[, -1]))))

# Add whether the information was found in Methods (methods = 1)
comparator_long$methods <- ifelse(grepl("*", comparator_long$values, fixed = TRUE), 0, 1) 

# Remove asterix at the end
comparator_long$values_new <- gsub("*", "", comparator_long$values, fixed = TRUE)

# Add a new *variable* column
comparator_long$variable2 <- 
  ifelse(is.element(comparator_long$variable, c("Placebo", "No treatment", "Other drug")), "Comparator arm", comparator_long$variable)

# Add a new *values_new* column
comparator_long$values_new2 <- 
  ifelse(comparator_long$variable == "Placebo" & comparator_long$values_new == "yes", "placebo",
       ifelse(comparator_long$variable == "No treatment" & comparator_long$values_new == "yes", "no treatment",
              ifelse(comparator_long$variable == "Other drug" & comparator_long$values_new == "yes", "other drug", comparator_long$values_new)))

# Include an indicator on whether a characteristics was reported or not
comparator_long$indicator <- ifelse(is.na(comparator_long$values_new), "No", "Yes")

# Shorten the name of some variables
comparator_long <- comparator_long %>%
  mutate(variable  = recode(variable , 
                            "Administration route" = "Administration\nroute"),
         values_new = recode(values_new,
                             "yes" = " "))


## Plot the intervention characteristics per review ----
intervention_plot <-
  ggplot(intervention_long, 
         aes(x = variable, 
             y = factor(review, levels = unique(review)),
             fill = indicator)) +
  geom_point(aes(col = factor(methods, levels = c(1, 0))),
             size = 7,
             shape = 21,
             stroke = 3.5,
             alpha = 0.6) +
  scale_fill_manual(breaks = c("Yes", "No"),
                    values = c("#009E73", "#D55E00")) +
  scale_colour_manual(breaks = c("0", "1"),
                      values = c("grey45", "white"),
                      labels = c("Elsewhere", "Methods section")) +
  geom_text(aes(x = variable, 
                y = factor(review, levels = unique(review)),
                label = values_new),
            size = 4.5,    
            fontface = "bold",
            vjust = -0.01,
            hjust = "center") +
  geom_label(data = data.frame(n = sort(intervention_new[-1], decreasing = TRUE),
                               variable = unique(intervention_long$variable)),
             aes(x = variable, 
                 y = 1.1,
                 label = n),
             hjust = 0.5,
             vjust = 3.0, 
             colour = "black", 
             fontface = "bold",
             size = 4,
             inherit.aes = FALSE) +
  facet_grid(cols = vars(factor(variable, levels = unique(variable))), 
             scales = "free") +
  labs(x = "",
       y = "",
       fill = "Characteristic was reported",
       col = "Information found in") + 
  theme_classic() +
  ggtitle("Intervention characteristics") +
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
        strip.text = element_text(size = 13.0, face = "bold"))


## Plot the comparator characteristics per review ----
comparator_plot <-
  ggplot(comparator_long, 
         aes(x = variable, 
             y = factor(review, levels = unique(review)),
             fill = indicator)) +
  geom_point(aes(col = factor(methods, levels = c(1, 0))),
             size = 7,
             shape = 21,
             stroke = 3.5,
             alpha = 0.6) +
  scale_fill_manual(breaks = c("Yes", "No"),
                    values = c("#009E73", "#D55E00")) +
  scale_colour_manual(breaks = c("0", "1"),
                      values = c("grey45", "white"),
                      labels = c("Elsewhere", "Methods section")) +
  geom_text(aes(x = variable, 
                y = factor(review, levels = unique(review)),
                label = values_new),
            size = 4.5,
            fontface = "bold",
            vjust = -0.01,
            hjust = "center") +
  geom_label(data = data.frame(n = sort(comparator_new[-1], decreasing = TRUE),
                               variable = unique(comparator_long$variable)),
             aes(x = variable, 
                 y = 1.1,
                 label = n),
             hjust = 0.5,
             vjust = 3.0,
             colour = "black",
             fontface = "bold",
             size = 4,
             inherit.aes = FALSE) +
  facet_grid(cols = vars(factor(variable, levels = unique(variable))), 
             scales = "free") +
  labs(x = "",
       y = "",
       fill = "Characteristic was reported",
       col = "Information found in") + 
  theme_classic() +
  ggtitle("Comparator characteristics") +
  theme(plot.title = element_text(size = 15, face = "bold"),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "bottom",
        legend.margin = margin(t = -10),
        legend.title = element_text(size = 15, face = "bold"),
        legend.text = element_text(size = 15),
        strip.text = element_text(size = 13.0, face = "bold"))


## Bring both in one Figure ----
tiff("./Figures/Supplementary Figure 3.tiff", 
     height = 25, 
     width = 50, 
     units = "cm", 
     compression = "lzw", 
     res = 300)
ggarrange(intervention_plot, comparator_plot,
          ncol = 2,
          labels = c("a)", "b)"),
          common.legend = TRUE,
          legend = "bottom")
dev.off()
