#*******************************************************************************
#*
#*
#*                   Distribution of Outcome characteristics                   
#*                     <Labour duration & characteristics>                                          
#*       
#* Author: Loukia M. Spineli
#* Date: April 2025      
#*******************************************************************************      


## Load libraries ----
list.of.packages <- c("forstringr", "ggplot2", "dplyr", "ggpubr")
lapply(list.of.packages, require, character.only = TRUE); rm(list.of.packages)


## Load datasets ----
# Outcomes
load("./data/outcome.RData")


## Prepare datasets ----
# First author with publication year
outcome$`First author` <- paste(outcome$`First author`, outcome$Year)

# Labour duration
labour_duration <- outcome[, c(2, 7:11)]

# Labour characteristics
labour_char <- outcome[, c(2, 12:17, 32)]


## Turn *labour duration* into long format ----
# Re-order the whole datasetd by year and alphabetically
labour_duration <- labour_duration[order(str_right(labour_duration$`First author`, 4), labour_duration$`First author`), ]

# Calculate the frequency of each characteristic
labour_duration_new <- colSums(!is.na(labour_duration))

# Sort columns by their frequency
labour_duration <- labour_duration[ , order(labour_duration_new, decreasing = TRUE)]

# Data-frame
labour_duration_long <- data.frame(review = rep(labour_duration[, 1], length(colnames(labour_duration)[-1])),
                                   variable = rep(colnames(labour_duration)[-1], each = dim(labour_duration)[1]),
                                   values = unname(unlist(c(labour_duration[, -1]))))

# Add whether the information was found in Methods (methods = 1)
labour_duration_long$methods <- ifelse(grepl("*", labour_duration_long$values, fixed = TRUE), 0, 1) 

# Remove asterix at the end
labour_duration_long$values_new <- gsub("*", "", labour_duration_long$values, fixed = TRUE)

# Shorten the name of some variables
labour_duration_long_rec <- labour_duration_long %>%
  mutate(variable  = recode(variable , 
                            "First stage duration" = "First stage\nduration",
                            "Active phase duration" = "Active phase\nduration",
                            "Second stage duration" = "Second stage\nduration",
                            "Third stage duration" = "Third stage\nduration"),
         values_new2 = recode(values_new,
                              "primary" = "P",
                              "secondary" = "S",
                              "unspecified" = "U"))

# Include an indicator on whether a characteristics was reported or not
labour_duration_long_rec$indicator <- ifelse(is.na(labour_duration_long_rec$values_new), "No", "Yes")
  

## Turn *labour characteristics* into long format ----
# Re-order the whole datasetd by year and alphabetically
labour_char <- labour_char[order(str_right(labour_char$`First author`, 4), labour_char$`First author`), ]

# Calculate the frequency of each characteristic
labour_char_new <- colSums(!is.na(labour_char))

# Sort columns by their frequency
labour_char <- labour_char[ , order(labour_char_new, decreasing = TRUE)]

# Data-frame
labour_char_long <- data.frame(review = rep(labour_char[, 1], length(colnames(labour_char)[-1])),
                               variable = rep(colnames(labour_char)[-1], each = dim(labour_char)[1]),
                               values = unname(unlist(c(labour_char[, -1]))))

# Add whether the information was found in Methods (methods = 1)
labour_char_long$methods <- ifelse(grepl("*", labour_char_long$values, fixed = TRUE), 0, 1) 

# Shorten the name of some variables
labour_char_long_rec <- labour_char_long %>%
  mutate(variable  = recode(variable, 
                           "Cervical dilation rate" = "Cervical\ndilation rate",
                           "Oxytocin augmentation" = "Oxytocin\naugmentation",
                           "Analgesia need" = "Analgesia\nneed",
                           "Vertex delivery rate" = "Vertex\ndelivery rate",
                           "Instrumental labour" = "Instrumental\nlabour",
                           "Caesarean section rate" = "Caesarean\nsection rate",
                           "Maternal satisfaction" = "Maternal\nsatisfaction"),
         values2 = recode(values,
                          "primary" = "P",
                          "secondary" = "S",
                          "unspecified" = "U"))

# Include an indicator on whether a characteristics was reported or not
labour_char_long_rec$indicator <- ifelse(is.na(labour_char_long_rec$values), "No", "Yes")


## Plot labour duration ----
labour_duration <- 
  ggplot(labour_duration_long_rec, 
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
            size = 4.0,
            fontface = "bold",
            vjust = -0.01,
            hjust = 0.5) +
  geom_label(data = data.frame(n = sort(labour_duration_new[-1], decreasing = TRUE),
                               variable = unique(labour_duration_long_rec$variable)),
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
  ggtitle("Labour duration outcomes") +
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


## Plot labour characteristics ----
labour_char <- 
  ggplot(labour_char_long_rec, 
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
                label = values),
            size = 4.0,
            fontface = "bold",
            vjust = -0.01,
            hjust = 0.5) +
  geom_label(data = data.frame(n = sort(labour_char_new[-1], decreasing = TRUE),
                               variable = unique(labour_char_long_rec$variable)),
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
  ggtitle("Other labour-related characteristics") +
  theme(plot.title = element_text(size = 15, face = "bold"),
        axis.title = element_text(size = 15),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "bottom",
        legend.margin = margin(t = -10),
        legend.title = element_text(size = 15, face = "bold"),
        legend.text = element_text(size = 15),
        strip.text = element_text(size = 12.0, face = "bold"))


## Bring both in one Figure ----
tiff("./Figures/Figure 4.tiff", 
     height = 25, 
     width = 50, 
     units = "cm", 
     compression = "lzw", 
     res = 300)
ggarrange(labour_duration, labour_char,
          ncol = 2,
          labels = c("a)", "b)"),
          common.legend = TRUE,
          legend = "bottom")
dev.off()
