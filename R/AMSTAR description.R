#*******************************************************************************
#*
#*
#*                      Distribution of AMSTAR & citations                                                                                      
#*       
#* Author: Loukia M. Spineli
#* Date: April 2025      
#*******************************************************************************      


## Load libraries ----
list.of.packages <- c("forstringr", "ggplot2", "dplyr")
lapply(list.of.packages, require, character.only = TRUE); rm(list.of.packages)


## Load datasets ----
# Databases search
load("./data/databases.RData")

# AMSTAR results
load("./data/AMSTAR_results.RData")


## Main Figure: Bar plots ----
# First author with publication year
databases$`First author` <- paste(databases$`First author`, databases$Year)
AMSTAR_results$`First author` <- paste(AMSTAR_results$`First author`, AMSTAR_results$Year)

# Keep the columns starting with 'Item'
AMSTAR_results_new <- AMSTAR_results[, startsWith(colnames(AMSTAR_results), "Domain") | startsWith(colnames(AMSTAR_results), "First author")]

# Add 'No. citations' while merging the datasets correctly
dataset <- merge(AMSTAR_results_new, databases[, c(2, 5)], by = "First author")

# Add year
dataset$year <- str_right(dataset$`First author`, 4)


## Turn AMSTAR results into long format ----
# Re-order the whole datasetd by year and alphabetically
dataset <- dataset[order(str_right(dataset$`First author`, 4), dataset$`First author`), ]


## Bar plot of AMSTAR overall score and citation numbers per review
# Critical domains
critical_domains <- paste("Domain", c(2, 4, 7, 9, 11, 13, 15)) 
no_critical_domains <- paste("Domain", c(1:16)[!is.element(1:16, c(2, 4, 7, 9, 11, 13, 15))])

# Count number of critical domains with 'no' per review
dataset$count_critical <- rowSums(dataset[critical_domains] == "no")

# Count number of non-critical domains with 'no' per review
dataset$count_non_critical <- rowSums(dataset[no_critical_domains] == "no")

# Get the overall score
dataset_new <- transform(dataset, 
                         overall = ifelse(count_critical == 0 & count_non_critical == 0, "High", 
                                          ifelse(count_critical == 0 & count_non_critical > 0, "Moderate", 
                                                 ifelse(count_critical == 1, "Low", "Critically low"))))
colnames(dataset_new)[2:17] <- paste("Domain", 1:16)

# Create bar plot
tiff("./Figures/Figure 1.tiff", 
     height = 25, 
     width = 50, 
     units = "cm", 
     compression = "lzw", 
     res = 300)
ggplot(dataset_new,
       aes(x = factor(`First author`, levels = unique(`First author`[order(str_right(`First author`, 4), `First author`, decreasing = TRUE)])),  #reorder(`First author`, desc(year)), 
           y = `No. Citations`,
           fill = overall)) +
  geom_bar(stat = "identity",
           show.legend = TRUE) +
  geom_text(aes(x = `First author`, 
                y = `No. Citations`,
                label = `No. Citations`), 
            vjust = -0.15,
            size = 4.5, 
            color = "black",
            fontface = "bold") +
  scale_fill_manual(name = "AMSTAR overall confidence",
                    breaks = c("High", "Moderate", "Low", "Critically low"),
                    #labels = c("High", "Moderate", "Low", "Critically low"),
                    values = c("#009E73", "#FFC20A", "#D55E00", "#DC3220"),
                    limits = c("High", "Moderate", "Low", "Critically low"),
                    drop = FALSE) +
  labs(x = "",
       y = "Number of citations") + 
  ggtitle("Number of citations and AMSTAR overall confidence") +
  theme_classic() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 16, face = "bold"),
        axis.text = element_text(size = 16),
        legend.position = "bottom",
        legend.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 16))
dev.off()


## Turn AMSTAR results into long format ----
# Data-frame
dataset_long <- data.frame(review = rep(dataset_new[, 1], length(colnames(dataset_new)[-c(1, 18)])),
                           variable = rep(colnames(dataset_new)[-c(1, 18)], each = dim(dataset_new)[1]),
                           values = unname(unlist(c(dataset_new[, -c(1, 18)]))))[1:128, ]

# Rename and shorten the name of some variables
#dataset_long_rec <- dataset_long %>%
#  mutate(variable  = recode(variable , 
#                            "Domain 1" = "Research question\n& PICO",
#                            "Domain 2" = "Protocol &\ndeviations",
#                            "Domain 3" = "Study design\nselection",
#                            "Domain 4" = "Search strategy",
#                            "Domain 5" = "Study selection",
#                            "Domain 6" = "Data extraction",
#                            "Domain 7" = "Excluded studies\nlist",
#                            "Domain 8" = "Included studies\ndescription",
#                            "Domain 9" = "RoB assessment",
#                            "Domain 10" = "Study-level funding",
#                            "Domain 11" = "Proper statistical\nanalysis",
#                            "Domain 12" = "RoB impact\nanalysis",
#                            "Domain 13" = "RoB impact\ndiscussion",
#                            "Domain 14" = "Heterogeneity\nimplications",
#                            "Domain 15" = "Publication bias\nassessment",
#                            "Domain 16" = "Study-level funding"))

## Supplementary material: Tabulate reviews with AMSTAR score ----
# Indicate the critical domains
dataset_long$critical <- ifelse(is.element(dataset_long$variable, critical_domains), "yes", "no")

# Plot AMSTAR results per review and item
tiff("./Figures/Supplementary Figure 1.tiff", 
     height = 25, 
     width = 50, 
     units = "cm", 
     compression = "lzw", 
     res = 300)
ggplot(dataset_long[1:128, ], 
       aes(x = variable,
           y = factor(review, levels = unique(review)))) +
  geom_point(aes(colour = values,
                 fill = critical),
             size = 9,
             shape = 21,
             stroke = 4) +
  scale_colour_manual(name = "Domain is flawless",
                      breaks = c("yes", "partial yes", "no"),
                      values = c("#009E73", "#FFC20A", "#D55E00"),
                      labels = c("yes" = "Yes", "partial yes" = "Partially yes", "no" = "No")) +
  scale_fill_manual(name = "Critical domain",
                    breaks = c("yes", "no"),
                    values = c("black", "white"), ##0099FF
                    labels = c("yes" = "Yes", "no" = "No")) +
  facet_grid(cols = vars(factor(variable, levels = unique(variable))), 
             scales = "free") +
  guides(fill = guide_legend(override.aes = list(size = 4, stroke = 1.8)),
         colour = guide_legend(override.aes = list(size = 4, stroke = 1.8), order = 1)) + 
  labs(x = "",
       y = "",
       fill = "",
       colour = "") + 
  theme_classic() +
  ggtitle("AMSTAR assessment") +
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.text.y = element_text(size = 16),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "bottom",
        legend.margin = margin(t = -10),
        legend.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 16),
        strip.text = element_text(size = 14.0, face = "bold"))
dev.off()

