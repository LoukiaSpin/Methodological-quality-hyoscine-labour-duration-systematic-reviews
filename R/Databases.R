#*******************************************************************************
#*
#*
#*                           Distribution of Databases                                                                                                
#*       
#* Author: Loukia M. Spineli
#* Date: August 2025      
#*******************************************************************************      


## Load libraries ----
list.of.packages <- c("forstringr", "ggplot2", "dplyr")
lapply(list.of.packages, require, character.only = TRUE); rm(list.of.packages)


## Load datasets ----
# Databases
load("./data/databases.RData")


## Main Figure: Bar plots ----
# First author with publication year
databases$`First author` <- paste(databases$`First author`, databases$Year)

# Keep the necessary columns
databases_short <- databases[, c(2, 10:22)]


## Turn *databases* into long format ----
# Re-order the whole dataset by year and alphabetically
databases_short <- databases_short[order(str_right(databases_short$`First author`, 4), databases_short$`First author`), ]

# Calculate the frequency of each characteristics
databases_new <- data.frame(review = databases_short[, 1],
                            n = rowSums(!is.na(databases_short[, -1])))

# Data-frame
databases_long <- data.frame(review = rep(databases_short[, 1], length(colnames(databases_short)[-1])),
                             values = unname(unlist(c(databases_short[, -1]))))

databases_long <- databases_long %>%
  mutate(values  = recode(values , "Cochrane Library" = "CENTRAL"))

# Set of grey literature sources
grey_sources <- c("Reference list", "Open Grey", "ProQuest", "Google Scholar", "ClinicalTrials.gov", "WHO", "USDD", "IranDoc", 
                  "NDLTD Taiwan", "NY Academy of Medicine", "PROSPERO", "Pharmaceutical companies", "Conference proceedings", "Field experts")

# Set of sources that include also grey literature
also_grey_sources <- c("Virtual Health Library", "Airiti Library", "CNKI", "SID", "CENTRAL")

# Include an indicator on whether a database is grey literature source or not
databases_long$grey <- ifelse(is.element(databases_long$values, c(grey_sources, also_grey_sources)), "Yes",  "No") 

# Data-frame with unique databases
databases_long_new <- data.frame(table(na.omit(databases_long$values)))
colnames(databases_long_new) <- c("values", "n")

# Sort databases in decreasing order of their frequency
databases_long_new[order(databases_long_new$n, decreasing = TRUE), ]


## Plot databases per review ----
tiff("./Figures/Supplementary Figure 5.tiff", 
     height = 28, 
     width = 45, 
     units = "cm", 
     compression = "lzw", 
     res = 300)
ggplot(na.omit(databases_long), 
       aes(x = factor(review, levels = rev(unique(review))), 
           y = factor(values, levels = levels(databases_long_new$values)[order(databases_long_new$n, decreasing = FALSE)]),
           fill = grey)) +
  geom_point(size = 6,
             shape = 21,
             col = "white",
             alpha = 0.7) +
  scale_fill_manual(breaks = c("Yes", "No"),
                    values = c("#009E73", "#D55E00")) +
  geom_label(data = databases_new,
             aes(x = review, 
                 y = 1.8,
                 label = n),
             hjust = 0.5,
             vjust = 3.2,
             colour = "black",
             fontface = "bold",
             size = 3,
             inherit.aes = FALSE) +
  facet_grid(cols = vars(factor(review, levels = rev(unique(review)))), 
             scales = "free") +
  labs(x = "",
       y = "",
       fill = "Considers grey literature") + 
  expand_limits(y = c(0, 13)) +
  theme_classic() +
  ggtitle("Databases searched") +
  theme(plot.title = element_text(size = 15, face = "bold"),
        axis.text.y = element_text(size = 15),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "bottom",
        legend.margin = margin(t = -10),
        legend.title = element_text(size = 15, face = "bold"),
        legend.text = element_text(size = 15),
        strip.text = element_text(size = 13.0, face = "bold"))
dev.off()
