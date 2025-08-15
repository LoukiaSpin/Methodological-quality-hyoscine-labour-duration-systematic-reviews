#*******************************************************************************
#*
#*
#*                           Coverage of PICO features                                                                                                                                                                                        
#*       
#* Author: Loukia M. Spineli
#* Date: August 2025      
#*******************************************************************************      


## Load libraries ----
list.of.packages <- c("forstringr", "ggplot2", "reshape2", "dplyr", "ggpubr")
lapply(list.of.packages, require, character.only = TRUE); rm(list.of.packages)


## Load datasets ----
# Participant characteristics
load("./data/participant.RData")

# Intervention (HBB) characteristics
load("./data/intervention.RData")

# Comparator characteristics
load("./data/comparator.RData")

# Outcomes (labour related and adverse events)
load("./data/outcome.RData")


## Prepare datasets ----
# Participant dataset with only the characteristics
participant_new <- participant[, c(2:3, 7:26)]

# Intervention dataset with only the characteristics
intervention_new <- Filter(function(x)!all(is.na(x)), intervention[, c(2:3, 7:11)])

# Comparator dataset with only the characteristics
comparator_new <- Filter(function(x)!all(is.na(x)), comparator[, c(2:3, 7, 9, 10, 13:15)])

# Labour duration
labour_duration <- outcome[, c(2:3, 7:11)]

# Labour characteristics
labour_char <- outcome[, c(2:3, 12:17, 32)]

# Maternal adverse events (AE)
maternal_ae <- outcome[, c(2:3, 19:31)]

# Neonatal adverse events (AE)
neonatal_ae <- outcome[, c(2:3, 33:39)]


## Total number of characteristics per PICO (sub)feature ----
# Total *participant* characteristics
total_partic <- dim(participant_new[, -c(1, 2)])[2]

# Total *intervention* characteristics
total_interv <- dim(intervention_new[, -c(1, 2)])[2]

# Total *comparator* characteristics
total_compar <- dim(comparator_new[, -c(1, 2)])[2] - 2 # Because 'placebo', 'no treatment' and 'other drug' are the comparator type (1 characteristic with three values)

# Total *labour outcomes* characteristics
total_labour_out <- dim(labour_duration[, -c(1, 2)])[2]

# Total *labour related* characteristics
total_labour_char <- dim(labour_char[, -c(1, 2)])[2]

# Total *maternal adverse events* characteristics
total_maternal_ae <- dim(maternal_ae[, -c(1, 2)])[2]

# Total *neonatal adverse events* characteristics
total_neonatal_ae <- dim(neonatal_ae[, -c(1, 2)])[2]

# Total characteristics *overall*
total_char <- total_partic + total_interv + total_compar + total_labour_out + total_labour_char + total_maternal_ae + total_neonatal_ae


## Percentage coverage of PICO features ----
# Participant coverage
participant_cov <- round((total_partic / total_char) * 100, 1)

# Intervention coverage
intervention_cov <- round((total_interv / total_char) * 100, 1)

# Comparator coverage
comparator_cov <- round((total_compar / total_char) * 100, 1)

# Labour outcomes
labour_out_cov <- round((total_labour_out / total_char) * 100, 1)

# Labour related characteristics
labour_char_cov <- round((total_labour_char / total_char) * 100, 1)

# Maternal adverse events characteristics
maternal_ae_cov <- round((total_maternal_ae / total_char) * 100, 1)

# Neonatal adverse events characteristics
neonatal_ae_cov <- round((total_neonatal_ae / total_char) * 100, 1)


## Prepare dataset for bar plot of PICO coverage ----
# Create data-frame
complete_dataset <- 
  data.frame(pico = c("Participant", "Intervention", "Comparator", "Labour duration", "Labour-related features", "Maternal AEs", "Neonatal AEs"),
             value = c(participant_cov, intervention_cov, comparator_cov, labour_out_cov, labour_char_cov, maternal_ae_cov, neonatal_ae_cov),
             count = c(total_partic, total_interv, total_compar, total_labour_out, total_labour_char, total_maternal_ae, total_neonatal_ae),
             type = c("Participant", "Intervention", "Comparator", rep("Outcome", 4)))

# Create the bar plot
tiff("./Figures/Supplementary Figure 2.tiff", 
     height = 28, 
     width = 45, 
     units = "cm", 
     compression = "lzw", 
     res = 300)
ggplot(complete_dataset, 
       aes(x = factor(pico, levels = unique(pico)), 
           y = value,
           fill = factor(type, levels = unique(type)))) +
  geom_bar(stat = "identity",
           show.legend = TRUE) +
  geom_text(aes(x = pico, 
                y = value,
                label = paste0(value, "% (n = ", count, ")")), 
            vjust = -0.15,
            size = 5.5, 
            color = "black") +
  scale_fill_manual(breaks = c("Participant", "Intervention", "Comparator", rep("Outcome", 4)),
                    values = c("#CC79A7", "#E69F00", "#56B4E9", "#009E73")) +
  scale_y_continuous(breaks = seq(0, 100, 20),
                     limits = c(0, 100)) +
  labs(x = "",
       y = "Percentage PICO coverage (%)",
       fill = "PICO feature") + 
  ggtitle("Frequency of extracted PICO features") +
  theme_classic() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 16, face = "bold"),
        axis.text = element_text(size = 16),
        legend.position = "bottom",
        legend.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 16))
dev.off()


## Percentage coverage systematic reviews per PICO ----
# Participant characteristics
review_partic_n <- rowSums(!is.na(participant_new[, -c(1, 2)]))
review_partic <- round((review_partic_n / total_partic) * 100, 1)

# Intervention characteristics
review_interv_n <- rowSums(!is.na(intervention_new[, -c(1, 2)]))
review_interv <- round((review_interv_n / total_interv) * 100, 1)

# Comparator characteristics
review_compar_type_n <- ifelse(rowSums(!is.na(comparator_new[, c("Placebo", "No treatment", "Other drug")])) > 1, 1, rowSums(!is.na(comparator_new[, c("Placebo", "No treatment", "Other drug")]))) 
review_compar_route_n <- ifelse(!is.na(comparator_new[, "Administration route"]) == TRUE, 1, 0)
review_compar_n <- review_compar_type_n + review_compar_route_n
review_compar <- round((review_compar_n / total_compar) * 100, 1)

# Labour outcomes 
review_duration_n <- rowSums(!is.na(labour_duration[, -c(1, 2)]))
review_duration <- round((review_duration_n / total_labour_out) * 100, 1)

# Labour related characteristics 
review_labour_char_n <- rowSums(!is.na(labour_char[, -c(1, 2)]))
review_labour_char <- round((review_labour_char_n / total_labour_char) * 100, 1)

# Maternal adverse events characteristics 
review_maternal_n <- rowSums(!is.na(maternal_ae[, -c(1, 2)]))
review_maternal <- round((review_maternal_n / total_maternal_ae) * 100, 1)

# Neonatal adverse events characteristics 
review_neonatal_n <- rowSums(!is.na(neonatal_ae[, -c(1, 2)]))
review_neonatal <- round((rowSums(!is.na(neonatal_ae[, -c(1, 2)])) / total_neonatal_ae) * 100, 1)


## Prepare dataset for tabulation (Systematic review PICO coverage) ----
# Merge vectors (percentages)
review_dataset <- 
  data.frame(review = participant_new$`First author`, review_partic, review_interv, review_compar, review_duration, review_labour_char, review_maternal, review_neonatal)

# Merge vectors (counts)
review_dataset_n <- 
  data.frame(review = participant_new$`First author`, review_partic_n, review_interv_n, review_compar_n, review_duration_n, review_labour_char_n, review_maternal_n, review_neonatal_n)

# Add the total coverage per systematic review
review_dataset$total_cov <- round((rowSums(review_dataset_n[, -1]) / total_char) * 100, 1)

# First author with publication year
review_dataset$review <- paste(participant$`First author`, participant$Year)

# Re-order the whole dataset by year and alphabetically
review_dataset <- review_dataset[order(str_right(review_dataset$review, 4), review_dataset$review), ]

# Turn into log format
review_dataset_long <- melt(review_dataset)

# Add the PICO type
review_dataset_long$type <- rep(c("Participant", "Intervention", "Comparator", rep("Outcome", 4), "Total coverage"), each = length(review_partic))

# Rename the variables
review_dataset_long <- review_dataset_long %>%
  mutate(variable = recode(variable , 
                           "review_partic" = "Participant\n(n = 20)",                  # total_partic
                           "review_interv" = "Intervention\n(n = 3)",                  # total_interv
                           "review_compar" = "Comparator\n(n = 2)",                    # total_compar
                           "review_duration" = "Labour duration\n(n = 5)",             # total_labour_out
                           "review_labour_char" = "Labour features\n(n = 7)",          # total_labour_char
                           "review_maternal" = "Maternal AEs\n(n = 13)",               # total_maternal_ae
                           "review_neonatal" = "Neonatal AEs\n(n = 7)",                # total_neonatal_ae
                           "total_cov" = "Total coverage\n(across all features)"))

# Create a dataset with the counts of characteristics per systematic review and PICO
review_dataset_counts <- 
  data.frame(review = participant_new$`First author`, review_partic_n, review_interv_n, review_compar_n, review_duration_n, review_labour_char_n, review_maternal_n, review_neonatal_n, dummy = rep(-1, 8))

# First author with publication year
review_dataset_counts$review  <- paste(participant$`First author`, participant$Year)

# Re-order the whole dataset by year and alphabetically
review_dataset_counts <- review_dataset_counts[order(str_right(review_dataset_counts$review, 4), review_dataset_counts$review), ]

# Turn into log format
review_dataset_counts_long <- melt(review_dataset_counts)

# Add counts into the 'review_dataset_long' dataset
review_dataset_long$counts <- review_dataset_counts_long$value

# Bubble plot
tiff("./Figures/Figure 2.tiff", 
     height = 28, 
     width = 45, 
     units = "cm", 
     compression = "lzw", 
     res = 300)
ggplot(review_dataset_long, 
       aes(x = variable, 
           y = factor(review, levels = unique(review)),
           colour = type)) +
  geom_point(aes(size = value),
             alpha = 0.4) +
  geom_text(aes(x = variable, 
                y = factor(review, levels = unique(review)),
                label = ifelse(counts < 0, paste0(value, "%"), paste0(value, "%\n(", counts, ")"))),
            col = "black",
            size = 5,
            vjust = 0.5,
            hjust = 0.5,
            fontface = "bold",
            lineheight = 0.85) +
  labs(x = "",
       y = "",
       size = "Characteristic was reported") + 
  ggtitle("PICO-specific and total percentage coverage of each review") +
  scale_size(range = c(0, 25)) +
  scale_colour_manual(breaks = c("Participant", "Intervention", "Comparator", "Outcome", "Total coverage"),
                      values = c("#CC79A7", "#E69F00", "#56B4E9", "#009E73", "red")) +
  scale_x_discrete(position = 'top') +
  geom_vline(xintercept = 7 + 0.52,
             col = "grey") +
  guides(colour = "none",
         size = "none") +
  theme_classic() +
  theme(strip.placement = "outside",
        plot.title = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(size = 16, hjust = 0.5, vjust = 0),
        axis.text.y = element_text(size = 16))
dev.off()
