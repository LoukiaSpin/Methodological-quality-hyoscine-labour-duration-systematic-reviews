#*******************************************************************************
#*
#*
#*                         Coverage of Included Trials                                                                                                                                     
#*       
#* Author: Loukia M. Spineli
#* Date: April 2025      
#*******************************************************************************      


## Load libraries ----
list.of.packages <- c("forstringr", "reshape2", "dplyr", "ggplot2", "ggpubr")
lapply(list.of.packages, require, character.only = TRUE); rm(list.of.packages)


## Load datasets ----
# Included trials
load("./30_Analysis/Datasets in RData/included_trials.RData")



## Tabulate systematic review with trial and publication year ----
# First remove 'Sirohiwal 2005' for being non-randomised (all other reviews considered only RCTs)
included_trials <- included_trials[included_trials$`First author trial` != "Sirohiwal 2005", ]

# First author with publication year
included_trials$`First author SR` <- paste(included_trials$`First author SR`, included_trials$`Year review`)

# Number of unique trials across the reviews
length(table(included_trials$`First author trial`)) # 56 unique trials

# Number of trials per review
num_trials_sr <- unlist(lapply(split(included_trials, included_trials$`First author SR`), function(x) dim(x)[1]))
range(num_trials_sr) # 6 to 44
median(num_trials_sr) # 10

# Range of trial publication year
range(included_trials$`Year trial`) # 1993 to 2021

# Remove publication year from trial name (add new column)
included_trials$First_author_trial_new <- substr(included_trials$`First author trial`, 1, nchar(included_trials$`First author trial`) - 5)

# Indicate whether a review included a trial or not
dataset <- melt(table(included_trials$`First author SR`, included_trials$`First author trial`))
colnames(dataset) <- c("review", "trial", "value")

# Remove publication year from trial name (add new column)
dataset$trial_new <- substr(dataset$trial, 1, nchar(as.character(dataset$trial)) - 5)

# Add publication year from trial name (add new column)
dataset$trial_year_new <- str_right(as.character(dataset$trial), 4)

# Replace value == 2 with 1
dataset$value_new <- ifelse(dataset$value == 2, 1, dataset$value) 

# Re-order the whole datasetd by year and alphabetically
dataset <- dataset[order(str_right(as.character(dataset$review), 4), dataset$review), ]

# Trial was published before the review
dataset$value_new <- ifelse(str_right(as.character(dataset$review), 4) < dataset$trial_year_new, 2, dataset$value_new)

# Calculate the frequency of each trial
dataset_new <- 
  subset(dataset, value == 1) %>% 
  count(trial)

# Remove publication year from trial name (add new column)
dataset_new$trial_new <- substr(dataset_new$trial, 1, nchar(as.character(dataset_new$trial)) - 5)

# Add publication year from trial name (add new column)
dataset_new$trial_year_new <- str_right(as.character(dataset_new$trial), 4)


## Create plot ----
tiff("./30_Analysis/Supplementary Figures/Supplementary Figure 6.tiff", 
     height = 28, 
     width = 49, 
     units = "cm", 
     compression = "lzw", 
     res = 300)
ggplot(dataset, 
       aes(x = trial_new, 
           y = factor(review, levels = unique(review)),
           fill = as.factor(value_new))) +
  geom_point(size = 7,
             shape = 21,
             col = "white",
             alpha = 0.7) +
  facet_grid(. ~ trial_year_new,
             scales = "free_x",
             space = "free_x") +
  scale_fill_manual(breaks = c("1", "0", "2"),
                    values = c("#009E73", "#D55E00", "grey85"),
                    labels = c("0" = "No", "1" = "Yes", "2" = "Not applicable")) +
  geom_label(data = dataset_new,
            aes(x = trial_new, 
                y = 1.1,
                label = n),
            hjust = 0.5,
            vjust = 2.5,
            colour = "grey45",
            fontface = "bold",
            size = 4,
            inherit.aes = FALSE) +
  scale_x_discrete(position = 'top') +
  labs(x = "",
       y = "",
       fill = "Trial was included") + 
  theme_classic() +
  theme(strip.placement = "outside",
        axis.text.x = element_text(size = 11, angle = 90, hjust = 0, vjust = 0),
        axis.text.y = element_text(size = 11),
        strip.background = element_rect(fill = 'white'),
        strip.switch.pad.grid = unit(10, "pt"),
        legend.position = "bottom",
        legend.margin = margin(t = -10),
        legend.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 14))
dev.off()


## Percentage intra-coverage of each systematic review ----
# Split the dataset by the reviews
split_dataset <- split(dataset, dataset$review)

# Restrict each review to the trials they included
found_trials <- lapply(split_dataset, function(x) dim(subset(x, value_new == 1))[1])

# Restrict each review to the trials found up the publication year of the review
total_trials <- lapply(split_dataset, function(x) dim(subset(x, value_new < 2))[1])

# Intra-coverage (%)
intra_coverage <- round((unlist(found_trials) / unlist(total_trials)) * 100, 1)

# Bring in a data-frame
intra_coverage_dataset <- data.frame(intra_coverage = c(intra_coverage, 100 - intra_coverage), 
                                     n = c(unlist(found_trials), unlist(total_trials) - unlist(found_trials)), 
                                     review = rep(names(intra_coverage), 2),
                                     included = rep(c("Yes", "No"), each = length(found_trials)))


# Create the bar plot
bar_plot <- 
  ggplot(intra_coverage_dataset, 
         aes(x = factor(review, levels = unique(review[order(str_right(review, 4), review, decreasing = TRUE)])), 
             y = intra_coverage,
             fill = factor(included, levels = c("No", "Yes")))) +
  geom_bar(stat = "identity",
           position = "stack") +
  geom_text(aes(x = review, 
                y = intra_coverage,
                label = paste0(intra_coverage, "%\n(n = ", n, ")")), 
            position = position_stack(vjust = .6),
            hjust = 0.5,
            size = 4.5,
            lineheight = 0.8,
            #fontface = "bold",
            colour = "black") +
  scale_fill_manual(breaks = c("Yes", "No"),
                    values = c("#009E73", "#D55E00")) +
  scale_y_continuous(breaks = seq(0, 100, 20),
                     limits = c(0, 100)) +
  labs(x = "",
       y = "Percentage intra-coverage (%)",
       fill = "Included trials") + 
  theme_classic() +
  theme(axis.title = element_text(size = 15, face = "bold"),
        axis.text = element_text(size = 15),
        legend.position = "bottom",
        legend.title = element_text(size = 15, face = "bold"),
        legend.text = element_text(size = 15))


## Publication year coverage across the systematic reviews ----
# For each trial, find the systematic reviews published the same or later year
prepare_dataset0 <- lapply(names(intra_coverage), function(x) ifelse(dataset_new$trial_year_new <= str_right(x, 4), 1, 0))

# Merge with the first columns of 'dataset_new'
prepare_dataset <- data.frame(dataset_new[, c(1:2, 4)], t(do.call("rbind", prepare_dataset0)))
colnames(prepare_dataset)[-c(1:3)] <- names(intra_coverage)

# For each trial add the sum of systematic reviews published the same or later year
prepare_dataset$total_reviews <- rowSums(prepare_dataset[, -c(1:3)])

# Split the data-frame by trial publication year
trial_pub_year <- split(prepare_dataset, prepare_dataset$trial_year_new)

# Percentage publication year coverage (%)
pub_year_coverage <- round(unlist(lapply(trial_pub_year, function(x) sum(x$n) / sum(x$total_reviews))) * 100, 1)

# Combine cumulatively across the years
cum_trial_pub_year <- lapply(1:length(trial_pub_year), function(x) do.call("rbind", trial_pub_year[1:x]))

# Cumulative publication year coverage (%)
cum_pub_year_coverage <- round(unlist(lapply(cum_trial_pub_year, function(x) sum(x$n) / sum(x$total_reviews))) * 100, 1)

# Include also cumulative probabilities
complete_dataset <- data.frame(year = rep(names(pub_year_coverage), 2), 
                               prob = c(pub_year_coverage, cum_pub_year_coverage),
                               type = rep(c("Year-specific", "Cumulatively"), each = length(pub_year_coverage)))

# Create the line plot
line_plot <- 
  ggplot(complete_dataset, 
         aes(x = year, 
             y = prob,
             group = factor(type, levels = c("Year-specific", "Cumulatively")),
             colour = factor(type, levels = c("Year-specific", "Cumulatively")))) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3,
             col = "black") +
  geom_text(aes(x = year, 
                y = prob,
                label = paste0(prob, "%")), 
            vjust = -0.7,
            size = 4.5, 
            color = "black",
            check_overlap = TRUE) +
  scale_colour_manual(breaks = c("Year-specific", "Cumulatively"),
                    values = c("#D93511", "#808080")) +
  scale_y_continuous(breaks = seq(0, 100, 20),
                     limits = c(0, 100)) +
  labs(x = "",
       y = "Percentage year coverage (%)",
       colour = "Coverage calculated") + 
  theme_classic() +
  theme(axis.title = element_text(size = 15, face = "bold"),
        axis.text = element_text(size = 15),
        legend.position = "bottom",
        legend.title = element_text(size = 15, face = "bold"),
        legend.text = element_text(size = 15))


## Bring together ----
tiff("./30_Analysis/Main Figures/Figure 5.tiff", 
     height = 35, 
     width = 45, 
     units = "cm", 
     compression = "lzw", 
     res = 300)
ggarrange(bar_plot, line_plot,
          nrow = 2,
          labels = c("a)", "b)"))
dev.off()
