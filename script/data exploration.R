#Initial data exploration of vitality data collected during 2025 A and
  #B season groundfish fisheries

#Author: Erin Fedewa

#load
library(dplyr)
library(tidyverse)
library(readr)
library(scales)

dat <- read.csv("./data/Crab_vitality.csv")

###############
#DATA SUMMARIES:

#create overall summary
overall_summary <- tibble(
  total_records = nrow(dat),
  unique_cruises = n_distinct(dat$cruise, na.rm = TRUE),
  number_of_observers = n_distinct(dat$observer_id, na.rm = TRUE),
  number_of_species = n_distinct(dat$species, na.rm = TRUE),
  number_of_hauls = n_distinct(dat$haul, na.rm = TRUE))

print(overall_summary)

#summary tables
# Samples by species
samples_by_species <- dat %>%
  count(species, name = "n_samples", sort = TRUE) %>%
  mutate(percent = 100 * n_samples / sum(n_samples))

# Samples by shell condition
samples_by_shell_condition <- dat %>%
  count(shell_condition, name = "n_samples", sort = TRUE) %>%
  mutate(percent = 100 * n_samples / sum(n_samples))

# Species by shell condition
species_by_shell <- dat %>%
  count(species, shell_condition, name = "n_samples") %>%
  arrange(species, desc(n_samples))

# Samples by month, including months with zero records
samples_by_month <- dat %>%
  count(month, name = "n_samples") %>%
  arrange(month)

# Samples by sex
samples_by_sex <- dat %>%
  count(sex, name = "n_samples", sort = TRUE)

# Samples by fishery
samples_by_fishery <- dat %>%
  count(fishery, name = "n_samples", sort = TRUE)

######################################
#DATA SUMMARY PLOTS

# Plot 1: Samples by species
p_species <- ggplot(samples_by_species,
  aes(x = reorder(as.character(species), n_samples),
    y = n_samples)) +
  geom_col(na.rm = FALSE) +
  coord_flip() +
  labs(title = "Samples by Species",
    x = "Species", y = "Number of Samples")
ggsave(p_species, filename="./figures/data exploration/n_species.png")

# Plot 2: Samples by shell condition
p_shell <- ggplot(samples_by_shell_condition,
  aes(x = reorder(as.character(shell_condition), n_samples),
    y = n_samples)) +
  geom_col() +
  coord_flip() +
  labs(title = "Samples by Shell Condition",
    x = "Shell Condition", y = "Number of Samples")
ggsave(p_shell, filename="./figures/data exploration/n_shellcondition.png")

# Plot 3: Shell condition within each species
plot_species_shell <- dat %>%
  count(species, shell_condition, name = "n_samples")

p_species_shell <- ggplot(plot_species_shell,
  aes(x = species, y = n_samples, fill = shell_condition)) +
  geom_col() +
  labs(title = "Shell Condition by Species",
    x = "Species", y = "Number of Samples", fill = "Shell Condition") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(p_species_shell, filename="./figures/data exploration/n_species_shell.png")

# Plot 4: Samples by month
p_month <- ggplot(samples_by_month,
  aes(x = factor(month, levels = 1:12), y = n_samples)) +
  geom_col() +
  scale_x_discrete(labels = month.abb) +
  scale_y_continuous(breaks = breaks_pretty()) +
  labs(title = "Sampling Effort by Month",
    x = "Month", y = "Number of Samples")
ggsave(p_month, filename="./figures/data exploration/n_month.png")

# Plot 5: Species by month
p_species_month <- ggplot(plot_species_month <- dat %>%
    count(month, species, name = "n_samples"),
  aes(x = factor(month, levels = 1:12),
      y = n_samples, fill = species)) +
  geom_col() +
  scale_x_discrete(labels = month.abb) +
  labs(title = "Sampling by Month and Species",
    x = "Month", y = "Number of Samples", fill = "Species")
ggsave(p_species_month, filename="./figures/data exploration/n_species_month.png")

######################################
#VITALITY METRIC PLOTS

# Set vitality categories in the desired order
vitality_levels <- c("Excellent", "Poor", "Dead")

dat_vitality <- dat %>%
  mutate(vitality = tolower(trimws(as.character(vitality))),
    vitality = recode(vitality, "e" = "Excellent",
      "p" = "Poor", "d" = "Dead", .default = vitality),
    vitality = factor(vitality, levels = vitality_levels),
    month = as.integer(as.character(month)),
    length = as.numeric(as.character(length)),
    species = as.character(species),
    shell_condition = as.character(shell_condition),
    sex = as.character(sex),
    fishery = as.character(fishery)) %>%
  filter(!is.na(vitality))

# Use a consistent color scheme across every plot
vitality_colors <- c(
  "Excellent" = "#238443",
  "Poor"      = "#FDB863",
  "Dead"      = "#D73027")

theme_set(theme_bw(base_size = 12))

# Output folder
out_dir <- "./figures/data exploration"

# Helper function to save plots
save_plot <- function(plot, filename, width = 10, height = 7) {
  ggsave(filename = file.path(out_dir, filename),
    plot = plot, width = width, height = height, dpi = 300)
}

#Plot 1: Overall proportions of vitality categories within each species

vitality_by_species <- dat %>%
  count(species, vitality, name = "n") %>%
  group_by(species) %>%
  mutate(total = sum(n), percent = 100 * n / total) %>%
  ungroup()

p1 <- ggplot(dat_vitality, aes(x = species, fill = vitality)) +
  geom_bar(position = "fill") +
  coord_flip() +
  scale_fill_manual(values = vitality_colors, drop = FALSE) +
  scale_y_continuous(labels = percent) +
  labs(title = "Vitality by Species",
    x = "Species", y = "Proportion of Samples",
    fill = "Vitality")

save_plot(p1, "vitality_by_species.png")

#Plot 2: Vitality by shell condition, faceted by species
p2 <- ggplot(dat_vitality %>%
    filter(!is.na(shell_condition)),
  aes(x = shell_condition, fill = vitality)) +
  geom_bar(position = "fill") +
  facet_wrap(~ species, scales = "free_x") +
  scale_fill_manual(values = vitality_colors,
    drop = FALSE) +
  scale_y_continuous(labels = percent) +
  labs(title = "Vitality by Shell Condition and Species",
    x = "Shell Condition", y = "Proportion of Samples",
    fill = "Vitality") +
  theme(axis.text.x = element_text(
      angle = 45, hjust = 1))

save_plot(p2, "vitality_by_shell_condition.png",
          width = 12, height = 8)

#Plot 3: Vitality by sex, faceted by species
p3 <- ggplot(dat_vitality %>% filter(sex %in% c("f", "m")),
  aes(x = sex, fill = vitality)) +
  geom_bar(position = "fill") +
  facet_wrap(~ species) +
  scale_fill_manual(
    values = vitality_colors, drop = FALSE) +
  scale_y_continuous(labels = percent) +
  labs(title = "Vitality by Sex and Species",
    x = "Sex", y = "Proportion of Samples",
    fill = "Vitality")

save_plot(p3, "vitality_by_sex.png",
          width = 12, height = 8)

#Plot 4: Vitality by length, faceted by species

#first let's look at length distribution of samples
p_length_density <- ggplot(dat_vitality %>%
    filter(!is.na(length), length > 0),
  aes(x = length)) +
  geom_density(fill = "steelblue", alpha = 0.4,
    color = "steelblue4", linewidth = 0.8, na.rm = TRUE) +
  facet_wrap(~ species, scales = "free_y") +
  labs(title = "Length Distributions by Species",
    x = "Length (check units)", y = "Density") +
  theme_bw(base_size = 12)

save_plot(p_length_density, "length_by_species.png",
          width = 12, height = 8)

#now examine vitality composition across 10mm length bins
p4 <- ggplot(dat_vitality %>% filter(!is.na(length)),
  aes(x = cut_width(length, width = 10, boundary = 0),
    fill = vitality)) +
  geom_bar(position = "fill") +
  facet_wrap(~ species, scales = "free_x") +
  scale_fill_manual(values = vitality_colors, drop = FALSE) +
  scale_y_continuous(labels = percent) +
  labs(title = "Vitality Composition Across Length Bins",
    x = "Length (10 mm bins)",
    y = "Proportion of Samples", fill = "Vitality") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

save_plot(p4, "vitality_by_length.png",
          width = 14, height = 9)

#Plot 5: Vitality by fishery, faceted by species:
p5 <- ggplot(dat_vitality, aes(x = fishery, fill = vitality)) +
  geom_bar(position = "fill") +
  facet_wrap(~ species, scales = "free_x") +
  scale_fill_manual(values = vitality_colors, drop = FALSE) +
  scale_y_continuous(labels = percent) +
  labs(title = "Vitality by Fishery and Species",
    x = "Fishery", y = "Proportion of Samples",
    fill = "Vitality") +
  theme(axis.text.x = element_text(
      angle = 45, hjust = 1))

save_plot(p5, "vitality_by_fishery.png",
          width = 12, height = 8)

#Plot 6: Vitality by month, faceted by species
p6 <- ggplot(dat_vitality, aes(x = factor(month, levels = 1:12), fill = vitality)) +
  geom_bar(position = "fill") +
  facet_wrap(~ species) +
  scale_x_discrete(labels = month.abb) +
  scale_fill_manual(values = vitality_colors, drop = FALSE) +
  scale_y_continuous(labels = percent) +
  labs(title = "Vitality by Month and Species",
    x = "Month", y = "Proportion of Samples",
    fill = "Vitality")

save_plot(p6, "vitality_by_month.png",
          width = 14, height = 9)

