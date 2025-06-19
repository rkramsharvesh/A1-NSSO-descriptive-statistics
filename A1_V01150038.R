setwd("D:\\Masters\\VCU\\Classes\\SCMA\\R\\A1")

# 1. Installing and Importing Necessary Libraries ####
install_and_load <- function(package) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package, dependencies = TRUE)
    library(package, character.only = TRUE)
  }
}

libraries <- c("dplyr", "readr", "readxl", "tidyr", "ggplot2", "BSDA")
lapply(libraries, install_and_load)

# 2. Reading the Dataset ####
data <- read.csv("../A1/Ref/NSSO68.csv")

# 3. Filter by State and Save ####
state_name <- "KA"
state_data <- data %>% filter(state_1 == state_name)
write.csv(state_data, '../A1/Ref/state_filtered_data.csv')

# 4. Display Dataset Info ####
cat("Dataset Information:\n")
print(names(state_data))
print(head(state_data))
print(dim(state_data))
cat("Total missing values:", sum(is.na(state_data)), "\n")

# 5. Check for Missing Values ####
missing_info <- colSums(is.na(state_data))
cat("Missing Values Information:\n")
print(missing_info)

# 6. Subset Relevant Columns ####
state_subset <- state_data %>%
  select(state_1, District, Region, Sector, State_Region,
         Meals_At_Home, ricetotal_v, wheattotal_v, Milktotal_v,
         pulsestot_v, chicken_v, No_of_Meals_per_day)

# 7. Impute Missing Values with Mean ####
impute_with_mean <- function(column) {
  if (any(is.na(column))) {
    column[is.na(column)] <- mean(column, na.rm = TRUE)
  }
  return(column)
}

state_subset$Meals_At_Home <- impute_with_mean(state_subset$Meals_At_Home)

# 8. Define and Apply Outlier Removal ####
remove_outliers_vec <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower <- Q1 - 1.5 * IQR
  upper <- Q3 + 1.5 * IQR
  x[x < lower | x > upper] <- NA
  return(x)
}

boxplot_columns <- c("Meals_At_Home", "ricetotal_v", "wheattotal_v",
                     "Milktotal_v", "pulsestot_v", "chicken_v", "No_of_Meals_per_day")

state_subset_plot <- state_subset %>%
  mutate(across(all_of(boxplot_columns), remove_outliers_vec))

# 9. Create Total Consumption Variable ####
state_subset$total_consumption <- rowSums(state_subset[, c('ricetotal_v', 'wheattotal_v', 'Milktotal_v',
                                                           'pulsestot_v', 'chicken_v')], na.rm = TRUE)

# 10. Summarize by District, Region, Sector ####
summarize_consumption <- function(group_col) {
  summary <- state_subset %>%
    group_by(across(all_of(group_col))) %>%
    summarise(total = sum(total_consumption, na.rm = TRUE)) %>%
    arrange(desc(total))
  return(summary)
}

district_summary <- summarize_consumption("District")
region_summary <- summarize_consumption("Region")
sector_summary <- summarize_consumption("Sector")

cat("Top Consuming Districts:\n")
print(head(district_summary, 4))
cat("Bottom Consuming Districts:\n")
print(tail(district_summary, 4))
cat("Region Consumption Summary:\n")
print(region_summary)
cat("Sector Consumption Summary:\n")
print(sector_summary)

# 11. Rename Districts and Sector Codes ####
district_mapping <- c("20" = "Bangalore", "26" = "Mysore", "1" = "Belgaum",
                      "4" = "Gulbarga", "3" = "Bijapur", "2" = "Bagalkot",
                      "5" = "Bidar", "13" = "Chitradurga", "25" = "Kodagu", "8" = "Gadag")
sector_mapping <- c("2" = "URBAN", "1" = "RURAL")

state_subset$District <- as.character(state_subset$District)
state_subset$Sector <- as.character(state_subset$Sector)
state_subset$District <- ifelse(state_subset$District %in% names(district_mapping),
                                district_mapping[state_subset$District],
                                state_subset$District)
state_subset$Sector <- ifelse(state_subset$Sector %in% names(sector_mapping),
                              sector_mapping[state_subset$Sector],
                              state_subset$Sector)

# Recompute summaries after renaming
district_summary <- summarize_consumption("District")
region_summary <- summarize_consumption("Region")
sector_summary <- summarize_consumption("Sector")

cat("Highest Consuming Districts:\n")
print(head(district_summary, 4))
cat("Lowest Consuming Districts:\n")
print(tail(district_summary, 4))
cat("Region Consumption Summary:\n")
print(region_summary)
cat("Sector Consumption Summary:\n")
print(sector_summary)

# 12. Z-Test: Urban vs Rural ####
rural <- state_subset %>% filter(Sector == "RURAL") %>% pull(total_consumption)
urban <- state_subset %>% filter(Sector == "URBAN") %>% pull(total_consumption)

z_test_result <- z.test(rural, urban, alternative = "two.sided",
                        mu = 0, sigma.x = sd(rural), sigma.y = sd(urban), conf.level = 0.95)

cat("\nUrban vs Rural Z-Test:\n")
print(z_test_result)
if (z_test_result$p.value < 0.05) {
  cat("P value is <", 0.05, "→ Reject H0: Significant difference.\n")
} else {
  cat("P value is >=", 0.05, "→ Fail to reject H0: No significant difference.\n")
}

# 13. Z-Test: Top vs Bottom District ####
top_district <- state_subset %>%
  filter(District == "Bangalore") %>% pull(total_consumption)

bottom_district <- state_subset %>%
  filter(District == "Bidar") %>% pull(total_consumption)

z_test_result <- z.test(top_district, bottom_district, alternative = "two.sided",
                        mu = 0, sigma.x = sd(top_district), sigma.y = sd(bottom_district), conf.level = 0.95)

cat("\nTop vs Bottom District Z-Test:\n")
print(z_test_result)
if (z_test_result$p.value < 0.05) {
  cat("P value is <", 0.05, "→ Reject H0: Significant difference.\n")
} else {
  cat("P value is >=", 0.05, "→ Fail to reject H0: No significant difference.\n")
}

# 14. Box Plots After Outlier Removal ####
df_box_long <- state_subset_plot %>%
  pivot_longer(cols = all_of(boxplot_columns),
               names_to = "variable", values_to = "value")

ggplot(df_box_long, aes(x = variable, y = value)) +
  geom_boxplot(outlier.size = 0.8, fill = "skyblue") +
  facet_wrap(~ variable, scales = "free_y") +
  labs(title = "Box-plots of Selected Variables (Outliers Removed)") +
  theme_bw()
