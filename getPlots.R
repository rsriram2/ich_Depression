engagement <- engagement %>%
  mutate(patient_id = sub(".*/(\\d{4}-\\d{3}).*", "\\1", file))

# Print the resulting patient_id to verify
print(head(engagement$patient_id))

# Merge `engagement` with `demographic_outcomes`
merged_data <- engagement %>%
  left_join(demographic_outcomes, by = c("patient_id" = "id_patient")) %>%
  select(patient_id, label, pct_region_engaged, cesd_d180, depression) %>%
  filter(label != 0)  # Exclude rows where label is 0

# Define the region mapping directly within the code
region_mapping <- data.frame(
  label = 1:20,
  region = c(
    "Frontal_L", "Frontal_R", "Temporal_L", "Temporal_R", "Parietal_L", "Parietal_R",
    "Occipital_L", "Occipital_R", "Caudate_L", "Caudate_R", "Lenticular_L", "Lenticular_R",
    "Thalamus_L", "Thalamus_R", "InternalCapsule_L", "InternalCapsule_R", "DeepWhiteMatter_L",
    "DeepWhiteMatter_R", "Cerebellum", "Brainstem"
  )
)

merged_data <- merged_data %>%
  left_join(region_mapping, by = "label")


library(ggplot2)
library(dplyr)

regions <- unique(merged_data$region)

#non-binary plot
pdf("Percent Engagement vs. Depression Score for Each Region.pdf", width = 8, height = 6)

for (region in regions) {
  subset_data <- merged_data %>% filter(region == !!region)

  p <- ggplot(subset_data, aes(x = pct_region_engaged, y = cesd_d180)) +
    geom_point() +
    labs(title = paste("Percent Engagement vs. Depression Score for Region", region),
         x = "Percent Engagement",
         y = "Depression Score (CESD-D180)") +
    theme_minimal()

  print(p)
}
dev.off()
)

#binary plot
pdf("Percent_Engagement_vs_Binary_Depression_Score.pdf", width = 8, height = 6)
for (region in regions) {
  subset_data <- merged_data %>% filter(region == !!region)

  p <- ggplot(subset_data, aes(x = pct_region_engaged, y = depression)) +
    geom_point() +
    labs(title = paste("Percent_Engagement_vs_Binary_Depression_Score", region),
         x = "Percent Engagement",
         y = "Binary Depression Score") +
    theme_minimal()

  print(p)
}
dev.off()

# AUC plot: Percent Engagement vs. Depression Score with AUC
pdf("AUC_for_Percent_Engagement_vs_Depression_Score.pdf", width = 8, height = 6)

for (region in regions) {
  subset_data <- merged_data %>% filter(region == !!region)

  # Calculate ROC curve and AUC
  roc_result <- roc(subset_data$cesd_d180, subset_data$pct_region_engaged)
  auc_value <- auc(roc_result)

  # Create plot with AUC
  p <- ggplot(subset_data, aes(x = pct_region_engaged, y = cesd_d180)) +
    geom_point() +
    labs(title = paste("Percent Engagement vs. Depression Score for", region,
                       "\nAUC =", round(auc_value, 2)),
         x = "Percent Engagement",
         y = "Depression Score") +
    theme_minimal()

  print(p)
}

dev.off()


library(pROC)
library(dplyr)

auc_list <- list()

for (region_name in names(mods)) {
  mod_list <- mods[[region_name]]

  if (is.null(mod_list)) next

  model <- mod_list$model

  # Predict the probability of depression
  preds <- predict(model, type = "response")


    roc_result <- roc(model$y, preds)
    auc_value <- auc(roc_result)

    # Store AUC value in a list
    auc_list[[region_name]] <- data.frame(region = region_name, auc = auc_value)
  }

# Convert list to a data frame
auc_values_df <- do.call(rbind, auc_list)

library(ggplot2)

# Assuming auc_values_df is your data frame with AUC values
auc_values_df <- read.csv("AUC_values_by_region.csv")

# Create a bar plot of AUC values
ggplot(auc_values_df, aes(x = reorder(region, auc), y = auc)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() +
  labs(title = "AUC Values for Each Region",
       x = "Region",
       y = "AUC") +
  theme_minimal()
