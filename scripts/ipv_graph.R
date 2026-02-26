##women and IPV, descriptives 
outcomes_ipv <- c("d106", "d107", "d108", "d129")

library(tidyverse)


custom_2 <- c("Less Severe Violence", "Severe Violence", "Sexual Violence", "Afraid of Husband")
outcomes_ipv <- c("d106", "d107", "d108", "d129")

# Calculate raw counts for each outcome (yes = 1)
ipv_counts <- map2_dfr(outcomes_ipv, custom_2, function(var, label) {
  data.frame(
    outcome = label,
    count = sum(data_2019[[var]] == 1, na.rm = TRUE),
    proportion = sum(data_2019[[var]] == 1, na.rm = TRUE) / sum(!is.na(data_2019[[var]]))
  )
})
# Plot
ggplot(ipv_counts, aes(x = factor(outcome, levels = custom_2), y = proportion * 100)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = paste0(round(proportion * 100, 1), "%")), 
            vjust = -0.5, size = 3.5) +
  scale_y_continuous(limits = c(0, max(ipv_counts$proportion * 100) * 1.15)) +
  labs(
    title = "Prevalence of Intimate Partner Violence",
    subtitle = "Proportion of women reporting each type of violence",
    x = NULL,
    y = "Percentage (%)",
    caption = "Unweighted estimates. Sample restricted to ever-married women."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(size = 10)
  )