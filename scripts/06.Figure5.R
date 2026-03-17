# ============================================================
#  Master Thesis: Fig.5 Kernel Regression Density Graph
#  Author: Ananya Mazumder 
# ============================================================


# ── Setting up necessary variables ────────────────────────────────────────
b2_cols <- sort(names(data_2019)[grepl("^b2_", names(data_2019))])

data_2019 <- data_2019 %>%
  mutate(
    # bord tells you birth order directly, so:
    # firstborn = child with highest bord index = last b2 column
    # secondborn = second highest
    birth_year_firstborn = apply(select(., all_of(b2_cols)), 1, function(x) {
      vals <- x[!is.na(x)]
      if(length(vals) == 0) NA else vals[length(vals)]
    }),
    
    birth_year_secondborn = apply(select(., all_of(b2_cols)), 1, function(x) {
      vals <- x[!is.na(x)]
      if(length(vals) < 2) NA else vals[length(vals) - 1]
    }),
    
    # Age at interview using v007 (interview year)
    age_firstborn  = v007 - birth_year_firstborn,
    age_secondborn = v007 - birth_year_secondborn
  )
# ── Running the regressions ────────────────────────────────────────
# Filter to relevant groups
df_plot <- data_2019 %>%
  filter(!is.na(age_firstborn), !is.na(age_secondborn)) %>%
  mutate(district_type = ifelse(treated_district == 1, "Treatment", "Control")) 


# Kernel density plot - first and second born side by side
p1 <- ggplot(df_plot, aes(x = age_firstborn, color = district_type, fill = district_type)) +
  geom_density(alpha = 0.3, kernel = "epanechnikov") +
  labs(
    title   = "Age of Firstborn",
    x       = "Age",
    y       = "Density",
    caption = "Kernel density estimates of the age of first and second-born children across treatment
and control districts, estimated using an Epanechnikov kernel. Birth year was obtained from the DHS
birth history module (variable b2) and age was approximated using the survey interview year (v007)."
  ) +
  theme_minimal() +
  theme(
    plot.caption          = element_text(hjust = 0, size = 8, face = "italic"),
    plot.caption.position = "plot"
  )

ggsave(
  filename = "outputs/fig5_kernel_1.jpeg",
  plot     = p1,
  width    = 8,
  height   = 4.5,
  dpi      = 300,
  device   = "jpeg"
)

p2 <- ggplot(df_plot, aes(x = age_secondborn, color = district_type, fill = district_type)) +
  geom_density(alpha = 0.3, kernel = "epanechnikov") +
  labs(
    title   = "Age of Secondborn",
    x       = "Age",
    y       = "Density",
    caption = "Kernel density estimates of the age of first and second-born children across treatment
and control districts, estimated using an Epanechnikov kernel. Birth year was obtained from the DHS
birth history module (variable b2) and age was approximated using the survey interview year (v007)."
  ) +
  theme_minimal() +
  theme(
    plot.caption          = element_text(hjust = 0, size = 8, face = "italic"),
    plot.caption.position = "plot"
  )

theme_minimal()

ggsave(
  filename = "outputs/fig5_kernel_2.jpeg",
  plot     = p2,
  width    = 8,
  height   = 4.5,
  dpi      = 300,
  device   = "jpeg"
)
############################################# END ###################################################

