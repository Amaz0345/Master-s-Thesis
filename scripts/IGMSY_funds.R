## Funds reported and utilized
#table with states/Uts, followed by (Fund released in 2011-12 divided by Fund utilized reported in 2011-12)*100
library(dplyr)
library(ggplot2)

###################### Creating a workable dataframe ############################
IGMSY_funds <- read_excel("IGMSY_funds.xlsx")

names(IGMSY_funds) ##renaming the columns 
IGMSY_funds <- IGMSY_funds %>%
  rename(
    state        = "States/Uts",
    released_2010 = "Fund released in 2010-11",
    utilized_2010 = "Fund utilized in 2010-11",
    released_2011 = "Fund released in 2011-12",
    utilized_2011 = "Fund utilized reported in 2011-12",
    released_2012 = "Fund released in 2012-13",
    utilized_2012 = "Fund utilization reported in 2012-13",
    benefit_2010 = "Beneficiaries covered in 2010-11",
    benefit_2011 = "Beneficiaries covered in 2011-12",
    benefit_2012 = "Beneficiaries covered in 2012-13 (as on 31-12-2012)"
  ) %>%
  select(state, released_2010, utilized_2010, released_2011, utilized_2011,
         released_2012, utilized_2012, benefit_2010, benefit_2011, benefit_2012
         ) %>%
  mutate(across(-state, ~ as.numeric(gsub("[^0-9.]", "", .))))

panel_df <- IGMSY_funds %>% ##panel data structure 
  pivot_longer(
    cols = -state,
    names_to  = c(".value", "year"),
    names_pattern = "(released|utilized|benefit)_(\\d{4})"
  ) %>%
  mutate(
    year     = case_when(
      year == "2010" ~ "2010-11",
      year == "2011" ~ "2011-12",
      year == "2012" ~ "2012-13"
    ),
    released = as.numeric(released),
    utilized = as.numeric(utilized),
    pct_utilized = round((utilized / released) * 100, 1),
    benefit = as.numeric(benefit)
  )

########################### visualizing the data ################################
panel_df <- panel_df %>%
  group_by(state) %>%
  filter(!any(is.na(pct_utilized))) %>%
  ungroup()

fig1 <- ggplot(panel_df, aes(x = year, y = reorder(state, pct_utilized), fill = pct_utilized)) +
  geom_tile(colour = "white", linewidth = 0.5) +
  geom_text(aes(label = paste0(pct_utilized, "%")), size = 2.8, colour = "grey20") +
  scale_fill_gradientn(
    colours = c("#d73027", "#fee08b", "#1a9850"),
    values  = scales::rescale(c(0, 50, 100)),
    limits  = c(0, 100),
    name    = "% Utilized"
  ) +
  labs(
    title    = "Fund Utilization Rate by State/UT (2010–2013)",
    subtitle = "Percentage of released funds reported as utilized",
    x = NULL, y = NULL,
    caption  = "Source: IGMSY administrative data"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title       = element_text(face = "bold", size = 13),
    plot.subtitle    = element_text(colour = "grey40", size = 10),
    axis.text.y      = element_text(size = 8),
    axis.text.x      = element_text(size = 10),
    legend.position  = "right",
    panel.grid       = element_blank(),
    plot.background  = element_rect(fill = "white", colour = NA),
    plot.caption     = element_text(size = 9, colour = "grey50")
  )
ggsave(file.path(output_dir, "fig1_heatmap_utilization.png"),
       fig1, width = 10, height = 10, dpi = 300, bg = "white")
## Beneficiaries per year
# First calculate each state's share of total beneficiaries per year
IGMSY_funds <- IGMSY_funds %>%
  group_by(year) %>%
  mutate(pct_beneficiaries = round((benefit / sum(benefit, na.rm = TRUE)) * 100, 1)) %>%
  ungroup()

fig2a <- ggplot(panel_df, aes(x = year, y = pct_beneficiaries, fill = state)) +
  geom_bar(stat = "identity", colour = "white", linewidth = 0.3) +
  scale_y_continuous(labels = scales::percent_format(scale = 1),
                     expand = expansion(mult = c(0, 0.02))) +
  scale_fill_manual(values = colorRampPalette(
    c("#2166ac","#4dac26","#d01c8b","#f1b6da","#b8e186","#fdae61","#d7191c"))(35)) +
  labs(
    title    = "Share of Beneficiaries by State/UT (2010–2013)",
    subtitle = "Each state's share of total beneficiaries covered per year",
    x = NULL, y = "% of Total Beneficiaries",
    fill    = "State/UT",
    caption = "Source: IGMSY administrative data. Excluding States with NR (Not Reported)."
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title      = element_text(face = "bold", size = 13),
    plot.subtitle   = element_text(colour = "grey40", size = 10),
    legend.text     = element_text(size = 7),
    legend.key.size = unit(0.4, "cm"),
    panel.grid.major.x = element_blank(),
    plot.background = element_rect(fill = "white", colour = NA),
    plot.caption    = element_text(size = 9, colour = "grey50")
  )

ggsave(file.path(output_dir, "fig2a_stacked_beneficiaries.png"),
       fig2a, width = 10, height = 8, dpi = 300, bg = "white")


##Cleveland plot 
fig2b <- ggplot(panel_df, aes(x = pct_beneficiaries, y = reorder(state, pct_beneficiaries))) +
  geom_line(aes(group = state), colour = "grey70", linewidth = 0.5) +
  geom_point(aes(colour = year), size = 3) +
  scale_colour_manual(
    values = c("2010-11" = "#2166ac", "2011-12" = "#f4a582", "2012-13" = "#1a9850"),
    name   = "Year"
  ) +
  scale_x_continuous(labels = scales::percent_format(scale = 1)) +
  labs(
    title    = "Beneficiary Coverage by State/UT (2010–2013)",
    subtitle = "Each point shows a state's share of total beneficiaries in that year",
    x = "% of Total Beneficiaries", y = NULL,
    caption  = "Source: IGMSY administrative data"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title      = element_text(face = "bold", size = 13),
    plot.subtitle   = element_text(colour = "grey40", size = 10),
    axis.text.y     = element_text(size = 8),
    panel.grid.major.y = element_line(colour = "grey92"),
    panel.grid.major.x = element_line(colour = "grey92"),
    panel.grid.minor   = element_blank(),
    legend.position    = "top",
    plot.background    = element_rect(fill = "white", colour = NA),
    plot.caption       = element_text(size = 9, colour = "grey50")
  )

ggsave(file.path(output_dir, "fig2b_dotplot_beneficiaries.png"),
       fig2b, width = 10, height = 10, dpi = 300, bg = "white")




















