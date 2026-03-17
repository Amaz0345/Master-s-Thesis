# ============================================================
#  Master Thesis: Fig. 2 
#  Author: Ananya Mazumder 
#  Export ggplot
#  Note: States reporting NA are removed 
# ============================================================

# ── Libraries ────────────────────────────────────────────────

library(readxl)
library(dplyr)
library(ggplot2)

# ── Creating a workable Dataframe ────────────────────────────────────────────────
IGMSY_funds <- read_excel("data/IGMSY_funds.xlsx") ## In case you are wondering where this data came from: I accessed the pdf for the data on fund utilization and then made an excel from it which I use here.  
## Link to the pdf here: https://www.pib.gov.in/newsite/PrintRelease.aspx?relid=92842&reg=3&lang=2
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
  mutate(across(-state, ~ as.numeric(gsub("[^0-9.]", "", .)))) ##renaming columns because I did not do due diligence to the excel names

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

# ── Creating the heatmap ────────────────────────────────────────────────
panel_df <- panel_df %>%
  group_by(state) %>%
  filter(!any(is.na(pct_utilized))) %>%
  ungroup()

fig2 <- ggplot(panel_df, aes(x = year, y = reorder(state, pct_utilized), fill = pct_utilized)) +
  geom_tile(colour = "white", linewidth = 0.5) +
  geom_text(aes(label = paste0(pct_utilized, "%")), size = 2.8, colour = "grey20") + ## i was told the font size was too small so i increased it 
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
    caption  = "Source: IGMSY administrative data, released by the Press Information Bureau, 01.03.2013"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title       = element_text(face = "bold", size = 13),
    plot.subtitle    = element_text(colour = "grey40", size = 10),
    axis.text.y      = element_text(size = 8),
    axis.text.x      = element_text(size = 10),
    legend.position  = "right",
    panel.grid       = element_blank(),
    plot.background  = element_rect(fill = "white", colour = NA),
    plot.caption     = element_text(size = 11, colour = "grey50") ##are the fonts too small???
  )
ggsave(file.path(output_dir, "fig2.png"), width = 8, height = 5, dpi = 300)
############################################# END ###################################################

















