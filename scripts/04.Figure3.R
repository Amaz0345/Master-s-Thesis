# ============================================================
#  Master Thesis: Fig. 3. Theoretical Pathways for Cash Transfers & IPV 
#  Author: Ananya Mazumder 
#  References: Heinemann & Rawal (2024); Buller et al. (2018);
#              Pereira et al. (2023)
# ============================================================
# Requires: ggplot2, ggtext
# install.packages(c("ggplot2", "ggtext"))
library(ggplot2)
library(ggtext)

# ── Canvas dimensions ────────────────────────────────────────
W <- 20   # width  (inches when saved)
H <- 14   # height (inches when saved)

# ── Colour palette ───────────────────────────────────────────
col_teal      <- "#2a7f6f"
col_teal_fill <- "#dff0ec"
col_red       <- "#a83232"
col_red_fill  <- "#f5e0e0"
col_mid_fill  <- "#f0f0f0"
col_mid_bord  <- "#999999"
col_top_fill  <- "#e8edf2"
col_top_bord  <- "#4a6080"
col_text      <- "#1a1a1a"
col_muted     <- "#555555" ##fun colours, didn't really think about this are they too much? 

# ── Helper: rectangle data frame ─────────────────────────────
rect <- function(xmin, xmax, ymin, ymax, fill, colour, label = NA) {
  data.frame(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
             fill = fill, colour = colour, label = label,
             stringsAsFactors = FALSE)
}

# ── Layout coordinates ───────────────────────────────────────
# Y: 0 (bottom) → 14 (top)
# Boxes (x centres): top=10, left=4.5, right=15.5

# TOP BOX  — Cash Transfer
top <- rect(6.5, 13.5, 11.6, 13.4, col_top_fill, col_top_bord)

# MIDDLE BOX — Bargaining power
mid <- rect(6.5, 13.5,  8.8, 11.0, col_mid_fill, col_mid_bord)

# PATHWAY A (left)
pa  <- rect(0.4,  9.2,  2.2,  7.6, col_teal_fill, col_teal)

# PATHWAY B (right)
pb  <- rect(10.8, 19.6, 2.2,  7.6, col_red_fill,  col_red)

# OUTCOME A
oa  <- rect(0.9,  8.7,  0.3,  1.6, col_teal_fill, col_teal)

# OUTCOME B
ob  <- rect(11.3, 19.1, 0.3,  1.6, col_red_fill,  col_red)

# ── Arrows ───────────────────────────────────────────────────
arrows_df <- data.frame(
  x    = c(10,   10,    4.8,  15.2),
  xend = c(10,    4.8,  4.8,  15.2),
  y    = c(11.8, 10.5,  8.8,  8.8),
  yend = c(11.0, 10.5,  7.6,  7.6),
  col  = c(col_mid_bord, col_mid_bord, col_teal, col_red)
)

# horizontal fork line
fork_df <- data.frame(
  x = 4.8, xend = 15.2, y = 10.5, yend = 10.5,
  col = col_mid_bord
)

# outcome arrows
out_arrows <- data.frame(
  x    = c(4.8,  15.2),
  xend = c(4.8,  15.2),
  y    = c(2.2,  2.2),
  yend = c(1.6,  1.6),
  col  = c(col_teal, col_red)
)

# ── Build plot ───────────────────────────────────────────────
p <- ggplot() +
  
  # ── Rectangles ──────────────────────────────────────────
  # top box
  geom_rect(data = top, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
            fill=col_top_fill, colour=col_top_bord, linewidth=0.8) +
  # mid box
  geom_rect(data = mid, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
            fill=col_mid_fill, colour=col_mid_bord, linewidth=0.7) +
  # pathway boxes
  geom_rect(data = pa, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
            fill=col_teal_fill, colour=col_teal, linewidth=0.9) +
  geom_rect(data = pb, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
            fill=col_red_fill, colour=col_red, linewidth=0.9) +
  # outcome boxes
  geom_rect(data = oa, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
            fill=col_teal_fill, colour=col_teal, linewidth=1.0) +
  geom_rect(data = ob, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
            fill=col_red_fill, colour=col_red, linewidth=1.0) +
  
  # ── Arrows ──────────────────────────────────────────────
  # vertical top → mid
  geom_segment(aes(x=10, xend=10, y=11.8, yend=11.0),
               arrow=arrow(length=unit(0.25,"cm"), type="closed"),
               colour=col_mid_bord, linewidth=0.7) +
  # vertical mid → fork
  geom_segment(aes(x=10, xend=10, y=11.0, yend=10.5),
               colour=col_mid_bord, linewidth=0.7) +
  # horizontal fork
  geom_segment(aes(x=4.8, xend=15.2, y=10.5, yend=10.5),
               colour=col_mid_bord, linewidth=0.7) +
  # left branch down
  geom_segment(aes(x=4.8, xend=4.8, y=10.5, yend=7.6),
               arrow=arrow(length=unit(0.25,"cm"), type="closed"),
               colour=col_teal, linewidth=0.7) +
  # right branch down
  geom_segment(aes(x=15.2, xend=15.2, y=10.5, yend=7.6),
               arrow=arrow(length=unit(0.25,"cm"), type="closed"),
               colour=col_red, linewidth=0.7) +
  # outcome arrows
  geom_segment(aes(x=4.8, xend=4.8, y=2.2, yend=1.6),
               arrow=arrow(length=unit(0.25,"cm"), type="closed"),
               colour=col_teal, linewidth=0.8) +
  geom_segment(aes(x=15.2, xend=15.2, y=2.2, yend=1.6),
               arrow=arrow(length=unit(0.25,"cm"), type="closed"),
               colour=col_red, linewidth=0.8) +
  
  # ── Labels: TOP BOX ─────────────────────────────────────
  annotate("text", x=10, y=12.75, label="INTERVENTION",
           size=6.5, colour=col_top_bord, fontface="bold",
           family="sans", hjust=0.5, vjust=0.5, letter_spacing=2) +
  annotate("text", x=10, y=12.25, label="Cash Transfer Programme",
           size=7.5, colour=col_text, fontface="bold",
           family="sans", hjust=0.5, vjust=0.5) +
  annotate("text", x=10, y=11.85,
           label="Direct financial transfer to women in the household",
           size=5.5, colour=col_muted, family="sans", hjust=0.5, vjust=0.5) +
  
  # ── Labels: MID BOX ─────────────────────────────────────
  annotate("text", x=10, y=10.25,
           label="Women's Bargaining Power Increases",
           size=7.0, colour=col_text, fontface="bold",
           family="sans", hjust=0.5, vjust=0.5) +
  annotate("text", x=10, y=9.7,
           label="Improved economic conditions strengthen women's\nnegotiating position within the household",
           size=6.0, colour=col_muted, family="sans", hjust=0.5, vjust=0.5) +
  
  # ── Pathway labels on fork ───────────────────────────────
  annotate("text", x=4.8,  y=10.75, label="PATHWAY A",
           size=5.5, colour=col_teal, fontface="bold", family="sans") +
  annotate("text", x=15.2, y=10.75, label="PATHWAY B",
           size=5.5, colour=col_red,  fontface="bold", family="sans") +
  
  # ── PATHWAY A box text ───────────────────────────────────
  annotate("text", x=4.8, y=7.25,
           label="Acceptance & Adaptation",
           size=7.5, colour=col_teal, fontface="bold",
           family="sans", hjust=0.5) +
  annotate("text", x=4.8, y=6.65,
           label="Male Acceptance Mechanism",
           size=5.5, colour=col_teal, family="sans", hjust=0.5,
           fontface="italic") +
  annotate("text", x=4.8, y=5.85,
           label="Women assert greater agency as economic\ndependence on male partner decreases.",
           size=6.5, colour=col_muted, family="sans", hjust=0.5) +
  annotate("text", x=4.8, y=4.85,
           label="Men adapt to the shift in household dynamics,\naccepting rebalancing of power without\nperceiving it as a threat to their status.",
           size=6.5, colour=col_muted, family="sans", hjust=0.5) +
  annotate("text", x=4.8, y=3.65,
           label="Reduced financial stress diminishes\nconflict triggers; household cooperation\nimproves.",
           size=6.5, colour=col_muted, family="sans", hjust=0.5) +
  
  # ── PATHWAY B box text ───────────────────────────────────
  annotate("text", x=15.2, y=7.25,
           label="Resistance & Reprisal",
           size=7.5, colour=col_red, fontface="bold",
           family="sans", hjust=0.5) +
  annotate("text", x=15.2, y=6.65,
           label="Male Backlash Mechanism",
           size=5.5, colour=col_red, family="sans", hjust=0.5,
           fontface="italic") +
  annotate("text", x=15.2, y=5.85,
           label="Women attempt to assert agency and\nrenegotiate power, challenging existing\ngender norms.",
           size=6.5, colour=col_muted, family="sans", hjust=0.5) +
  annotate("text", x=15.2, y=4.75,
           label="Men perceive the shift as a threat to\nmasculine identity and household authority,\nresponding with resistance and coercive control.",
           size=6.5, colour=col_muted, family="sans", hjust=0.5) +
  annotate("text", x=15.2, y=3.65,
           label="Violence is used instrumentally to\nreassert dominance and punish women\nfor challenging the power hierarchy.",
           size=6.5, colour=col_muted, family="sans", hjust=0.5) +
  
  # ── OUTCOME A ────────────────────────────────────────────
  annotate("text", x=4.8, y=1.1,
           label="IPV DECREASES",
           size=7.0, colour=col_teal, fontface="bold",
           family="sans", hjust=0.5) +
  annotate("text", x=4.8, y=0.6,
           label="Economic empowerment → reduced violence",
           size=5.5, colour=col_teal, family="sans", hjust=0.5) +
  
  # ── OUTCOME B ────────────────────────────────────────────
  annotate("text", x=15.2, y=1.1,
           label="IPV INCREASES",
           size=7.0, colour=col_red, fontface="bold",
           family="sans", hjust=0.5) +
  annotate("text", x=15.2, y=0.6,
           label="Backlash effect — women face reprisals for empowerment",
           size=5.5, colour=col_red, family="sans", hjust=0.5) +
  
  # ── Caption / references ─────────────────────────────────
  labs(
    caption = paste0(
      "Theoretical framework drawing on: Heinemann & Rawal (2024); ",
      "Buller et al. (2018); Pereira et al. (2023).\n",
      "Note: The direction of the relationship between cash transfers and IPV remains empirically contested ",
      "and may vary by programme design, cultural context, and complementary interventions."
    )
  ) +
  
  # ── Theme ────────────────────────────────────────────────
  theme_void() +
  theme(
    plot.background  = element_rect(fill = "white", colour = NA),
    panel.background = element_rect(fill = "white", colour = NA),
    plot.caption     = element_text(size = 12, colour = "#777777",
                                    hjust = 0.5, margin = margin(t=10),
                                    family = "sans"),
    plot.margin      = margin(12, 12, 12, 12)
  ) +
  
  coord_fixed(ratio = 1, xlim = c(0, 20), ylim = c(0, 13.5), expand = FALSE)

# ── Save ─────────────────────────────────────────────────────
ggsave(
  filename = file.path(output_dir, "fig.3.png"),  
  plot     = p,
  width    = W,
  height   = H,
  dpi      = 500,
  bg       = "white"
)

message("Saved: fig.3.png") ## used AI a lot for this one tbh
############################################# END ###################################################
