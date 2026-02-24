library(ggdag)
library(ggplot2)

# Define DAG for your CCT study with men's attitudes
cct_dag <- dagify(
  # Main causal pathways
  empowerment ~ cct + income + health + education + norms + men_attitudes,
  ipv ~ cct + empowerment + income + norms + poverty + men_attitudes,
  
  # Mediating pathways (how CCT works)
  income ~ cct,
  health ~ cct,
  education ~ cct,
  
  # Men's attitudes pathway
  men_attitudes ~ norms + cct,
  
  # What determines CCT rollout (confounders)
  cct ~ poverty + norms + infrastructure + politics,
  
  # Other relationships
  empowerment ~ health + education,
  
  # Define treatment and outcomes
  exposure = "cct",
  outcome = c("empowerment", "ipv"),
  
  # Labels for readability
  labels = c(
    cct = "CCT\nProgram",
    empowerment = "Women's\nEmpowerment",
    ipv = "Intimate\nPartner\nViolence",
    income = "Household\nIncome",
    health = "Health\nServices\nUse",
    education = "Children's\nEducation",
    poverty = "District\nPoverty",
    norms = "Gender\nNorms",
    men_attitudes = "Men's\nAttitudes",
    infrastructure = "Infrastructure",
    politics = "Political\nPriorities"
  ),
  
  # Coordinates for layout
  coords = list(
    x = c(cct = 1, income = 2, health = 2, education = 2, 
          empowerment = 3.5, ipv = 3.5, poverty = 0, norms = 0, 
          infrastructure = 0, politics = 0, men_attitudes = 2),
    y = c(cct = 0, income = 1.2, health = 0.4, education = -0.4,
          empowerment = 0.6, ipv = -0.6, poverty = 1.2, norms = 0.4,
          infrastructure = -0.8, politics = -1.6, men_attitudes = -1.2)
  )
)

# Plot with title
ggdag(cct_dag, text = FALSE, use_labels = "label") +
  theme_dag_gray_grid() +
  labs(title = "Visualising how a conditional cash transfer is affecting\nwomen's empowerment and intimate partner violence") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.margin = margin(10, 10, 10, 10)
  )

# Alternative: Show pathways from CCT to outcomes
ggdag_paths(cct_dag, 
            from = "cct", 
            to = c("empowerment", "ipv"),
            text = FALSE, 
            use_labels = "label") +
  theme_void() +
  labs(title = "Visualising how a conditional cash transfer is affecting\nwomen's empowerment and intimate partner violence") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.margin = margin(10, 10, 10, 10)
  )

# Save the plot
ggsave("cct_dag.png", width = 12, height = 8, dpi = 300)