library(flextable)
library(dplyr)
```{r}
library(knitr)
library(kableExtra)

districts_data %>%
  kable(
    col.names = c("Development Level", "Pilot District", "State", 
                  "Control District", "State"),
    caption = "District Selection for IGMSY Pilot Program"
  ) %>%
  kable_styling(bootstrap_options = c("striped", "hover"))
```
# Create your dataset (cleaned up)
districts_data <- tibble(
  development_level = c(
    rep("Low", 10),
    rep("Middle", 19),
    rep("High", 7)
  ),
  pilot_district = c(
    # Low
    "Saharsa", "Simdega", "Tamenglong", "Vaishali", "Mahoba", 
    "Sultanpur", "East Garo Hills", "Bhilwara", "Udaipur", "Dhalai",
    # Middle
    "Bastar", "Goalpara", "Lawngtlai", "Jalpaiguri", "Bankura",
    "Papumpare", "Dehradun", "Purbi Singhbhum", "West District", 
    "North West Delhi", "Dhamtari", "Panchkula", "Patan", "Kapurthala",
    "Kamrup", "West Delhi", "Bharuch", "Nalgonda", "YSR",
    # High
    "Dharwad", "Amritsar", "Kolar", "West Godavari", "Hamirpur", 
    "Palakkad", "North Goa"
  ),
  pilot_state = c(
    # Low
    "Bihar", "Jharkhand", "Manipur", "Bihar", "UP",
    "UP", "Meghalaya", "Rajasthan", "Rajasthan", "Tripura",
    # Middle
    "Chhattisgarh", "Assam", "Mizoram", "West Bengal", "West Bengal",
    "Arunachal Pradesh", "Uttarakhand", "Jharkhand", "Sikkim",
    "Delhi", "Chhattisgarh", "Haryana", "Gujarat", "Punjab",
    "Assam", "Delhi", "Gujarat", "Telengana", "Andhra Pradesh",
    # High
    "Karnataka", "Punjab", "Karnataka", "Andhra Pradesh", "Himachal Pradesh", 
    "Kerala", "Goa"
  ),
  control_district = c(
    # Low
    "Katihar", "Godda", "Muzaffarnagar", "Saran", "Azamgarh",
    "West Garo Hills", "Banswara", "Ukhrul", "Bilaspur", "",
    # Middle
    "Tonk", "Dhemaji", "Ranchi", "North Tripura", "Changlang",
    "Chamoli", "Puruliya", "Durg", "Mamit", "Dakshin Dinajpur",
    "South Delhi", "South District", "Rewari", "Fatehgarh Sahib",
    "Valsad", "Dibrugarh", "Kheda", "East Delhi", "",
    # High
    "Davangere", "Bilaspur", "Muktsar", "Rangareddy", "Tumkur", 
    "Kozhikode", "South Goa"
  ),
  control_state = c(
    # Low
    "Bihar", "Jharkhand", "Uttar Pradesh", "Bihar", "Uttar Pradesh",
    "Meghalaya", "Rajasthan", "Manipur", "Chhattisgarh", "",
    # Middle
    "Rajasthan", "Assam", "Jharkhand", "Tripura", "Arunachal Pradesh",
    "Uttarakhand", "West Bengal", "Chhattisgarh", "Mizoram", "West Bengal",
    "Delhi", "Sikkim", "Haryana", "Punjab", "Gujarat",
    "Assam", "Gujarat", "Delhi", "",
    # High
    "Karnataka", "Himachal Pradesh", "Punjab", "Telengana", "Karnataka", 
    "Kerala", "Goa"
  )
)
districts_table <- districts_data %>%
  flextable() %>%
  set_caption("District Selection for IGMSY Pilot Program by Development Level") %>%
  set_header_labels(
    development_level = "Development Level",
    pilot_district = "District",
    pilot_state = "State",
    control_district = "District",
    control_state = "State"
  ) %>%
  add_header_row(
    values = c("", "Pilot", "Control"),
    colwidths = c(1, 2, 2)
  ) %>%
  theme_booktabs() %>%
  autofit() %>%
  align(align = "left", part = "all")

districts_table
