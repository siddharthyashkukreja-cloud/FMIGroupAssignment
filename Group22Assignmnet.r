# Financial Markets and Institutions
# 
# Purpose:
#     Main analysis file for Group Home Assignment
# 
# Date:
#     2025/11
# 
# Author:
#     Group 22 - S.Y. Kukreja, M.A. van Leeuwen, K.L. Veenenbos, D.P.H. Kouwenhoven

# Auto-install missing packages
required_packages <- c("tidyverse", "lubridate", "PerformanceAnalytics", 
                       "lmtest", "sandwich", "plm", "broom", "stargazer")
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

# Load packages
library(tidyverse)
library(lubridate)
library(PerformanceAnalytics)
library(lmtest)
library(sandwich)
library(plm)
library(broom)
library(stargazer)

# Import datasets from GitHub
base_url <- "https://raw.githubusercontent.com/siddharthyashkukreja-cloud/FMIGroupAssignment/ec99007c2669ef90e49b597bc6d98dbab5115559/01_Data/"

mtsfix <- read_csv(paste0(base_url, "01_mtsfix.csv"), show_col_types = FALSE)
prices <- read_csv(paste0(base_url, "02_prices.csv"), show_col_types = FALSE)
fx <- read_csv(paste0(base_url, "03_fx.csv"), show_col_types = FALSE)
banks <- read_csv(paste0(base_url, "04_banks.csv"), show_col_types = FALSE)
rates <- read_csv(paste0(base_url, "04_rates.csv"), show_col_types = FALSE)
funds <- read_csv(paste0(base_url, "05_funds.csv"), show_col_types = FALSE)

# ============================================================
# Week 1: Exploring DSL (Dutch Government Bonds) Data
# ============================================================

# Convert date columns to proper format
mtsfix <- mtsfix %>%
  mutate(
    RefDate = dmy(RefDate),
    Maturity = mdy(Maturity)  # Changed from dmy to mdy
  )

# Extract coupon rate and years to maturity
mtsfix <- mtsfix %>%
  mutate(
    Coupon = as.numeric(str_extract(Description, "(?<=NETHER\\s)\\d+\\.?\\d*")),
    Years_to_Maturity = as.numeric(year(Maturity) - year(RefDate))
  )

# Filter data for key years (2010, 2015, 2020, 2025)
key_years <- mtsfix %>%
  filter(year(RefDate) %in% c(2010, 2015, 2020, 2025))

# Quick visualization of bond prices by maturity
# Financial Markets and Institutions
# 
# Purpose:
#     Main analysis file for Group Home Assignment
# 
# Date:
#     2025/11
# 
# Author:
#     Group 22 - S.Y. Kukreja, M.A. van Leeuwen, K.L. Veenenbos, D.P.H. Kouwenhoven

# Auto-install missing packages
required_packages <- c("tidyverse", "lubridate", "PerformanceAnalytics", 
                       "lmtest", "sandwich", "plm", "broom", "stargazer")
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

# Load packages
library(tidyverse)
library(lubridate)
library(PerformanceAnalytics)
library(lmtest)
library(sandwich)
library(plm)
library(broom)
library(stargazer)

# Import datasets from GitHub
base_url <- "https://raw.githubusercontent.com/siddharthyashkukreja-cloud/FMIGroupAssignment/ec99007c2669ef90e49b597bc6d98dbab5115559/01_Data/"

mtsfix <- read_csv(paste0(base_url, "01_mtsfix.csv"), show_col_types = FALSE)
prices <- read_csv(paste0(base_url, "02_prices.csv"), show_col_types = FALSE)
fx <- read_csv(paste0(base_url, "03_fx.csv"), show_col_types = FALSE)
banks <- read_csv(paste0(base_url, "04_banks.csv"), show_col_types = FALSE)
rates <- read_csv(paste0(base_url, "04_rates.csv"), show_col_types = FALSE)
funds <- read_csv(paste0(base_url, "05_funds.csv"), show_col_types = FALSE)

# ============================================================
# Week 1: Exploring DSL (Dutch Government Bonds) Data
# ============================================================

# Convert date columns to proper format
mtsfix <- mtsfix %>%
  mutate(
    RefDate = dmy(RefDate),
    Maturity = mdy(Maturity)  # Changed from dmy to mdy
  )

# Extract coupon rate and years to maturity
mtsfix <- mtsfix %>%
  mutate(
    Coupon = as.numeric(str_extract(Description, "(?<=NETHER\\s)\\d+\\.?\\d*")),
    Years_to_Maturity = as.numeric(year(Maturity) - year(RefDate))
  )

# Filter data for key years (2010, 2015, 2020, 2025)
key_years <- mtsfix %>%
  filter(year(RefDate) %in% c(2010, 2015, 2020, 2025))

# Financial Markets and Institutions
# 
# Purpose:
#     Main analysis file for Group Home Assignment
# 
# Date:
#     2025/11
# 
# Author:
#     Group 22 - S.Y. Kukreja, M.A. van Leeuwen, K.L. Veenenbos, D.P.H. Kouwenhoven

# Auto-install missing packages
required_packages <- c("tidyverse", "lubridate", "PerformanceAnalytics", 
                       "lmtest", "sandwich", "plm", "broom", "stargazer")
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

# Load packages
library(tidyverse)
library(lubridate)
library(PerformanceAnalytics)
library(lmtest)
library(sandwich)
library(plm)
library(broom)
library(stargazer)

# Import datasets from GitHub
base_url <- "https://raw.githubusercontent.com/siddharthyashkukreja-cloud/FMIGroupAssignment/ec99007c2669ef90e49b597bc6d98dbab5115559/01_Data/"

mtsfix <- read_csv(paste0(base_url, "01_mtsfix.csv"), show_col_types = FALSE)
prices <- read_csv(paste0(base_url, "02_prices.csv"), show_col_types = FALSE)
fx <- read_csv(paste0(base_url, "03_fx.csv"), show_col_types = FALSE)
banks <- read_csv(paste0(base_url, "04_banks.csv"), show_col_types = FALSE)
rates <- read_csv(paste0(base_url, "04_rates.csv"), show_col_types = FALSE)
funds <- read_csv(paste0(base_url, "05_funds.csv"), show_col_types = FALSE)

# ============================================================
# Week 1: Exploring DSL (Dutch Government Bonds) Data
# ============================================================

# Convert date columns to proper format
mtsfix <- mtsfix %>%
  mutate(
    RefDate = dmy(RefDate),
    Maturity = mdy(Maturity)  # Changed from dmy to mdy
  )

# Extract coupon rate and years to maturity
mtsfix <- mtsfix %>%
  mutate(
    Coupon = as.numeric(str_extract(Description, "(?<=NETHER\\s)\\d+\\.?\\d*")),
    Years_to_Maturity = as.numeric(year(Maturity) - year(RefDate))
  )

# Filter data for key years (2010, 2015, 2020, 2025)
key_years <- mtsfix %>%
  filter(year(RefDate) %in% c(2010, 2015, 2020, 2025))

# Quick visualization of bond prices by maturity
p <- ggplot(key_years, aes(x = Years_to_Maturity, y = `Mid Price`, color = as.factor(year(RefDate)))) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(
    title = "DSL Bond Prices by Maturity",
    x = "Years to Maturity",
    y = "Mid Price",
    color = "Year"
  ) +
  theme_minimal()

print(p)

# ============================================================
# Question 1a: Zero-Coupon Yield Curve
# ============================================================

# Function to calculate zero-coupon yield from bond price
calculate_zcyield <- function(price, coupon, maturity_years, known_yields = NULL) {
  # For zero-coupon bonds (coupon = 0), direct calculation
  if (coupon == 0) {
    return((100 / price)^(1 / maturity_years) - 1)
  }
  
  # For coupon bonds, need to strip out the zero-coupon yield
  # Bond price = sum of discounted cash flows
  # Need to use bootstrapping with previously calculated yields
  
  if (is.null(known_yields) || maturity_years == 1) {
    # For 1-year bonds or when no previous yields known, solve directly
    # Price = (100 + coupon) / (1 + yield)
    return((100 + coupon) / price - 1)
  }
  
  # For longer maturity coupon bonds, subtract PV of known coupons
  pv_coupons <- 0
  for (t in 1:(maturity_years - 1)) {
    if (t <= length(known_yields)) {
      pv_coupons <- pv_coupons + coupon / (1 + known_yields[t])^t
    }
  }
  
  # Remaining value must equal final coupon + principal
  remaining_pv <- price - pv_coupons
  final_payment <- 100 + coupon
  
  # Solve for yield: remaining_pv = final_payment / (1 + yield)^maturity
  yield <- (final_payment / remaining_pv)^(1 / maturity_years) - 1
  
  return(yield)
}

# Function to bootstrap yield curve for a given year
bootstrap_yields <- function(data, ref_year) {
  # Filter data for the reference year
  year_data <- data %>%
    filter(year(RefDate) == ref_year) %>%
    arrange(Years_to_Maturity)
  
  # Initialize results
  yields <- numeric(10)
  
  # Bootstrap yields for each maturity
  for (mat in 1:10) {
    bond <- year_data %>% filter(Years_to_Maturity == mat)
    
    if (nrow(bond) == 0) {
      yields[mat] <- NA
      next
    }
    
    # Get known yields for shorter maturities
    known_yields <- if (mat > 1) yields[1:(mat-1)] else NULL
    
    # Calculate yield
    yields[mat] <- calculate_zcyield(
      price = bond$`Mid Price`,
      coupon = bond$Coupon,
      maturity_years = mat,
      known_yields = known_yields
    )
  }
  
  return(yields)
}

# Calculate yields for key years
years_to_plot <- c(2010, 2015, 2020, 2025)
yield_curves <- data.frame(Maturity = 1:10)

for (yr in years_to_plot) {
  yields <- bootstrap_yields(mtsfix, yr)
  yield_curves[[as.character(yr)]] <- yields * 100  # Convert to percentage
}

# Print the yield curves
print("Zero-Coupon Yield Curves (in %):")
print(yield_curves)

# Reshape for plotting
yield_curves_long <- yield_curves %>%
  pivot_longer(
    cols = -Maturity,
    names_to = "Year",
    values_to = "Yield"
  )

# Plot the yield curves
p1a <- ggplot(yield_curves_long, aes(x = Maturity, y = Yield, color = Year)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  labs(
    title = "Zero-Coupon Yield Curves for Dutch Government Bonds",
    subtitle = "Annual Compounding, as of July 15",
    x = "Maturity (Years)",
    y = "Zero-Coupon Yield (%)",
    color = "Year"
  ) +
  scale_x_continuous(breaks = 1:10) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "right"
  )

print(p1a)

# Save the plot
ggsave("Q1a_yield_curves.png", p1a, width = 10, height = 6, dpi = 300)

