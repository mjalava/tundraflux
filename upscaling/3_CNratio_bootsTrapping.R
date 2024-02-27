# calculating the relationship between C:N ratio and SOC

# plotting the convex hull for the measured C:N ratio and TN (measured vs gridded)

# code for figures: SI Fig 8, SI Fig 9

# contact: matti.kummu@aalto.fi



library(tidyr)
library(dplyr)
library(sf)
library(readxl)
library(ggpubr)
library(geometry)

# set working directory as the path where this script is located
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Read the CSV file and select specific columns
dfCO2 <- read.csv("data_in/df_2.csv") %>%
  as_tibble() %>%
  select(c(SOC_Min_CTL, SON_Min_CTL, CN_Min_CTL)) %>%
  mutate(CN_calc = SOC_Min_CTL / SON_Min_CTL) %>%
  drop_na() %>%
  distinct(.keep_all = TRUE)

# Fit a linear regression model
cnCorr <- lm(CN_Min_CTL ~ 0 + CN_calc, data = dfCO2)
summary(cnCorr)

# Predict values using the linear regression model
predict(cnCorr, interval = "confidence")

# Perform bootstrap sampling
N <- 100
seed <- 21
boots <- sapply(1:N, function(x) {
  mod <- try(lm(CN_Min_CTL ~ 0 + CN_calc, data = dfCO2[sample(1:nrow(dfCO2), size = nrow(dfCO2), replace = TRUE),]), TRUE)
  if (class(mod) == "try-error") return(NA)
  return(coef(mod))
})

# Plot the histogram of bootstrap sampling results
hist(boots)

# Plot the observed values and the regression lines from bootstrap sampling
plot(CN_Min_CTL ~ CN_calc, dfCO2)
invisible(sapply(boots, function(boot_k) curve(boot_k * x, lty = 2, add = TRUE, col = "#DD000020")))
curve(coef(cnCorr) * x, lty = 1, lwd = 2, add = TRUE)

# Save the bootstrap results
save(boots, file = 'data_out/CNboots.rdata')

# Plot the validation scatterplot
validation <- ggscatter(dfCO2, x = "CN_Min_CTL", y = "CN_calc",
                        add = "reg.line", conf.int = TRUE,
                        cor.coef = TRUE, cor.method = "pearson",
                        xlab = "Observed Value", ylab = "Downscaled Value") +
  geom_abline(intercept = 0, slope = 1) +
  xlim(0, 35) + ylim(0, 35)
validation

# Calculate the correlation between C and N
c_nCorr <- cor(dfCO2$SOC_Min_CTL, dfCO2$SON_Min_CTL)

# Plot the scatterplot between C and N
ggscatter(dfCO2, x = "SOC_Min_CTL", y = "SON_Min_CTL",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "C", ylab = "N")

# Read the gridded TN and SOC values
gridTN <- readRDS('results/v_TN.rds')
gridSOC <- readRDS('results/v_SOC.rds')

# Combine the gridded TN and SOC values and calculate CN ratio
gridTN_SOC <- gridTN %>%
  select(mean) %>%
  rename(TN = mean) %>%
  bind_cols(gridSOC) %>%
  select(-sd) %>%
  rename(SOC = mean) %>%
  as_tibble() %>%
  mutate(CN = SOC / TN) %>%
  sample_frac(0.001)

# Plot the scatterplot of TN and CN values
p_CN_SOC <- ggplot() +
  geom_point(data = gridTN_SOC, aes(TN, CN),
             colour = "blue",
             size = 2, stroke = 0,
             alpha = 0.05) +
  geom_point(data = dfCO2, aes(SON_Min_CTL, CN_calc),
             colour = "brown",
             size = 2, stroke = 0) +
  labs(x = "TN [cg g-1]", y = "C:N-ratio [-]") +
  ggtitle("Combined Plot") +
  theme_minimal()

# Create convex hull
x1 <- dfCO2$SON_Min_CTL
y1 <- dfCO2$CN_calc
xdf <- data_frame(x1, y1)
ConVexHull <- convhulln(cbind(dfCO2$SON_Min_CTL, dfCO2$CN_calc), 'FA')
h_df <- data.frame(
  do.call(
    rbind,
    lapply(1:nrow(ConVexHull$hull), function(i) {
      rbind(xdf[ConVexHull$hull[i, 1], ], xdf[ConVexHull$hull[i, 2], ])
    })
  )
)

# Plot the convex hull
ggplot() +
  geom_point(data = xdf, aes(x1, y1), color = "red") +
  geom_point(data = h_df, aes(x1, y1), shape = 21, fill = NA, color = "black", size = 3)

# Sort the points in the convex hull in clockwise order
h_df <- h_df[order(-1 * atan2(h_df$y1 - mean(range(h_df$y1)), h_df$x1 - mean(range(h_df$x1)))), ]
h_df <- rbind(h_df, h_df[1, ])

# Set ylim and xlim
ylim <- 40
xlim <- 1

# Plot the scatterplot with convex hull
p_CN_SOC_hull <- p_CN_SOC +
  geom_point(data = xdf, aes(x1, y1), color = "red") +
  geom_point(data = h_df, aes(x1, y1), shape = 21, fill = NA, color = "black", size = 3) +
  geom_path(data = h_df, aes(x1, y1), color = "blue") +
  coord_cartesian(ylim = c(0, ylim), xlim = c(0, xlim))

ggsave(plot = p_CN_SOC_hull, filename = "figures/SOC_CN_scatter_hull.pdf")

# Count the number of points inside the convex hull
p_grid <- cbind(gridTN_SOC$TN, gridTN_SOC$CN)
inHullTest <- geometry::inhulln(ConVexHull, p_grid)
# inHullTest2 <- geometry::inhulln(h_df, p_grid)
inHullCount <- sum(inHullTest, na.rm = TRUE)

# Calculate the percentage of points inside the convex hull
inHullCount / nrow(p_grid)

