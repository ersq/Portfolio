library(tidyverse)
library(cars)
library(glmnet)

# Load files
life <- read.csv("Original_Life_Dataset.csv")
View(life)



# Clean data + rename columns
life = na.omit(life) |>
  filter(Year == 2014) |>
  select(-Year, -Country) |>
    rename(
      income_comp = Income.composition.of.resources,
      percent_expend = percentage.expenditure,
      status = Status,
      life_expect = Life.expectancy,
      a_mortal = Adult.Mortality,
      i_mortal = infant.deaths,
      alcohol = Alcohol,
      hepatitis = Hepatitis.B,
      measles = Measles,
      bmi = BMI,
      under5_mortal = under.five.deaths,
      polio = Polio,
      total_expend = Total.expenditure,
      diphtheria = Diphtheria,
      hiv = HIV.AIDS,
      gdp = GDP,
      pop = Population,
      thin1_19 = thinness..1.19.years,
      thin5_9 = thinness.5.9.years,
      schooling = Schooling
      )

# correlation heatmap
no_life_expect = life |>
  select(-life_expect)
life_matrix <- 
  cor(life[, sapply(life, is.numeric)], ) %>%
  as_tibble(rownames = 'v1') %>%
  pivot_longer(-v1, names_to = "v2", values_to = "corr")

heatmap_life <- 
  life_matrix %>%
  ggplot(aes(x = v1, y = v2)) +
  geom_tile(aes(fill = corr), color = "white") +
  scale_fill_distiller("Correlation \n",
                       palette =  "RdBu",
                       direction = 1, 
                       limits = c(-1, 1)
  ) +
  labs(x = "", y = "", title = "Heatmap for Numerical Variables") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, size = 13, hjust = 1),
    axis.text.y = element_text(vjust = 1, size = 13, hjust = 1),
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 11),
    legend.key.size = unit(1.3, "cm")
  ) +
  coord_fixed() +
  geom_text(aes(x = v1, y = v2, label = round(corr, 2)), color = "black", size = 3)

heatmap_life

# histogram of response
hist(life$life_expect, xlab = "Life Expectancy in Years", main = "Histogram of Life Expectancy")

# boxplot of status
boxplot_life = life |>
      ggplot() +
      geom_boxplot(aes(x = life$status, y = life$life_expect)) +
      labs(x = "Status of Country", 
           y = "Life Expectancy in Years", 
           title = "Relationship Between Status of Country and Life Expectancy")
boxplot_life

# cleaned dataset, no health factors
life_clean <- life %>%
  select(life_expect, status, total_expend, income_comp, gdp, percent_expend, schooling, pop)

# Fit a linear model and check linearity
org_model <- lm(life_expect ~ ., data = life_clean)
crPlots(org_model)

# Convert 'status' to factor
life_clean$status <- as.factor(life_clean$status)

# Log-transform skewed variables
life_transformed <- life_clean %>%
  mutate(
    log_gdp = log(gdp + 1),
    log_percent_expend = log(percent_expend + 1),
    log_pop = log(pop + 1)
  ) %>%
  select(-gdp, -percent_expend, -pop)

# fitted full model and characteristics of chosen model
full_model <- lm(life_expect ~ status+total_expend + income_comp + log_gdp +log_percent_expend + schooling + log_pop, data = life_transformed)
step_model <- step(full_model, direction = "both")
summary(step_model)

# residual plots for chosen model
plot(step_model, which = 1, main = "Residuals vs Fitted Values",sub = "", caption = NULL)
plot(step_model, which = 2, main = "Normal Q-Q plot",sub = "", caption = NULL)
vif(step_model)
AIC(step_model)
AIC(full_model)

# finding Cp of full model and selected model
ssres_step = sum(resid(step_model)**2)
msres_step = summary(full_model)$sigma**2
n = nrow(life_clean)
cp_step = ssres_step/msres_step - (n - 2*5)

ssres_full = sum(resid(full_model)**2)
msres_full = summary(full_model)$sigma**2
cp_full = ssres_full/msres_full - (n - 2*8)


