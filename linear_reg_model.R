marketing <- read_csv("WA_Fn-UseC_-Marketing-Campaign-Eff-UseC_-FastF.csv", 
                      col_types = cols(MarketID = col_skip()))

library(simpleSetup)

# Load packages, install if needed
packages <- c('tidyverse',
              'readr',
              'skimr',
              "GGally",
              "stringr",
              "DataExplorer",
              "summarytools",
              "recipes",
              "magrittr",
              "corrr",
              "h2o",
              "rsample",
              "caret",
              "fs",
              "viridis",
              "tidyquant",
              "caret",
              "parsnip", 
              "cowplot")
library_install(packages)


set.seed(3456)
split <- rsample::initial_split(marketing_trnformed, prop = 0.7)

split %>% training()

split %>% testing() 

train <- training(split)
test  <- testing(split)



# Feature engineering
# 1. week can be categorized into 4, as categorical data
# 2. promotion turns into 3 categories too
# 3. how many locationID are there: 137 stores





marketing %>% glimpse() %>% skim()

marketing_trnformed <- marketing %>%
  mutate(
    Promotion = as.character(Promotion),
    Promotion_factor = as_factor(Promotion),
    week = as.character(week),
    Week_factor = as_factor(week)
  ) %>% 
  select(-c("week", "Promotion"))

marketing_trnformed %>% 
  select(-c("LocationID", "AgeOfStore")) 


# plot violin
marketing_trnformed %>% 
  select(Promotion_factor, Week_factor, MarketSize, SalesInThousands) %>% 
  distinct() %>% 
  group_by(Promotion_factor, SalesInThousands, MarketSize, Week_factor) %>% 
  summarise(sales = sum(SalesInThousands)) %>%
  ungroup() %>% 
  select(-SalesInThousands) %>% 
  gather(x, y, Promotion_factor:Week_factor) %>% 
  ggplot(aes(x = y, y = sales)) +
  facet_wrap(~ x, ncol = 1, nrow = 3, scales = "free") + 
  geom_violin(aes(x = y, fill = sales), alpha = 0.5) +
  coord_flip() +
  geom_jitter(width = 0.1, alpha = 0.5, color = "#2c3e50") +
  ylab("Sales in Thousands") + xlab(NULL) +
  theme_bw() <- promotion_plot

#continuous variables: ageinstores and locationID

num_vars_plot <- marketing_trnformed %>% 
  select(AgeOfStore, SalesInThousands) %>%
  gather(x, y, AgeOfStore:SalesInThousands) %>% 
  ggplot(aes(x = y)) +
  facet_wrap(~ x, ncol = 2, nrow = 1, scales = "free") + 
  geom_density(color = "skyblue", fill = "skyblue", alpha = 0.5) +
  xlab("Sales In Thousands") + ylab(NULL) +
  theme_bw()
  

marketing_trnformed %>% glimpse() %>% skim() 

market

recipe <- recipe(SalesInThousands ~ Promotion_factor + MarketSize, data = train) %>%
  step_string2factor(MarketSize) %>%
  step_dummy(MarketSize, Promotion_factor, one_hot = TRUE) %>% 
  #step_range(AgeOfStore) %>% 
  #step_discretize(AgeOfStore, options = list(min_unique = 1)) %>% 
  prep(data = train)

recipe


train_recipe <- bake(recipe, new_data = train)
test_recipe  <- bake(recipe, new_data = test)


# 3.1 LINEAR REGRESSION - NO ENGINEERED FEATURES ----

# 3.1.1 Model ----
?lm

linearModel <- linear_reg(mode = "regression") %>%
  set_engine("lm") %>%
  fit(SalesInThousands ~ ., data = train_recipe)

linearModel <- linear_reg(mode = "regression") %>%
  set_engine("lm") %>%
  fit(SalesInThousands ~ Promotion_factor_X1 +
                         Promotion_factor_X3 + 
                         Promotion_factor_X2 +
                         MarketSize_Large +
                         MarketSize_Medium +
                         MarketSize_Small, data = train_recipe)

linearModel %>% 
  predict(new_data = test_recipe) %>%
  bind_cols(test_recipe %>% select(SalesInThousands)) %>%
  yardstick::metrics(truth = SalesInThousands, estimate = .pred) 



## .metric .estimator .estimate
##  rmse    standard   10.5  
##  rsq     standard   0.579
##  mae     standard   8.69 

# 3.1.2 Feature Importance ----
linearModel

linearModel$fit %>% class()

linearModel$fit %>%
  broom::tidy() %>%
  arrange(p.value) %>%
  mutate(term = as_factor(term) %>% fct_rev()) %>%
  
  ggplot(aes(x = estimate, y = term)) +
  geom_point() +
  ggrepel::geom_label_repel(aes(label = scales::dollar(estimate, accuracy = 1)),
                            size = 3) +
  scale_x_continuous(labels = scales::dollar_format()) +
  labs(title = "Linear Regression: Feature Importance",
       subtitle = "Looks like large market size, promotion 1 and 3 contribute most to sales") +
  theme_bw()


