# ----------------------------------------------------
#   Heiss blog post for learning causal inf methods
#   02: Heiss methods
# ----------------------------------------------------

library("here")
library("tidyverse")
library("broom")

theme_set(theme_minimal())

source(here("code", "01_make-data.R"))


# ---- descriptives -----------------------

math_camp %>% 
  count(math_camp) %>% 
  mutate(prop = n / sum(n))


# ---- incorrect correlation estimate -----------------------

math_camp %>% 
  group_by(math_camp) %>% 
  summarize(avg = mean(final_grade))

ggplot(math_camp) +
  aes(x = final_grade, fill = math_camp) +
  geom_histogram(
    binwidth = 2, color = NA,
    position = "identity",
    alpha = 0.5
  )


model_wrong <- lm(final_grade ~ math_camp, data = math_camp)

tidy(model_wrong)
  

# ---- forbidden unmeasured variable -----------------------

model_adj_needs_camp <- 
  lm(final_grade ~ math_camp + needs_camp, data = math_camp)

tidy(model_adj_needs_camp)
