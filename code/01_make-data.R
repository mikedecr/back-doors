# ----------------------------------------------------
#   Heiss blog post for learning causal inf methods
#   01: making the data
# ----------------------------------------------------

library("tidyverse")  
library("dagitty")  
library("ggdag")
library("broom")  

# ---- make dag -----------------------

node_details <- 
  tribble(
    ~ name              , ~ label             , ~ x , ~ y ,
    "math_camp"         , "Math camp"         , 2   , 1   ,
    "final_grade"       , "Final grade"       , 4   , 1   ,
    "needs_camp"        , "Needs camp"        , 1   , 2   ,
    "gre_quant"         , "GRE quantitative"  , 2.5 , 2   ,
    "gre_verbal"        , "GRE verbal"        , 5   , 2   ,
    "background"        , "Background"        , 2   , 3   ,
    "undergraduate_gpa" , "Undergraduate GPA" , 4   , 3
  ) %>%
  print()

node_labels <- node_details$label
names(node_labels) <- node_details$name

math_camp_dag <- 
  dagify(
    final_grade ~ math_camp + gre_quant + gre_verbal + 
                  undergraduate_gpa + background,
    math_camp ~ needs_camp, 
    needs_camp ~ background + undergraduate_gpa + gre_quant, 
    gre_quant ~ background + undergraduate_gpa, 
    gre_verbal ~ background + undergraduate_gpa, 
    undergraduate_gpa ~ background, 
    exposure = "math_camp", 
    outcome = "final_grade", 
    latent = "background", 
    coords = node_details, 
    labels = node_labels
  ) %>%
  print()

# Turn DAG into a tidy data frame for plotting
math_camp_dag_tidy <- math_camp_dag %>% 
  tidy_dagitty() %>%
  node_status() %>%
  print()

status_colors <- c(
  exposure = "#0074D9", 
  outcome = "#FF4136", 
  latent = "grey50"
)

# Fancier graph
ggplot(math_camp_dag_tidy) +
  aes(x = x, y = y, xend = xend, yend = yend) +
  geom_dag_edges() +
  geom_dag_point(aes(color = status)) +
  geom_dag_label_repel(
    aes(label = label, fill = status), 
    seed = 1234, 
    color = "white", 
    fontface = "bold"
  ) +
  scale_color_manual(values = status_colors, na.value = "grey20") +
  scale_fill_manual(values = status_colors, na.value = "grey20") +
  guides(color = FALSE, fill = FALSE) + 
  theme_dag()


# possible paths
paths(math_camp_dag)

# what do I NEED to control for 
# to ID [math camp] -> [final grade]
adjustmentSets(math_camp_dag)


# ---- make the data -----------------------

# Make these random draws the same every time
set.seed(1234)

# Create 2,000 rows
num <- 2000

# Create confounder variables that are related to each other
mu <- c(undergrad_gpa = 3, gre_verbal = 160, gre_quant = 145)
stddev <- c(undergrad_gpa = 0.5, gre_verbal = 10, gre_quant = 5)

# Correlation matrix: undergrad GPA and verbal GRE have a correlation of 0.8;
# undergrad GPA and quantitative GRE have a correlation of 0.6, and verbal GRE
# and quantitative GRE have a correlation of 0.4
cor_matrix <- matrix(
  c(1.0, 0.8, 0.6, 
    0.8, 1.0, 0.4, 
    0.6, 0.4, 1.0), 
  ncol = 3
)

# Convert correlation matrix to covariance matrix using fancy math
cov_matrix <- diag(stddev) %*% cor_matrix %*% diag(stddev)

# Draw random numbers
confounders <- 
  MASS::mvrnorm(n = num, mu = mu, Sigma = cov_matrix, empirical = TRUE) %>%
  as_tibble() %>%
  # Truncate values so they're within 130-170 range for GRE and less than 4.0 for GPA
  mutate_at(vars(gre_verbal, gre_quant),
            ~case_when(
              . > 170 ~ 170,
              . < 130 ~ 130,
              TRUE ~ .
            )) %>%
  mutate(undergrad_gpa = ifelse(undergrad_gpa > 4, 4, undergrad_gpa)) %>%
  print()

# Make official dataset of simulated values
math_camp <- 
  tibble(id = 1:num) %>%
  bind_cols(confounders) %>%  # Bring in confounders
  # People need math camp if their GRE and GPA is lower than the average
  mutate(
    needs_camp = gre_quant < mean(gre_quant) & 
                 undergrad_gpa < mean(undergrad_gpa)
  ) %>%
  # Build in some noncompliance: 80% of those who need math camp do it; 20% of
  # those who don't need it do it.
  mutate(
    math_camp = case_when(
      needs_camp == TRUE ~ rbinom(n(), 1, 0.8), 
      needs_camp == FALSE ~ rbinom(n(), 1, 0.2)
    )
  ) %>%
  # Create random error in grades
  mutate(grade_noise = rnorm(num, 15, 5)) %>%
  # Create final grade based on all the arrows going into the node in the DAG.
  # There's a 10 point causal effect
  mutate(
    final_grade = (0.3 * gre_verbal) + (0.5 * gre_quant) + 
                  (0.4 * undergrad_gpa) + (10 * math_camp) + grade_noise
  ) %>%
  mutate(math_camp = as.logical(math_camp)) %>%  # Make true/false
  print()
