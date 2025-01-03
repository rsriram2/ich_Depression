library(dplyr)
library(readr)
library(purrr)
library(tidyr)
library(here)

get_id_from_filename = function(x) {
  x = basename(x)
  x = sub("_.*", "", x)
  x = sub(".*(\\d{4}-\\d{3}).*", "\\1", x)
  x
}

file_df = readr::read_rds(here::here("/Users/rushil/Downloads/DepressionProject/filenames.rds"))

#### Joining data with Outcomes/files ####
demog = readr::read_rds(here::here("/Users/rushil/Downloads/DepressionProject/demographic_outcomes.rds"))
demog = demog %>%
  select(id_patient, depression, age, male, gcs = gcs_randomization,
         nihss = nihss_randomization)
stopifnot(all(file_df$id_patient %in% demog$id_patient))
demog = file_df %>%
  select(id_patient, file = roi) %>%
  left_join(demog)

# Remove this if you're going to do other depression scores here
demog = demog %>%
  filter(!is.na(depression))

# mat = readr::read_rds(here::here("data/roi_matrix.rds"))
# stopifnot(all(rownames(mat) %in% file_df$roi))

#### Reading Engagement ####
engagement = readr::read_rds(here::here("/Users/rushil/Downloads/DepressionProject/engagement.rds"))


##### Add in the demographics ####
e = engagement %>%
  inner_join(demog)

# remove background label data
e = e %>%
  filter(label != 0)

# separate the data by label, so each data.frame as N rows, where N
# is the number of participants
sdf = split(e, e$region)
# Background
sdf$`0` = NULL
model_df = sdf[[1]]


#### Run % Engagement Models ####
mods = purrr::map(sdf, function(model_df) {
  if (all(model_df$pct_region_engaged == 0)) {
    return(NULL)
  }
  model = glm(
    depression ~ pct_region_engaged,
    data = model_df, family = binomial())

  demog_adj_model = glm(
    depression ~ pct_region_engaged + male + age,
    data = model_df, family = binomial())

  adj_model = glm(
    depression ~ pct_region_engaged + male + age  + gcs + nihss,
    data = model_df, family = binomial())

  list(
    model = model,
    adjusted_model = adj_model,
    demog_adjusted_model = demog_adj_model
  )
})

readr::write_rds(mods, here::here("/Users/rushil/Downloads/DepressionProject/logistic_models_engagement.rds"))

#### Extract Model Coefficients ####
coef_df = purrr::map_df(mods, function(x) {
   if (is.null(x)) {
     return(NULL)
   }
   df = broom::tidy(x$model, conf.int = TRUE) %>%
     mutate(model = "unadjusted")
   adj_df = broom::tidy(x$adjusted_model, conf.int = TRUE) %>%
     mutate(model = "adjusted")
   df = dplyr::bind_rows(df, adj_df)
   df
 }, .id = "region")
