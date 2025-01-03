library(dplyr)
library(readr)
library(purrr)
library(tidyr)
library(here)
library(readxl)
library(janitor)

convert_safe = function(
    x,
    func,
    type = "logical",
    varname = "") {
  na_x = is.na(x)
  xx = func(x)
  na_xx = is.na(xx)
  if (any(na_xx & !na_x)) {
    msg = paste0("Recoding",
                 ifelse(nchar(varname) > 0, " ", ""),
                 varname, " to ",
                 type, " would cause NA, returning original")
    warning(msg)
    return(x)
  }
  xx
}

parse_number_safe = function(x) {
  if (is.numeric(x)) {
    return(x)
  }
  x = convert_safe(x = x,
                   func = readr::parse_number,
                   type = "numeric")
  x
}


file_df = readr::read_rds(here::here("/Users/rushil/Downloads/DepressionProject/filenames.rds"))

get_id_from_filename = function(x) {
  x = basename(x)
  x = sub("_.*", "", x)
  x = sub(".*(\\d{4}-\\d{3}).*", "\\1", x)
  x
}

reverse_id = function(x) {
  x = sub(".*(\\d{3})-(\\d{4}).*", "\\2-\\1", x)
  x
}

df = read_excel(here::here("/Users/rushil/Downloads/DepressionProject/Data/M3_demographics and outcome metrics_Rushil.xlsx"))
null_to_na = function(x) {
  x[tolower(x) %in% "null"] = NA
  x
}

df = df %>%
  janitor::clean_names()

df = df %>%
  mutate(id_patient = reverse_id(patient_name)) %>%
  rename(age = age_at_consent,
         male = male_gender) %>%
  mutate_if(is.character, null_to_na)
stopifnot(all(df$study_name == "MISTIE III"))
df = df %>%
  select(-any_of(c("study_name")))
df = df %>%
  mutate(cesd_d180 = parse_number_safe(cesd_d180),
         depression = 1*(cesd_d180 >= 16))
readr::write_rds(df, here::here("/Users/rushil/Downloads/DepressionProject/demographic_outcomes.rds"))
