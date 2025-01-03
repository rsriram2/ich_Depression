library(dplyr)
library(RNifti)
library(neurobase)
library(readr)
library(tidyr)


files = list.files(path = "/Users/rushil/Downloads/DepressionProject/Data/MISTIE_3_sample_data",
                   recursive = TRUE, pattern = ".nii.gz",
                   full.names = TRUE)
df = data.frame(
  file = files,
  stringsAsFactors = FALSE
) %>%
  mutate(
    fname = tolower(basename(file)),
    date = sub(".*(\\d{8})_.*", "\\1", fname),
    time = sub(".*_(\\d*)[.]nii.*", "\\1", fname),
    stub = sub("_\\d{8}.*", "", fname)
  ) %>%
  tidyr::separate(
    stub,
    into = c("id_patient", "type"),
    extra = "merge",
    sep = "_",
    remove = FALSE)
readr::write_rds(df, "/Users/rushil/Downloads/DepressionProject/long_filenames.rds")

df = df %>%
  select(file, id_patient, type) %>%
  spread(type, file)
readr::write_rds(df, "/Users/rushil/Downloads/DepressionProject/filenames.rds")
# img
