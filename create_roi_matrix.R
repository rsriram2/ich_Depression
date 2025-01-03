library(dplyr)
library(RNifti)
library(neurobase)
library(readr)
library(purrr)
library(tidyr)

file_df = readr::read_rds("/Users/rushil/Downloads/DepressionProject/filenames.rds")

# get the template information
temp_brain = readNifti("/Users/rushil/Downloads/DepressionProject/Data/templates/extracted_brain.nii.gz")
temp = readNifti("/Users/rushil/Downloads/DepressionProject/Data/templates/20_labeled_atlas.nii.gz")
full_temp = readNifti("/Users/rushil/Downloads/DepressionProject/Data/67M_283_LABELS_MNI.nii.gz")
temp_label_df = as.data.frame(table(label = c(temp))) %>%
  rename(n_template_voxels = Freq) %>%
  mutate(label = as.numeric(as.character(label)))
full_temp = round(full_temp)
full_temp_label_df = as.data.frame(table(label = c(full_temp))) %>%
  rename(n_template_voxels = Freq) %>%
  mutate(label = as.numeric(as.character(label)))

full_labels = read_csv("/Users/rushil/Downloads/DepressionProject/Data/templates/locus_atlas_tree.csv")
full_labels = full_labels %>%
  select(label = original_labels, region = anatomic_name) %>%
  filter(!(is.na(label) & is.na(region)))
stopifnot(all(1:283 %in% full_labels$label))
full_temp_label_df = full_temp_label_df %>%
  full_join(full_labels) %>%
  arrange(label)

# temp_labels = data.frame(label = c(temp),
#                          index = 1:length(c(temp)))

# get the labels for the template
# labels = read_csv("images/templates/locus_atlas_tree.csv")
# labels = labels %>%
#   distinct(region,
#            # anatomic_name,
#            label = `20_label`) %>%
#   arrange(label)

labels = read_csv("/Users/rushil/Downloads/DepressionProject/Data/atlas_labels.csv")

# can't merge because labels are off
# temp_labels = left_join(temp_labels, labels)


x = file_df$roi[1]
read_roi = function(x) {
  img = readNifti(x)
  arr = array(img)
  # make sure it's a mask!
  stopifnot(all(arr %in% c(0, 1)))
  img
}
roi_vec = function(img) {
  arr = array(img)
  # make sure it's a mask!
  stopifnot(all(arr %in% c(0, 1)))
  c(arr)
}

# file_df = file_df[1:20,]
############################
# read the ROI
imgs = map(file_df$roi, readNifti, .progress = TRUE)
names(imgs) = file_df$roi

x = imgs[[1]]

# cross-reference the template with the ROI
engagement = map_df(imgs, function(x) {
  tab = table(temp[c(x) > 0])
  tab = as.data.frame(tab) %>%
    rename(n = Freq,
           label = Var1) %>%
    mutate(label = as.numeric(as.character(label)))
  tab = full_join(temp_label_df, tab, by = join_by(label)) %>%
    tidyr::replace_na(list(n = 0))
  tab
}, .progress = TRUE, .id = "file")

engagement = engagement %>%
  group_by(file) %>%
  mutate(n_roi = sum(n))

# get Percent engagement
engagement = engagement %>%
  ungroup() %>%
  mutate(
    # what % of the template segmentation label engaged with ROI
    pct_region_engaged = n / n_template_voxels * 100,
    # what % of the ROI total is engaged with this region
    pct_roi_engaged = n / n_roi * 100)
engagement = engagement %>%
  left_join(labels)
readr::write_rds(engagement, "/Users/rushil/Downloads/DepressionProject/engagement.rds")

########################################
# Full engagement - all 283 labels
########################################
full_engagement = map_df(imgs, function(x) {
  tab = table(full_temp[c(x) > 0])
  tab = as.data.frame(tab) %>%
    rename(n = Freq,
           label = Var1) %>%
    mutate(label = as.numeric(as.character(label)))
  tab = full_join(full_temp_label_df, tab, by = join_by(label)) %>%
    tidyr::replace_na(list(n = 0))
  tab
}, .progress = TRUE, .id = "file")

full_engagement = full_engagement %>%
  group_by(file) %>%
  mutate(n_roi = sum(n))

# get Percent engagement
full_engagement = full_engagement %>%
  ungroup() %>%
  mutate(
    # what % of the template segmentation label engaged with ROI
    pct_region_engaged = n / n_template_voxels * 100,
    # what % of the ROI total is engaged with this region
    pct_roi_engaged = n / n_roi * 100)
full_engagement = full_engagement %>%
  left_join(labels)
readr::write_rds(full_engagement, "/Users/rushil/Downloads/DepressionProject/full_engagement.rds")


############################
# Make regression mat
############################
mat = sapply(imgs, roi_vec)
mat = t(mat)

readr::write_rds(mat, "/Users/rushil/Downloads/DepressionProject/roi_matrix.rds", compress = "gz")

cm = colMeans(mat)
cm = array(cm, dim = dim(temp_brain))
cm = asNifti(cm, reference = temp_brain)
writeNifti(cm, file = "/Users/rushil/Downloads/DepressionProject/mean_engagement.nii.gz")
# ortho2(temp_brain, cm, xyz = xyz(cm > 0), window = c(0, 100))
