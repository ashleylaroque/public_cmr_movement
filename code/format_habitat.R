
#' Description:
#' This script format habitat data from the North Campus
#' at the University of North Carolina Greensboro.

# setup -------------------------------------------------------------------

## remove objects
rm(list = ls())

source(here::here("code/library.R"))
source(here::here("code/function.R"))

# Read Habitat Data-----------------------------------------------------------------

## download data for pit tags
## run only when data need to be updated
# drive_download("data_habitat_v1_1_3",
#                type = "csv",
#                path = "data_raw/data_habitat.csv",
#                overwrite = T)

## source data
## - remove velocity cells with NA:
## -- one transect was overlooked during the fieldwork
## - remove deepest_depth:
## -- stopped measuring deepest depth partway
df_habitat <- read_csv("data_raw/data_habitat.csv") %>%
  rename_with(.fn = str_to_lower, .cols = everything()) %>% 
  mutate(occasion = as_factor(occasion),
         occasion = as.numeric(occasion)) %>% 
  select(-c(deepest_depth, comments)) %>% 
  drop_na(starts_with("velocity"))


# Format Habitat Data -----------------------------------------------------

## aggregate quadrats to transect
df_tr <- df_habitat %>%
  mutate(across(starts_with("substrate"),
                .fns = function(x) {
                  case_when(x == "SL" ~ 1, #function finds what is in " " and replaces with what's after ~
                            x == "SD" ~ 2,
                            x == "GV" ~ 3,
                            x == "PE" ~ 4,
                            x == "CO" ~ 5,
                            x == "BO" ~ 6,
                            x == "BR" ~ 7)})) %>% # convert substrate to numeric values
  rowwise() %>% # rowwise operation
  mutate(depth = mean(depth1, depth2, depth3),
         velocity = mean(velocity1, velocity2, velocity3),
         substrate = mean(substrate1, substrate2, substrate3)) %>% # take average of 3 values
  select(-c(paste0("depth", 1:3),
            paste0("velocity", 1:3),
            paste0("substrate", 1:3))) %>% # remove depth1, depth2, etc...
  ungroup()

## aggregate transects to section
df_sec <- df_tr %>%
  rowwise() %>% # rowwise operation
  mutate(section_length = sum(pool_length,
                              riffle_length,
                              run_length)) %>% # sum pool, riffle, run length for each row
  ungroup() %>%
  group_by(occasion, section) %>% # mean or remove NAs by occasion and section
  reframe(width = mean(width),
            section_length = na.omit(section_length),
            depth_mean = mean(depth),
            velocity_mean = mean(velocity),
            substrate_mean = mean(substrate),
            area = width * section_length,
            area_pool = width * na.omit(pool_length),  
            area_run = width * na.omit(run_length),
            area_riffle = width * na.omit(riffle_length)) %>%
  ungroup()

## undercut bank
## - organize data by individual undercut bank
df_ucb <- df_habitat %>% 
  pivot_longer(cols = starts_with("ucb"), # select columns starting with "ucb"
               names_to = c("ucb_id", "dimension"),
               values_to = "value",
               names_sep = "_") %>% # split ucb columns into "ucb_id" and "dimension" by "_"
  dplyr::select(occasion,
                section,
                ucb_id,    #refers to what ucb it is (length, dep 1, dep 2 etc)
                dimension, #refers to the measurement (length, dep 1, dep 2 etc)
                value) %>% #refers to actual numerical value measured 
  mutate(dimension = case_when(dimension == "len" ~ "length",
                               str_detect(dimension, "dep") ~ "depth")) %>%  # change "dimensions" column elements
  drop_na(value) %>% # drop NAs in "value" column (Occ 11: from 22672 values to 850)
  group_by(occasion,
           section,
           ucb_id,
           dimension) %>%
  reframe(value = mean(value)) %>% # take means for each ucb
  pivot_wider(values_from = value,
              names_from = dimension) %>% # lateral extension
  ungroup() %>% 
  mutate(depth = 0.01 * depth) # convert unit: [cm] to [m]

## - reframe by occasion and section
df_ucb_sec <- df_ucb %>%
  mutate(area = depth * length) %>% # add ucb area column 
  group_by(occasion, section) %>%
  reframe(area_ucb = sum(area)) %>%  # sum by section returning only 1 row 
  ungroup()

## merge data
df_h_sec <- df_sec %>% #has depth, substrate, velocity info
  left_join(df_ucb_sec, #has undercut bank info
            by = c("occasion", "section")) %>% 
  mutate(area_ucb = replace_na(area_ucb, 0)) # replace NA with zero


## export
saveRDS(df_h_sec, "data_fmt/data_habitat.rds")
