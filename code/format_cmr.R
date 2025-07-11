
#' Description:
#' This script format capture-mark-recapture data from the North Campus
#' at the University of North Carolina Greensboro.
#' This includes formatting of fish density data.

# setup -------------------------------------------------------------------

## remove objects
#rm(list = ls())

source(here::here("code/library.R"))
source(here::here("code/function.R"))

## run only when need to update habitat data
## `format_habitat.R` will update `data_habitat.rds`
source("code/format_habitat.R")

# format tagged data ------------------------------------------------------

# ## download data for pit tags
# ## run only when data need to be updated
# drive_download("data_cmr_v1_1_6",
#                type = "csv",
#                path = "data_raw/data_cmr_src.csv",
#                overwrite = T)

## df_cmr0: data frame before cleaning
df_cmr0 <- read_csv(here::here("data_raw/data_cmr_src.csv")) %>% 
  rename_with(.fn = str_to_lower,
              .cols = everything()) %>% # make column headers lowercase
  dplyr::select(occasion,
                date,
                time,
                tag_id = tag_id2,
                site,
                species,
                section,
                length,
                weight,
                recap,
                mortality) %>% 
  filter(mortality == "n")

# ## check length / weight relationship (visual)
# ## - before removing suspicious data
# ggplot(df_cmr0,
#        aes(x = length ,
#            y = weight,
#            color = factor(occasion))) +
#   geom_point()+
#   facet_wrap(~species) +
#   scale_x_continuous(trans = "log10") +
#   scale_y_continuous(trans = "log10") #shows some outliers of l v w relationship

## rows with suspicious weight/length entry
## - for checking; not used in the following analysis
df_err <- mrcheck(df_cmr0,
                  xi = 0.3,
                  cnm = colnames(df_cmr0))


## mrcheck() performs:
## - remove rows with tag_id = NA
## - check if inconsistent species names for a given tag_id
## - select the last capture if multiple captures occurred within the same occasion
## - append weight values of robust regression between body length and weight
## - convert date character to date format
df_cmr <- mrcheck(df_cmr0,
                  xi = 0,
                  cnm = colnames(df_cmr0)) %>% 
  mutate(weight = ifelse(rlm_weight <= 0.3, NA, weight),
         occasion = as_factor(occasion),
         occasion = as.numeric(occasion))

# ## check length / weight relationship (visual)
# ## - after removing suspicious data (NA in weight)
# ggplot(df_cmr,
#        aes(x = length ,
#            y = weight,
#            color = factor(occasion))) +
#   geom_point()+
#   facet_wrap(~species) +
#   scale_x_continuous(trans = "log10") +
#   scale_y_continuous(trans = "log10") #shows some outliers of l v w relationship

input <- c("section", "length", "datetime", "weight")

list_move <- lapply(input, function(x) {
  df_cmr %>% 
    arrange(occasion, species) %>% 
    mutate(event = paste0("oc_", sprintf("%02d", occasion))) %>% 
    pivot_wider(id_cols = c(species, tag_id),
                names_from = event,
                values_from = x)
})

# vectorize data
df_move <- lapply(seq_len(length(list_move)),
                  function(i) {
                    fvec(list_move[[i]], input = input[i])
                  }) %>% 
  reduce(left_join, by = c("species", "tag_id", "occasion0", "occasion1"))

## export
saveRDS(df_cmr,
        file = "data_fmt/data_cmr.rds")

saveRDS(df_move,
        file = "data_fmt/data_move.rds")


# format for predictors ---------------------------------------------------

# ## data on non-target species
# ## run only when data need to be updated
# drive_download("data_non_target_v1_1_4",
#                type = "csv",
#                path = "data_raw/data_non_target.csv",
#                overwrite = T )

## get abundance for untagged individuals per section per occasion
df_nt <- read_csv(here::here("data_raw/data_non_target.csv")) %>% 
  rename_with(.fn = str_to_lower) %>% # make all column headers lowercase
  mutate(occasion = as_factor(occasion),
         occasion = as.numeric(occasion),
         species = case_when(species == "BHC" ~ "bluehead_chub",
                             species == "BLG" ~ "bluegill",
                             species == "CCS" ~ "creekchub_sucker",
                             species == "CRC" ~ "creek_chub",
                             species == "DACE" ~ "dace",
                             species == "DAT" ~ "darter",
                             species == "EMF" ~ "eastern_mosquitofish",
                             species == "GSF" ~ "green_sunfish",
                             species == "KF" ~ "killifish",
                             species == "LMB" ~ "largemouth_bass",
                             species == "LOACH" ~ "loach",
                             species == "MADTOM" ~ "madtom",
                             species == "RBS" ~ "redbreast_sunfish",
                             species == "REDHORSE" ~ "redhorse",
                             species == "SHINER" ~ "shiner",
                             species == "STJ" ~ "striped_jumprock",
                             species == "WAR" ~ "warmouth",
                             species == "YB" ~ "yellow_bullhead")) %>% 
  group_by(species,
           section,
           occasion) %>% 
  tally() %>% 
  ungroup() %>% 
  drop_na(species)

## get abundance for tagged individuals per section per occasion
df_t <- df_cmr %>% 
  group_by(species,
           section,
           occasion) %>% 
  tally() %>% 
  ungroup() 

## get unique species id for tagged & untagged combined
usp <- c(df_t$species, df_nt$species) %>% 
  unique() %>% 
  sort()

## combine tagged and un-tagged data
df_n <- with(df_cmr,
             expand.grid(occasion = sort(unique(occasion)),
                         section = seq(1, 43, by = 1), 
                         species = usp)) %>% 
  as_tibble() %>% 
  left_join(df_t, by = c("occasion", "section", "species")) %>% 
  left_join(df_nt, by = c("occasion", "section", "species")) %>% 
  mutate(n.x = replace_na(n.x, 0),
         n.y = replace_na(n.y, 0),
         n = n.x + n.y) %>% 
  dplyr::select(-n.x, -n.y)

## append environmental variables
df_h <- readRDS("data_fmt/data_habitat.rds")

df_den <- df_n %>% 
  left_join(df_h %>% select(occasion, section, area)) %>% 
  mutate(density = n / area) 

## detectability correction
# comes from 'run_model_scjs'; detectability estimate
df_zeta <- readRDS("data_fmt/data_detection.rds")

# comes from 'run_model_scjs'; season data frame
df_season <- readRDS("data_fmt/data_season.rds") 

df_den_adj <- df_den %>% 
  left_join(df_season, by = "occasion") %>% 
  mutate(season = case_when(season == 0 ~ "winter",
                            season == 1 ~ "summer")) %>% 
  left_join(df_zeta,
            by = c("species", "season")) %>% 
  rename(p_detect = estimate) %>% 
  mutate(adj_density = (density / p_detect)) %>% 
  select(-para)

# density distance matrix -------------------------------------------------

theta <- 0.1

df_den_w0 <- df_den_adj %>% 
  transmute(occasion,
            section,
            species,
            area,
            n,
            density,
            adj_density,
            id = paste(occasion, species, sep = "-"),
            theta = theta) %>% 
  filter(species %in% c("bluehead_chub",
                        "creek_chub",
                        "green_sunfish",
                        "redbreast_sunfish"))

index <- pull(df_den_w0, id) %>% 
  unique()

pb <- txtProgressBar(min = 0, 
                     max = length(index), 
                     style = 3) 

## obtain weighted density by distance
df_den_w <- foreach(i = seq_len(length(index)),
                    .combine = bind_rows) %do% {
                      
                      ## progress bar
                      setTxtProgressBar(pb, i)
                      
                      ## occasion-species specific data
                      ## 43 data points
                      df_i <- df_den_w0 %>% 
                        filter(id == index[i]) %>% 
                        arrange(section)
                      
                      ## distance matrix
                      X <- df_i %>% 
                        mutate(x = section * 10 - 5) %>% 
                        pull(x) %>% 
                        dist() %>% 
                        data.matrix()
                      
                      ## weighted density
                      ## - weight matrix
                      W <- exp(-theta * X)
                      
                      ## - adjusted density vector
                      v_adj_den <- df_i %>% 
                        pull(adj_density)
                      
                      ## - weighted density
                      w_den <- drop(W %*% v_adj_den)
                      
                      ## return
                      df_i %>% 
                        mutate(w_density = w_den) %>% 
                        select(-id) %>% 
                        return()
                    }

close(pb)

## export
saveRDS(df_den_w, file = "data_fmt/data_density.rds")
