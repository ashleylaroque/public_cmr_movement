
#' Description:
#' This script runs modified CJS model for predicting movement with density and body size

# setup -------------------------------------------------------------------

source("code/library.R")
rm(list = ls())

## base data frame with all data
df_tag0 <- readRDS("data_formatted/data_cmr.rds")

## consecutive movement data frame
df_con <- readRDS("data_formatted/data_move.rds")

## baseline interval information
## = from the 1st day at a given occasion to the 1st day of the next
## `intv0` will be used for those that are not recaptured
## Thus, calculated across all species
intv0 <- df_tag0 %>%
  group_by(occasion) %>% 
  summarize(min_date0 = min(datetime)) %>% 
  mutate(min_date1 = lead(min_date0),
         intv = round(min_date1 - min_date0) %>% 
           as.numeric()) %>% 
  drop_na(intv) %>% 
  pull(intv)


# analysis by species -----------------------------------------------------

## species ID vector for separate model by species 
usp <- c("bluehead_chub",
         "creek_chub",
         "green_sunfish",
         "redbreast_sunfish",
         #"bluegill", doesn't converge
         "striped_jumprock") %>%
  sort()

list_est <- foreach(x = usp) %do% {
  
  # format data for JAGS ----------------------------------------------------
  
  ## filter by species
  df_i <- filter(df_tag0, species == x)
  df_c <- filter(df_con, species == x)
  
  ## raw recapture data for species x
  df_tag <- df_i %>%
    mutate(tag_index = as.numeric(as.factor(tag_id))) %>% 
    arrange(tag_index, occasion) %>% 
    relocate(tag_index, occasion)
  
  ## recapture interval for each individual
  ## - if recaptured, interval is as observed
  ## - if not, interval is: 
  ## -- "from the 1st day at a given occasion to the 1st day of the next"
  df_intv <- df_c %>% 
    mutate(intv = datetime1 - datetime0) %>% 
    group_by(occasion0) %>% 
    mutate(intv = ifelse(is.na(intv),
                         yes = intv0[occasion0],
                         no = intv),
           intv = round(intv)) %>% 
    select(tag_id,
           occasion = occasion0,
           intv) %>% 
    ungroup()
  
  ## create vectorized recapture format
  df_y <- df_tag %>% 
    pivot_wider(id_cols = c(tag_index, tag_id),
                names_from = occasion,
                values_from = section) %>% 
    pivot_longer(cols = -c(tag_index, tag_id),
                 names_to = "occasion",
                 values_to = "y") %>% 
    mutate(y = ifelse(is.na(y), 0, 1),
           occasion = as.numeric(occasion)) %>% 
    arrange(tag_index, occasion) %>% 
    left_join(df_intv)
  
  ## append season column to df_y
  df_season <- df_tag %>% 
    mutate(month = format(datetime, "%m") %>% 
             as.numeric(month),
           season = ifelse(between(month, 4, 9),
                           yes = 1,
                           no = 0)) %>% 
    distinct(occasion,
             season) %>% 
    arrange(occasion)
  
  df_y <- left_join(df_y, df_season)
  
  # create first capture vector 
  df_fc <- df_y %>% 
    group_by(tag_index) %>% 
    filter(y == 1) %>% 
    slice(which.min(occasion)) %>% 
    ungroup() %>% 
    arrange(tag_index)
  
  ## create movement vector
  df_dist <- df_tag %>% 
    pivot_wider(id_cols = tag_index,
                names_from = occasion,
                values_from = section) %>% 
    pivot_longer(cols = -tag_index,
                 names_to = "occasion",
                 values_to = "section") %>% 
    mutate(x = 10 * section - 5,
           occasion = as.numeric(occasion)) %>% 
    arrange(tag_index, occasion) %>% 
    drop_na(x)
  
  ## binary recapture record
  list_recap <- with(df_y,
                     list(Y = y, # capture state  
                          Id_tag_y = tag_index, # tag id
                          Id_occ_y = occasion, # occasion
                          Nind = n_distinct(tag_index), # n unique ind
                          Nocc = n_distinct(occasion), # n unique occasions
                          Nobs = nrow(df_y),
                          Season = season,
                          L = 430, # total length
                          Fc = df_fc$occasion # first capture
                     ))
  
  list_intv <- with(df_y %>% drop_na(intv),
                    list(Intv = intv,
                         Nint = nrow(df_y %>% drop_na(intv)),
                         Id_tag_int = tag_index, # tag id
                         Id_occ_int = occasion))
  
  ## movement distance
  list_move <- with(df_dist,
                    list(X = x, # distance class
                         Id_tag_x = tag_index,
                         Id_occ_x = occasion,
                         Nx = nrow(df_dist)))
  
  
  d_jags <- c(list_recap, list_intv, list_move)
  
  para <- c("zeta",
            "sd0",
            "mean.p",
            "mu.p",
            "mu.phi",
            "alpha",
            "mean.phi")
  
  # mcmc setup --------------------------------------------------------------
  
  ## model file ####
  mcjs <- runjags::read.jagsfile("code/model_scjs.R")
  
  ## mcmc setup ####
  n_ad <- 1000
  n_iter <- 25000
  n_thin <- max(3, ceiling(n_iter / 1000)) #happens second want chains to converge 
  n_burn <- ceiling(max(10, n_iter / 2)) #happens first and gets rid of noise 
  n_sample <- ceiling(n_iter / n_thin)
  n_chain <- 3
  
  inits <- replicate(n_chain,
                     list(.RNG.name = "base::Mersenne-Twister",
                          .RNG.seed = NA,
                          mean.p = 0.25,
                          mean.phi = 0.999),
                     simplify = FALSE)
  
  for (j in 1:n_chain) inits[[j]]$.RNG.seed <- (j - 1) * 10 + 1
  
  
  # run ---------------------------------------------------------------------
  
  post <- runjags::run.jags(model = mcjs$model,
                            monitor = para,
                            data = d_jags,
                            n.chains = n_chain,
                            inits = inits,
                            method = "parallel",
                            burnin = n_burn,
                            sample = n_sample,
                            adapt = n_ad,
                            thin = n_thin,
                            n.sims = n_chain,
                            module = "glm") #specific to jags 
  
  cout <- MCMCvis::MCMCsummary(post$mcmc) %>% 
    as_tibble(rownames = "para") %>% 
    mutate(species = x)
  
  return(cout)
}


## extract median zeta (= seasonal detection probability)
df_zeta <- lapply(list_est,
                  function(data) {
                    data %>% 
                      filter(str_detect(para, "zeta\\[.\\]")) %>% 
                      dplyr::select(para,
                                    species,
                                    estimate = `50%`) %>% 
                      mutate(season = ifelse(str_detect(para, "\\[1\\]"),
                                             "winter",
                                             "summer"))
                  }) %>% 
  bind_rows()


## export
saveRDS(list_est, file = "data_formatted/output_cjs.rds")
saveRDS(df_zeta, file = "data_formatted/data_detection.rds")
saveRDS(df_season, file = "data_formatted/data_season.rds")

list_est[[1]]
list_est[[2]]
list_est[[3]]
list_est[[4]]
list_est[[5]]
list_est[[6]]
