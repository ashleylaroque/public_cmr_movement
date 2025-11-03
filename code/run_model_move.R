#' Description:
#' This script runs modified dispersal-observation model for predicting movement with density and body size

rm(list = ls())
source("code/library.R")
source("code/function.R")

# data --------------------------------------------------------------------

## format data
df_move0 <- readRDS("data_fmt/data_move.rds") %>% 
  drop_na(section0)

df_den_w <- readRDS("data_fmt/data_density.rds") %>% 
  pivot_wider(id_cols = c(occasion, section, area),
              names_from = species,
              values_from = c(n, adj_density, w_density))

df_h <- readRDS("data_fmt/data_habitat.rds") %>% 
  dplyr::select(-area)

# comes from 'format_water_level'; water temperature data
df_hobo <- readRDS("data_fmt/data_water_hobo.rds")

## combine movement, density, and habitat
df_move <- df_move0 %>% # movement dataframe
  left_join(df_den_w, # add density values
            by = c("occasion0" = "occasion",
                   "section0" = "section")) %>% 
  left_join(df_h, # add habitat variables 
            by = c("occasion0" = "occasion",
                   "section0" = "section")) %>% 
  left_join(df_hobo, # add water temp
            by = c("occasion0" = "occasion")) %>% 
  mutate(intv = as.numeric(datetime1 - datetime0),
         y = ifelse(is.na(section1), 0, 1),
         julian = yday(datetime0)) %>%
  group_by(occasion0) %>% 
  mutate(intv = ifelse(is.na(intv),
                       median(intv, na.rm = TRUE),
                       intv)) %>% 
  ungroup() %>% 
  select(-c(starts_with("n_"))) %>% 
  filter(species %in% c("bluehead_chub",
                        "creek_chub",
                        "green_sunfish",
                        "redbreast_sunfish")) %>% 
  mutate(uid = paste0(species,
                      occasion0,
                      section0,
                      tag_id, 
                      sep = "-") %>% 
           factor() %>% 
           as.numeric() %>% 
           str_pad(width = 4, pad = "0") %>% 
           paste0("ID", .))

saveRDS(df_move, file = "data_fmt/data_combined.rds")


# run jags ----------------------------------------------------------------

usp <- c("green_sunfish",
         "redbreast_sunfish",
         "creek_chub",
         "bluehead_chub") %>%
  sort()

## mcmc setup ####
n_ad <- 1000
n_iter <- 30000
n_thin <- max(3, ceiling(n_iter / 1000))
n_burn <- ceiling(max(10, n_iter / 2))
n_sample <- ceiling(n_iter / n_thin)
n_chain <- 3

list_mcmc <- foreach(x = usp) %do% {
  
  df_i <- filter(df_move, species == x) %>%
    mutate(log_length = log(length0),
           area_ucb = sqrt(area_ucb))
  
  ## data for jags
  list_jags <- with(df_i,
                    list(Y = y,
                         X1 = section1 * 10 - 5,
                         X0 = section0 * 10 - 5,
                         Nsample = nrow(df_i),
                         Intv = intv)
  )
  
  ## select predictors
  X <- df_i %>%
    dplyr::select(log_length, # log-trans total length of individual
                  area_ucb,
                  velocity_mean,
                  w_density_bluehead_chub, # seasonally adjusted density
                  w_density_creek_chub,
                  w_density_green_sunfish,
                  w_density_redbreast_sunfish,
                  julian) %>% 
    mutate(across(.cols = everything(),
                  .fns = function(x) c(scale(x)))) %>%
    mutate(julian_sq = julian^2) %>% 
    model.matrix(~., data = .)
  
  list_jags$X <- X
  list_jags$K <- ncol(X)
  
  ## run.jags arguments
  ## - initial values
  n_chain <- 3
  
  inits <- replicate(n_chain,
                     list(.RNG.name = "base::Mersenne-Twister",
                          .RNG.seed = NA,
                          p = 0.1,
                          b = rep(0, ncol(X)),
                          nu = 50),
                     simplify = FALSE)
  
  for (j in 1:n_chain) inits[[j]]$.RNG.seed <- 100 * j
  
  ## - parameters to be monitored
  para <- c("b", "p", "nu", "D")
  
  ## model files
  m <- runjags::read.jagsfile("code/model_move.R")
  
  ## run model
  post <- runjags::run.jags(model = m$model,
                            data = list_jags,
                            monitor = para,
                            adapt = n_ad,
                            burnin = n_burn,
                            sample = n_sample,
                            thin = n_thin,
                            n.chains = n_chain,
                            inits = inits,
                            method = "parallel",
                            module = "glm")
  
  named_mcmc <- lapply(post$mcmc, 
                       FUN = function(chain) {
                         
                         ## rename columns of each chain
                         colnames(chain)[1:ncol(X)] <- colnames(X)
                         colnames(chain)[str_detect(colnames(chain), "D\\[.*\\]")] <- df_i$uid
                         return(chain)
                         
                       })
  
  return(named_mcmc)
}

names(list_mcmc) <- usp

## get summary estimates
df_est <- lapply(seq_len(length(list_mcmc)),
                 function(i) {
                   
                   mcmc <- list_mcmc[[i]]
                   p_neg <- MCMCvis::MCMCpstr(mcmc,
                                              func = function(x) mean(x < 0)) %>% 
                     unlist() %>% 
                     { .[!str_detect(names(.), "^ID.*")] }
                   
                   MCMCvis::MCMCsummary(mcmc) %>% 
                     as_tibble(rownames = "parm") %>% 
                     filter(!str_detect(parm, "^ID.*")) %>% 
                     mutate(p_neg = p_neg,
                            p_pos = 1 - p_neg) %>% 
                     mutate(y = names(list_mcmc)[i],
                            parm = ifelse(str_detect(parm, ".*Intercept.*"),
                                          "(Intercept)",
                                          parm))
                   
                 }) %>% 
  bind_rows() %>% 
  rename(species = "y",
         median = "50%" ,
         upper95 = "97.5%" ,
         lower95 = "2.5%") %>% 
  rowwise() %>% 
  mutate(prob = max(p_pos, p_neg)) %>% 
  ungroup()
  
## get predicted values
df_pred <- foreach(i = seq_len(length(usp)),
                   .combine = bind_rows) %do% {
                     
                     df_i <- df_move %>% 
                       filter(species == usp[i])
                     
                     mcmc <- list_mcmc[[i]]
                     uid <- colnames(mcmc[[1]]) %>% 
                       { .[str_detect(., "^ID\\d*")] }
                     
                     df_p <- MCMCvis::MCMCsummary(mcmc) %>% 
                       as_tibble(rownames = "parm") %>% 
                       filter(str_detect(parm, "^ID*")) %>% 
                       mutate(y = names(list_mcmc)[i],
                              uid = parm) %>% 
                       dplyr::select(uid,
                                     pred = `50%`,
                                     species = y) %>% 
                       left_join(df_i,
                                 by = c("uid", "species"))
                     
                     return(df_p)
                   }

# check convergence -------------------------------------------------------

## return max Rhat value for each species
## each element represents max Rhat for each species
v_rhat <- df_est$Rhat

## print max Rhat across species
print(max(v_rhat))

# export ------------------------------------------------------------------

list_mcmc <- lapply(list_mcmc, function(x) {
  MCMCvis::MCMCchains(x) %>% 
    { .[, !str_detect(colnames(.), "^ID.*")]}
})

## results will not be exported unless converged
if (max(v_rhat) < 1.1) {
  saveRDS(list_mcmc, file = "data_fmt/output_move_mcmc.rds")
  saveRDS(df_est, file = "data_fmt/output_move.rds")
  saveRDS(df_pred, file = "data_fmt/output_move_pred.rds")
}
