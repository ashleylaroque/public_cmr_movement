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
         y = ifelse(is.na(section1), 0, 1)) %>% 
  group_by(occasion0) %>% 
  mutate(intv = ifelse(is.na(intv),
                       median(intv, na.rm = TRUE),
                       intv)) %>% 
  ungroup() %>% 
  select(-c(starts_with("n_")))

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
                  mean_temp,
                  w_density_bluehead_chub, # seasonally adjusted density
                  w_density_creek_chub,
                  w_density_green_sunfish,
                  w_density_redbreast_sunfish
    ) %>%
    mutate(across(.cols = everything(),
                  .fns = function(x) c(scale(x)))) %>%
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
                         return(chain)
                         
                       })
  
  return(named_mcmc)
}

names(list_mcmc) <- usp

## get summary estimates
list_est <- lapply(seq_len(length(list_mcmc)),
                   function(i) {
                     
                     mcmc <- list_mcmc[[i]]
                     MCMCvis::MCMCsummary(mcmc) %>% 
                       as_tibble(rownames = "parm") %>% 
                       mutate(p_neg = MCMCvis::MCMCpstr(mcmc,
                                                        func = function(x) mean(x < 0)) %>%
                                unlist(),
                              p_pos = 1 - p_neg) %>% 
                       mutate(y = names(list_mcmc)[i])
                     
                   })

# check convergence -------------------------------------------------------

## return max Rhat value for each species
## each element represents max Rhat for each species
v_rhat <- sapply(list_est,
                 function(data) {
                   data %>% 
                     filter(!str_detect(parm, "D\\[.*\\]")) %>% 
                     pull(Rhat) %>% 
                     max()
                 })

## print max Rhat across species
print(max(v_rhat))

# export ------------------------------------------------------------------

## results will not be exported unless converged
if (max(v_rhat) < 1.1) {
  saveRDS(list_mcmc, file = "data_fmt/output_move_mcmc.rds")
  saveRDS(list_est, file = "data_fmt/output_move.rds")
}

print(list_est)
