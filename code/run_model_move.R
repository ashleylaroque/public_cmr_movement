
#' Description:
#' This script runs modified dispersal-observation model for predicting movement with density and body size

# setup -------------------------------------------------------------------

rm(list = ls())
source("code/library.R")
source("code/function.R")

# data --------------------------------------------------------------------

## format data
df_move0 <- readRDS("data_formatted/data_move.rds") %>% 
  drop_na(section0)

df_zeta <- readRDS("data_formatted/data_detection.rds") # comes from 'run_model_scjs'
df_season <- readRDS("data_formatted/data_season.rds") # comes from 'run_model_scjs'
df_hobo <- readRDS("data_formatted/data_water_hobo.rds")   # comes from 'format_water_level'


df_den <- readRDS("data_formatted/data_density.rds") %>% 
  left_join(df_season, by = "occasion") %>% 
  mutate(season= case_when(season == 0 ~ "winter",
                           season == 1 ~ "summer")) %>% 
  left_join(df_zeta,
            by = c("species", "season")) %>% 
  mutate(adj_density = (density / estimate))

df_den_adj <- df_den %>% 
  pivot_wider(id_cols = c(occasion, section, area),
              names_from = species,
              values_from = c(n, adj_density))

df_h <- readRDS("data_formatted/data_habitat.rds") %>% 
  dplyr::select(-area)

## combine movement, density, and habitat
df_combined <- df_move0 %>% # movement dataframe
  left_join(df_den_adj, # add density values
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

saveRDS(df_combined, file = "data_formatted/data_combined.rds")


# run jags ----------------------------------------------------------------

usp <- c("green_sunfish",
         "redbreast_sunfish",
         "creek_chub",
         "bluehead_chub",
         #"bluegill", doesn't converge
         "striped_jumprock")%>% 
  sort()

## mcmc setup ####
n_ad <- 1500
n_iter <- 30000
n_thin <- max(3, ceiling(n_iter / 1000))
n_burn <- ceiling(max(10, n_iter / 2))
n_sample <- ceiling(n_iter / n_thin)
n_chain <- 3

list_est <- foreach(x = usp) %do% {
  
  df_i <- filter(df_combined, species == x)
  
  ## data for jags
  list_jags <- with(df_i,
                    list(Y = y,
                         X1 = section1 * 10 - 5,
                         X0 = section0 * 10 - 5, 
                         Nsample = nrow(df_i),
                         Intv = intv))
  
  ## select predictors
  X <- df_i %>% 
    mutate(log_length = log(length0)) %>% 
    dplyr::select(log_length, # log-trans total length of individual
                  area_ucb,   # area of undercut bank coverage
                  mean_temp,  # temp
                  velocity_mean,
                  adj_density_bluehead_chub, # seasonally adjusted density
                  adj_density_creek_chub, 
                  adj_density_green_sunfish,
                  adj_density_redbreast_sunfish) %>% 
    mutate(across(.cols = c(log_length,
                            area_ucb,
                            mean_temp, 
                            velocity_mean,
                            starts_with("adj_density")),
                  .fns = function(x) c(scale(x)))) %>% 
    model.matrix(~., data = .)
  
  list_jags$X <- X
  list_jags$K <- ncol(X)
  
  ## run.jags arguments
  ## - initial values
  n_chain <- 3
  
  inits <- replicate(n_chain,
                     list(.RNG.name = "base::Mersenne-Twister",
                          .RNG.seed = NA),
                     simplify = FALSE)
  
  for (j in 1:n_chain) inits[[j]]$.RNG.seed <- 100 * j
  
  ## - parameters to be monitored
  para <- c("b", "p")
  
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
  # check into priors after if it still doesn't converge 
  
  MCMCvis::MCMCsummary(post$mcmc) %>% 
    as_tibble(rownames = "para") %>% 
    mutate(y = x,
           var = c(colnames(X), NA)) %>%
    relocate(var)
}


# check convergence -------------------------------------------------------

## return max Rhat value for each species
## each element represents max Rhat for each species
v_rhat <- sapply(list_est,
                 function(data) max(data$Rhat))

## print max Rhat across species
print(max(v_rhat))

# export ------------------------------------------------------------------

## results will not be exported unless converged
if (max(v_rhat) < 1.1) {
  saveRDS(list_est, file = "data_formatted/output_move.rds")
}

print(list_est)
