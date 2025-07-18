
rm(list = ls())
source(here::here("code/library.R"))

# Load Data ---------------------------------------------------------------

source(here::here("code/format_cmr.R"))
source(here::here("code/format_habitat.R"))

## raw movement data to which movement model was fitted
df_move <- readRDS("data_fmt/data_combined.rds") %>% 
  mutate(move = (section1 - section0) * 10, 
         abs_move = abs(move), # generate absolute movement for figures
         month = format(datetime0, "%m") %>% 
           as.numeric(month),
         season = ifelse(between(month, 4, 9),
                         yes = 1, # summer
                         no = 0),
         log_length = log(length0),
         area_ucb = sqrt(area_ucb)) %>% 
  filter(species %in% c("bluehead_chub",
                        "creek_chub",
                        "green_sunfish",
                        "redbreast_sunfish"))# comes from 'run_model_move'

## mcmc samples
list_mcmc <- readRDS("data_fmt/output_move_mcmc.rds")

df_mcmc <- lapply(X = seq_len(length(list_mcmc)),
                  FUN = function(i) {
                    
                    as_tibble(list_mcmc[[i]]) %>%
                      pivot_longer(cols = everything(),
                                   names_to = "parm",
                                   values_to = "value") %>%
                      arrange(parm) %>%
                      mutate(species = names(list_mcmc)[i])
                    
                  }) %>% 
  bind_rows()

## mcmc summary
df_output <- readRDS("data_fmt/output_move.rds") %>% 
  filter(species %in% c("bluehead_chub",
                        "creek_chub",
                        "green_sunfish",
                        "redbreast_sunfish")) # comes from 'run_model_move'

# Plot Theme --------------------------------------------------------------

# separate target species for use in loops
usp <- c("bluehead_chub",
         "creek_chub",
         "green_sunfish",
         "redbreast_sunfish") %>%
  sort()

## plot theme
plt_theme <- theme_bw() + theme(
  plot.background = element_blank(),
  
  panel.background = element_rect(grey(0.99)),
  panel.border = element_rect(),
  
  panel.grid = element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  
  panel.grid.major.x = element_blank(),
  panel.grid.major.y = element_blank(),
  panel.grid.minor.x = element_blank(),
  panel.grid.minor.y = element_blank(),
  
  strip.background = element_blank(),
  strip.text.x = element_text(size = 16),
  strip.text.y = element_text(size = 16),
  axis.title = element_text(size = 16),
  axis.text.x = element_text(size = 13),
  axis.text.y = element_text(size = 13),
  
  plot.margin = unit(c(1, 1, 1.5, 1.5), "cm")
)

theme_set(plt_theme)

pd <- position_dodge(0.3)

species.labs <- c("Bluehead chub",
                  "Creek chub",
                  "Green sunfish",
                  "Redbreast sunfish")
names(species.labs) <- c("bluehead_chub", 
                         "creek_chub",
                         "green_sunfish",
                         "redbreast_sunfish")

opp.labs <- c("Bluehead chub", 
              "Creek chub", 
              "Green sunfish", 
              "Redbreast sunfish")
names(opp.labs) <- c("w_density_bluehead_chub",
                     "w_density_creek_chub",
                     "w_density_green_sunfish",
                     "w_density_redbreast_sunfish")

strip1 <- strip_themed(background_x = elem_list_rect(fill = c("darkcyan",
                                                              "maroon",
                                                              "mediumpurple1",
                                                              "steelblue3")))

# Model estimate ----------------------------------------------------------

## define function for predictor labeling
f_label <- function(x) {
  sp_pattern <- df_mcmc %>% 
    pull(species) %>% 
    unique() %>% 
    paste(collapse = "|")
  
  str_extract(x, pattern = sp_pattern) %>% 
    str_replace_all(pattern = "_", replacement = " ")
}

## dataframe for plot
## - omit non-focus parameters from the figure
df_mcmc_plot <- df_mcmc %>% 
  filter(!(parm %in% c("p",
                       "nu",
                       "mean_temp",
                       "velocity_mean",
                       "area_ucb",
                       "(Intercept)")),
         !str_detect(parm, "D\\[.*\\]")) %>% 
  mutate(var_label = case_when(str_detect(parm, "w_density") ~ 
                                 paste("Density",  f_label(parm)),
                               parm == "log_length" ~ "log(Body length)") %>% 
           factor(levels = rev(c("log(Body length)",
                                 "Density bluehead chub",
                                 "Density creek chub",
                                 "Density green sunfish",
                                 "Density redbreast sunfish"))),
         sp_label = str_replace_all(species, "_", " ") %>% 
           str_to_sentence()
  ) %>% 
  group_by(sp_label,
           var_label) %>% 
  mutate(med = median(value),
         upper = quantile(value, 0.975),
         lower = quantile(value, 0.025),
         p_pos = mean(value > 0),
         prob = max(p_pos, 1 - p_pos)
  ) %>% 
  ungroup() 

## density ridge figure
fig_est <- df_mcmc_plot %>% 
  ggplot(aes(x = value,
             y = var_label)) +
  geom_density_ridges(scale = 0.9, 
                      aes(fill = prob),
                      # quantile_lines = TRUE,
                      # quantiles = 2,
                      color = grey(0.3, 0.8)) +
  geom_segment(aes(x = lower,
                   xend = upper),
               color = grey(0.3, 0.8),
               linewidth = 0.4,
               lineend = "round") +
  geom_point(aes(x = med),
             color = grey(0.3, 0.8)) +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             alpha = 0.5) +
  scale_fill_gradient(low = "white",
                      high = "salmon") +
  facet_wrap(~ sp_label,
             scales = "free") +
  theme_ridges() +
  theme(strip.background = element_blank(),
        strip.text = element_text(margin = margin(b = 10))) +
  labs(fill = "Posterior prob.",
       x = "Estimate",
       y = "Predictor")

## export
ggsave(fig_est,
       filename = "output/fig_est.pdf",
       height = 9,
       width = 12)


# prepare data frame for predicted values ---------------------------------

x_name <- df_output %>% 
  filter(str_detect(parm, "log_length|w_density")) %>% 
  pull(parm) %>% 
  unique()

qt_custum <- function(p, df, mu = 0, sigma = 1) {
  qt(p, df) * sigma + mu
}

df_y <- foreach(k = usp, .combine = bind_rows) %do% {
  
  v_b <- df_output %>% 
    filter(species == k,
           parm %in% c("(Intercept)", x_name)) %>% 
    pull(median)
  
  nu <- df_output %>% 
    filter(species == k,
           parm == "nu") %>% 
    pull(median)
  
  df_x <- foreach(v = x_name,
                  .combine = bind_rows) %do% {
                    
                    bid <- which(c("(Intercept)", x_name) == v)        
                    
                    ## rename focused predictor name to `x`
                    df_v <- df_move %>%
                      filter(species == k) %>% 
                      rename(x = all_of(v))
                    
                    ## calculate mean and sd for standardization
                    ## - this calculation must be done BEFORE dropping rows
                    ## - otherwise mean & sd are different from what was used in the model
                    mu_x <- mean(df_v$x)
                    sd_x <- sd(df_v$x)
                    
                    cout <- df_v %>% 
                      drop_na(section1) %>% 
                      reframe(x_value = seq(min(x, na.rm = T),
                                            max(x, na.rm = T),
                                            length = 100),
                              scl_x = (x_value - mu_x) / sd_x,
                              species = rep(unique(species))) %>% 
                      mutate(log_sigma = v_b[1] + v_b[bid] * scl_x, 
                             y50 = qt_custum(p = 0.75, # (0.75 - 0.5) * 2 = 0.50
                                             df = nu,
                                             sigma = exp(log_sigma)),
                             y90 = qt_custum(p = 0.95, # (0.95 - 0.5) * 2 = 0.90
                                             df = nu,
                                             sigma = exp(log_sigma)),
                             focus = v)
                    
                    ## safer to specify what is a return object
                    return(cout)
                  }
  
}

## prob_level - this sets whether predicted values show up in the figure
## Pr(>0.95) for now
## tweak the threshold if you want to change the figure
df_fig <- df_y %>% 
  left_join(df_output,
            by = c("species",
                   "focus" = "parm")) %>% 
  rowwise() %>% 
  mutate(prob_level = case_when(prob >= 0.95 ~ "high",
                                prob < 0.95 ~ "low")) %>% 
  ungroup() 

df_pred <- readRDS("data_fmt/output_move_pred.rds") %>% 
  mutate(log_length = log(length0),
         obs = !is.na(section1)) %>% 
  rename(abs_move = pred)
  

# Effect of Body Size ---------------------------------------------------

df_size <- df_pred %>% 
  group_by(species) %>% 
  filter(between(log_length,
                 min(log_length[obs]),
                 max(log_length[obs]))) %>% 
  ungroup()

(fig_size <- df_size %>%
   filter(obs) %>% # figure only includes recap
   ggplot(aes(x = log_length,
              y = abs_move,
              color = species)) +
    geom_point(size = 1) + ## add points
    scale_color_manual(values=c("darkcyan",
                               "maroon",
                               "mediumpurple1",
                               "steelblue3"),
                      name="Species") +
   geom_area(data = df_fig %>% ## draw shaded area
               filter(focus == "log_length"),
             aes(x = x_value,
                 y = y50,
                 alpha = prob_level,
                 fill = species),
             color = NA) +
   geom_area(data = df_fig %>% ## draw shaded area
               filter(focus == "log_length"),
             aes(x = x_value,
                 y = y90,
                 alpha = prob_level,
                 fill = species),
             color = NA) +
   scale_alpha_manual(values = c("high" = .5,
                                 "low" = 0)) +
   scale_fill_manual(values=c("darkcyan",
                              "maroon",
                              "mediumpurple1",
                              "steelblue3")) +
   scale_x_continuous(labels = label_number(accuracy = 0.2)) +
   facet_wrap2(~ species,
               scales = "free",
               strip = strip1,
               labeller = labeller(species = species.labs)) +
   labs(x= "ln(Body length at capture) (ln mm)",
        y= "Absolute movement (m/day)") +
   theme_set(plt_theme) +
   theme(legend.position = "none",
         strip.text = element_text(color = 'white'))) 

ggsave(fig_size,
       filename = "output/fig_size.pdf",
       height = 8,
       width = 9)

# Effect of Density -------------------------------------------------------

(fig_den <- df_move %>% 
   select(species,
          intv,
          abs_move,
          w_density_bluehead_chub,
          w_density_creek_chub, 
          w_density_green_sunfish,
          w_density_redbreast_sunfish) %>% 
   pivot_longer(cols = starts_with("w_density"),
                names_to = "opponent", 
                values_to = "density") %>% 
   drop_na(abs_move) %>% 
   ggplot(aes(x = density,
              y = abs_move / intv, 
              color = species)) +
   geom_point(size = 0.8) + ## add points
   geom_area(data = df_fig %>% ## draw shaded area
               filter(str_detect(focus, "w_density")) %>% 
               rename(opponent = "focus"),
             aes(x = x_value,
                 y = y50,
                 alpha = prob_level,
                 fill = species),
             color = NA) +
   geom_area(data = df_fig %>% ## draw shaded area
               filter(str_detect(focus, "w_density")) %>% 
               rename(opponent = "focus"),
             aes(x = x_value,
                 y = y90,
                 alpha = prob_level,
                 fill = species),
             color = NA) +
   scale_fill_manual(values=c("darkcyan",
                              "maroon",
                              "mediumpurple1",
                              "steelblue3")) +
   scale_alpha_manual(values = c("high" = .5,
                                 "low" = 0)) +
   facet_grid2(rows = vars(species),
               cols = vars(opponent),
               independent = "all",
               scales = "free",
               switch = "x",  # use switch = "y" to swap strip to the left side
               labeller = labeller(species = species.labs,
                                  opponent = opp.labs)) +
   scale_color_manual(values = c("darkcyan",
                                 "maroon",
                                 "mediumpurple1",
                                 "steelblue3"), 
                      name = "species") +
   labs(x= expression("Density (n /"~m^2*")"),
        y= "Absolote movement (m / day)") +
   theme_set(plt_theme) +
   theme(legend.position = "none",
         strip.background = element_rect(color = "black"),
         strip.text = element_text(color = 'white'),
         strip.placement = "outside"))

fig_density <- ggplot_gtable(ggplot_build(fig_den))
strip_both <- which(grepl('strip-', fig_density$layout$name))
fills <- c("darkcyan", "maroon", "mediumpurple1", "steelblue3", "darkcyan", "maroon", "mediumpurple1", "steelblue3")
k <- 1
for (i in strip_both) {
  j <- which(grepl('rect', fig_density$grobs[[i]]$grobs[[1]]$childrenOrder))
  fig_density$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  k <- k+1
}
grid.draw(fig_density)

ggsave(fig_density,
       filename = "output/fig_density.pdf",
       height = 12,
       width = 13)


