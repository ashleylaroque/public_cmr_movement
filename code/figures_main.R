

rm(list = ls())

# Load Data ---------------------------------------------------------------

source(here::here("code/library.R"))
source(here::here("code/format_cmr.R"))
source(here::here("code/format_habitat.R"))

df_combined <- readRDS("data_formatted/data_combined.rds") %>% 
  mutate(move = (section1 - section0) * 10, 
         abs_move = abs(move), # generate absolute movement for figures
         month = format(datetime0, "%m") %>% 
           as.numeric(month),
         season = ifelse(between(month, 4, 9),
                         yes = 1, # summer
                         no = 0),
         log_length = log(length0)) %>% 
  filter(species %in% c("bluehead_chub",
                        "creek_chub",
                        "green_sunfish",
                        "redbreast_sunfish"))# comes from 'run_model_move'

df_output <- readRDS("data_formatted/output_move.rds") %>% 
  bind_rows() %>% 
  rename(species = "y",
         median = "50%" ,
         upper95 = "97.5%" ,
         lower95 = "2.5%") %>% 
  drop_na(var) %>% 
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
  axis.text.y = element_text(size = 13))

theme_set(plt_theme)

pd <- position_dodge(0.3)

species.labs <- c("Bluehead Chub", "Creek Chub", "Green Sunfish", "Redbreast Sunfish")
names(species.labs) <- c("bluehead_chub", "creek_chub", "green_sunfish", "redbreast_sunfish")

opp.labs <- c("Density Bluehead Chub", "Density Creek Chub", "Density Green Sunfish", "Density Redbreast Sunfish")
names(opp.labs) <- c("adj_density_bluehead_chub", "adj_density_creek_chub" ,"adj_density_green_sunfish", "adj_density_redbreast_sunfish")
strip1 <- strip_themed(background_x = elem_list_rect(fill = c("darkcyan", "maroon", "mediumpurple1", "steelblue3")))

# Model estimate ----------------------------------------------------------

dat_fig <- df_output %>% 
  filter(str_detect(para, "b")) %>% 
  mutate(para = case_when(var == '(Intercept)' ~ 'Intercept',
                          var == 'log_length' ~ 'log Length',
                          var == 'mean_temp' ~ 'Mean Temperature',
                          var == 'velocity_mean' ~ "Velocity",
                          var == 'area_ucb' ~ 'Habitat Refuge Area',
                          var == 'adj_density_creek_chub' ~ 'Density Creek Chub',
                          var == 'adj_density_bluehead_chub' ~ 'Density Bluehead Chub',
                          var == 'adj_density_green_sunfish' ~ 'Density Green Sunfish',
                          var == 'adj_density_redbreast_sunfish' ~ 'Density Redbreast Sunfish')) %>% 
  mutate(para = factor(para, levels = rev(unique(para)))) %>% 
  rowwise() %>% 
  mutate(sig = ifelse(between(0, lower95, upper95), 
                      "no",
                      "yes")) 

#saveRDS(dat_fig, file = "data_formatted/data_est.rds")


# estimates faceted by species 

(fig_est <- dat_fig %>% 
  ggplot(aes(x = median, y = para, color = factor(sig))) +
  geom_errorbar(aes(xmin = lower95, xmax = upper95),
                width = 0,
                position = pd) +
  geom_point(position = pd) +
  geom_vline(xintercept = 0,
             lty = "solid",
             col = "gray") +
  scale_color_manual(values = c("gray", "steelblue3")) + 
  labs(y = NULL,
       x = "Estimate") +
    theme_set(plt_theme) +
  theme(legend.position = "none") +
  facet_wrap( ~ species, nrow = 2, ncol = 2, 
              labeller = labeller(species = species.labs)))

ggsave(fig_est,
       filename = "output/fig_est.pdf",
       height = 9,
       width = 12)


# Effect of Body Size ---------------------------------------------------

x_name <- df_output %>% 
  filter(var != "(Intercept)") %>% 
  pull(var) %>% 
  unique()

df_y <- foreach(k = usp, .combine = bind_rows) %do% {
  
  v_b <- df_output %>% 
    filter(species == k) %>% 
    pull(median)
  
  df_x <- foreach(v = x_name,
                  .combine = bind_rows) %do% {
                    bid <- which(c("(Intercept)", x_name) == v)        
                    
                    ## rename focused predictor name to `x`
                    df_v <- df_combined %>%
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
                             y = (exp(log_sigma) * sqrt(2)) / sqrt(pi),
                             focus = v,
                             move = exp(log_sigma))
                    
                    ## safer to specify what is a return object
                    return(cout)
                  }
  
}

df_fig <- df_y %>% 
  left_join(df_output,
            by = c("species",
                   "focus" = "var")) %>% 
  rowwise() %>% 
  mutate(sig = ifelse(between(0, lower95, upper95), 
                      "no",
                      "yes")) %>% 
  ungroup() 

(fig_size <- df_combined %>%
    drop_na(section1) %>% # figure only includes recap
    ggplot(aes(x = log_length,
               y = abs_move / intv,
               color = species)) +
    geom_point(alpha = 0.5) + ## add points
    geom_line(data = df_fig %>% ## draw line
                filter(focus == "log_length"),
              aes(x = x_value,
                  y = qnorm(.95 , mean = 0, sd = move),
                  linetype = sig)) +
    scale_linetype_manual(values = c("yes" = "solid",
                                     "no" = "dashed")) +
    scale_color_manual(values=c("darkcyan", "maroon", "mediumpurple1", "steelblue3"),
                       name="Species") +
    geom_area(data = df_fig %>% ## draw shaded area
                filter(focus == "log_length"),
              aes(x = x_value,
                  y = qnorm(.95 , mean = 0, sd = move),
                  alpha = sig,
                  fill = species,
                  linetype = sig)) +
    scale_alpha_manual(values = c("yes" = .5,
                                  "no" = 0)) +
    # geom_area(data = df_fig %>% ## draw shaded area
    #             filter(focus == "log_length"),
    #           aes(x = x_value,
    #               y = qnorm(.75, mean = 0, sd = move),
    #               alpha = sig,
    #               fill = species,
    #               linetype = sig)) +
    # scale_alpha_manual(values = c("yes" = .7,
    #                               "no" = 0)) +
    scale_fill_manual(values=c("darkcyan", "maroon", "mediumpurple1", "steelblue3")) +
    facet_wrap2(~ species,
                scales = "free",
                strip = strip1,
                labeller = labeller(species = species.labs)) +
    labs(x= "ln Length at Capture (ln mm)",
         y= "Standard Deviation of Movement (m/day)") +
    theme_set(plt_theme) +
    theme(legend.position = "none",
          strip.text = element_text(color = 'white'))) 

ggsave(fig_size,
       filename = "output/fig_size.pdf",
       height = 8,
       width = 9)

# Effect of Density -------------------------------------------------------

(fig_den <- df_combined %>% 
   select(species,
          intv,
          abs_move,
          adj_density_bluehead_chub,
          adj_density_creek_chub, 
          adj_density_green_sunfish,
          adj_density_redbreast_sunfish) %>% 
   pivot_longer(cols = starts_with("adj_"),
                names_to = "opponent", 
                values_to = "density") %>% 
   drop_na(abs_move) %>% 
   ggplot(aes(x = density,
              y = abs_move / intv, 
              color = species)) +
   geom_point(alpha = 0.5) + ## add points
   geom_line(data = df_fig %>% ## draw line
               filter(str_detect(focus, "adj_density")) %>% 
               rename(opponent = "focus"),
             aes(x = x_value,
                 y = qnorm(.95, mean = 0, sd = move),
                 color = species,
                 linetype = sig))  +
   scale_linetype_manual(values = c("yes" = "solid",
                                    "no" = "dashed")) +
   geom_area(data = df_fig %>% ## draw shaded area
               filter(str_detect(focus, "adj_density")) %>% 
               rename(opponent = "focus"),
             aes(x = x_value,
                 y = qnorm(.95, mean = 0, sd = move),
                 alpha = sig,
                 fill = species,
                 linetype = sig)) +
   scale_fill_manual(values=c("darkcyan", "maroon", "mediumpurple1", "steelblue3")) +
   scale_alpha_manual(values = c("yes" = .5,
                                 "no" = 0)) +
   # geom_area(data = df_fig %>% ## draw shaded area
   #             filter(str_detect(focus, "adj_density")) %>% 
   #             rename(opponent = "focus"),
   #           aes(x = x_value,
   #               y = qnorm(.75, mean = 0, sd = move),
   #               alpha = sig,
   #               fill = species,
   #               linetype = sig)) +
   # scale_fill_manual(values=c("darkcyan", "maroon", "mediumpurple1", "steelblue3")) +
   # scale_alpha_manual(values = c("yes" = .7,
   #                               "no" = 0)) +
   facet_grid(rows = vars(species),
              cols = vars(opponent),
              scales = "free",
              switch = "x",  # use switch = "y" to swap strip to the left side
              labeller = labeller(species = species.labs, opponent = opp.labs)) +
   scale_color_manual(values=c("darkcyan", "maroon", "mediumpurple1", "steelblue3"), 
                      name = "species") +
   labs(x= expression("Density (n /"~m^2*")"),
        y= "Standard Deviation of Movement (m / day)") +
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
       height = 10,
       width = 12)


