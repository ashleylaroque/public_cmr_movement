# Figures for supporting information

rm(list = ls())

# Load Data ---------------------------------------------------------------

source(here::here("code/library.R"))
source(here::here("code/format_cmr.R"))
source(here::here("code/format_habitat.R"))

df_combined <- readRDS("data_fmt/data_combined.rds") %>% 
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
                        "redbreast_sunfish"))

# Set theme ---------------------------------------------------------------

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
  strip.text.x = element_text(size = 25),
  strip.text.y = element_text(size = 25),
  axis.title = element_text(size = 25),
  axis.text.x = element_text(size = 20),
  axis.text.y = element_text(size = 20),
  legend.title = element_text(size = 20),
  legend.text = element_text(size = 20))

theme_set(plt_theme)

species.labs <- c("Bluehead Chub", "Creek Chub", "Green Sunfish", "Redbreast Sunfish")
names(species.labs) <- c("bluehead_chub", "creek_chub", "green_sunfish", "redbreast_sunfish")

# Abundance of all captured species ---------------------------------------

df_pro_abund <- df_n %>%
  group_by(species) %>%
  reframe(abund = sum(n)) %>% 
  ungroup() %>%  
  mutate(total = sum(abund),
         proportion = abund/total, 
         percent = round(((abund/total) * 100), digits = 1),
         sd = sd(proportion)) 

(fig_abundance <- df_pro_abund %>% 
    ggplot(aes(reorder(species, -percent), percent)) +
    geom_col(fill = "maroon") +
    geom_text(aes(label = percent), 
              color = "black", 
              vjust = 0,
              hjust = -.1,
              size = 5,
              angle = 45) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    ylim(0, 32) +
    xlab("Species") +
    ylab("Percent Abundance"))

ggsave(fig_abundance, 
       filename = "output/fig_abundance.pdf",
       height = 10,
       width = 12)


# Histogram of recaps by species ------------------------------------------
df_recap <- df_combined %>%
  mutate(recap = ifelse(is.na(section1), "No", "Yes"))

(fig_recap <- df_recap %>%
    ggplot(aes(x = recap, fill = recap)) +
    scale_fill_manual(values = alpha(c("gray", "maroon"))) +
    geom_bar() +
    stat_count(geom = "text", aes(label = after_stat(count)),
               vjust = -0.5,
               size = 7) +
    facet_wrap(~ species,
               labeller = as_labeller(species.labs)) +
    theme(legend.position = "none") +
    ylim(0, 1500) +
    labs(x = "Recapture", y = "Count"))

ggsave(fig_recap,
       filename = "output/fig_recap.pdf",
       height = 10,
       width = 12)

# Boxplot of body size -----------------------------------------------

(fig_size_dist <- ggplot(df_recap, aes(species, length0, fill = recap)) +
  geom_boxplot() +
  scale_fill_manual(values=c("gray", "maroon"))+
  scale_y_continuous(breaks = pretty_breaks()) +
  scale_x_discrete(labels = species.labs) +
  theme(legend.position = "right") +
  labs(fill = "Recapture") +
  xlab("Species") +
  ylab("Length (mm)"))

ggsave(fig_size_dist,
       filename = "output/fig_size_dist.pdf",
       height = 10,
       width = 12)

# Histogram of total movement  ----------------------------------------

# visualize movement across all occasions

# (fig_move <- ggplot(df_combined) +
#   geom_histogram(aes(x = move), fill = "darkcyan") +
#   geom_vline(xintercept = 0, linetype="dashed", color = "black", size=0.9) +
#   facet_wrap( ~ species,
#               scales = "free",
#               labeller = as_labeller(species.labs))+
#    scale_y_continuous(breaks = pretty_breaks())+
#   theme(axis.text.x = element_text(angle = 45, vjust = .5),
#         legend.position = "none") +
#   labs(x = "Distance (m)", y = "Count"))
# 
# ggsave(fig_move, 
#        filename = "output/fig_move.pdf",
#        height = 9,
#        width = 12)

# Histogram of total movement across seasons ------------------------------

# movement between seasons
season.labs <- c("0" = "Winter", "1" = "Summer")
(fig_total_move <- ggplot(df_combined) +
    geom_histogram(aes(x = abs_move), fill = "maroon") +
    #geom_vline(xintercept = 0, linetype="dashed", color = "black", size=0.9) +
  facet_grid(season ~ species, 
              scales = "free",
              labeller = labeller(species = species.labs, season = season.labs)) +
    scale_y_continuous(breaks = pretty_breaks()) +
    theme(axis.text.x = element_text(angle = 45, vjust = .5),
          legend.position = "none") +
    labs(x = "Absolute Distance (m)", y = "Count"))

ggsave(fig_total_move, 
       filename = "output/fig_total_move.pdf",
       height = 10,
       width = 14)


# Correlation of body size and density ------------------------------------
# df_com <- df_combined %>%
#   drop_na("section1") %>%
#    filter(species == "bluehead_chub") %>%
   #filter(w_density_bluehead_chub < max(w_density_bluehead_chub))
# 
# chart.Correlation(df_com[, c("length0",
#                                   "w_density_bluehead_chub",
#                                   "w_density_creek_chub",
#                                   "w_density_green_sunfish",
#                                   "w_density_redbreast_sunfish")],
#                          method = "pearson",
#                          histogram = TRUE,
#                          cex = 10)

# Correlations of habitat -----------------------------------------------------------------

chart.Correlation(df_combined[, c("sc_velocity",
                                  "sc_ucb",
                                  "sc_depth",
                                  "sc_sub")],
                         method="pearson",
                         histogram=TRUE,
                         cex = 10)




