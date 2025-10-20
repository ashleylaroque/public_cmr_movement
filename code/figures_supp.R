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

# Histogram of recaps by species ------------------------------------------

(fig_recap <- df_cmr %>% 
  filter(species %in% c("creek_chub", "bluehead_chub", "green_sunfish", "redbreast_sunfish")) %>% 
  mutate(Recap = ifelse(recap == "n", "No", "Yes")) %>% 
  ggplot(aes(x = recap, fill = Recap)) +
  scale_fill_manual(values = alpha(c("gray", "steelblue3"))) +
  geom_bar() +
  stat_count(geom = "text", aes(label = after_stat(count)), 
             vjust = -0.5,
             size = 7) +
  facet_wrap(~species,
             labeller = as_labeller(species.labs)) +
   ylim(0, 1300) +
   labs(x = "Recapture", y = "Count"))

ggsave(fig_recap, 
       filename = "output/fig_recap.pdf",
       height = 10,
       width = 12)


# Abundance of all captured species ---------------------------------------

(fig_abundance <- df_n %>%
  group_by(species) %>%
  tally(n) %>%
  ungroup() %>%
  ggplot(aes(reorder(species, -n), n)) +
  geom_col(fill = "darkcyan") +
   theme(axis.text.x = element_text( angle = 45, hjust = 1)) +
  xlab("Species") +
  ylab("Abundance"))

ggsave(fig_abundance, 
       filename = "output/fig_abundance.pdf",
       height = 10,
       width = 12)

# df_abund <- df_n %>% 
#   group_by(species) %>% 
#   tally(n) %>% 
#   mutate(total = sum(n),
#          percent = n / total) %>% 
#   filter(species %in% c("creek_chub", "bluehead_chub", "green_sunfish", "redbreast_sunfish")) %>% 
#   mutate(top4 = sum(n))


# Boxplot of mean body size -----------------------------------------------

(fig_size_dist <- ggplot(df_combined, aes(species, length0, fill = species)) +
  geom_boxplot() +
  scale_fill_manual(values=c("darkcyan", "maroon", "mediumpurple1", "steelblue3"))+
  theme(legend.position = "none") +
  xlab("Species") +
  ylab("Length (mm)"))

ggsave(fig_size_dist, 
       filename = "output/fig_size_dist.pdf",
       height = 10,
       width = 12)

# Histogram of total movement  ----------------------------------------

# visualize movement across all occasions

(fig_move <- ggplot(df_combined) +
  geom_histogram(aes(x = move), fill = "darkcyan") +
  geom_vline(xintercept = 0, linetype="dashed", color = "black", size=0.9) +
  facet_wrap( ~ species,
              scales = "free",
              labeller = as_labeller(species.labs))+
   scale_y_continuous(breaks = pretty_breaks())+
  theme(axis.text.x = element_text(angle = 45, vjust = .5),
        legend.position = "none") +
  labs(x = "Distance (m)", y = "Count"))

ggsave(fig_move, 
       filename = "output/fig_move.pdf",
       height = 9,
       width = 12)

# Histogram of total movement across seasons ------------------------------

# movement between seasons
season.labs <- c("0" = "Winter", "1" = "Summer")
(fig_total_move <- ggplot(df_combined) +
    geom_histogram(aes(x = abs_move), fill = "darkcyan") +
  facet_grid(season ~ species, 
              scales = "free",
              labeller = labeller(species = species.labs, season = season.labs)) +
    labs(x = "Distance (m)", y = "Frequency"))

ggsave(fig_total_move, 
       filename = "output/fig_total_move.pdf",
       height = 10,
       width = 12)

# Correlations of habitat -----------------------------------------------------------------

# chart.Correlation(df_combined[, c("depth_mean", "velocity_mean", "substrate_mean",
#                                          "area", "area_ucb", "mean_temp")],
#                          method="spearman",
#                          histogram=TRUE,
#                          cex = 10)

# PCA Habitat -------------------------------------------------------------

# cmr.pca <- prcomp(df_combined[,c("depth_mean", "velocity_mean", "substrate_mean", 
#                                  "area", "area_ucb", "mean_temp")], 
#                    center = TRUE, 
#                    scale. = TRUE) 
# summary(cmr.pca)
# autoplot(cmr.pca, data = df_combined,
#        loadings = TRUE, loadings.colour = '#20A387FF',
#        loadings.label = TRUE, loadings.label.colour = 'mediumpurple1')
# 


