# Tables for Supporting Information

rm(list = ls())

# Load Data ---------------------------------------------------------------

source(here::here("code/library.R"))
source(here::here("code/format_cmr.R"))
source(here::here("code/format_habitat.R"))
df_est <- readRDS("data_fmt/data_est.rds")
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


# Mean size of each tagged species (including replicates) -----------------

tab_size <- df_combined %>% 
  group_by(species) %>% 
  summarize(Mean = mean(length0),
            "Standard Deviation" = sd(length0)) %>% 
  rename("Species" = "species") %>% 
  mutate(Species = str_to_sentence(Species) %>% 
           str_replace("_", " ")) 

## export
print(xtable(tab_size,
             caption = "Mean and standard deviation total length (mm) for each target species.",
             label = "tab:size"),
      tabular.environment = "tabular", # use \begin{tabular}
      sanitize.text.function = function(x) x, # for math mode
      include.rownames = FALSE,
      caption.placement = "top",
      file = "tex/table_size.tex")

# Mean density of each tagged species  ------------------------------------

tab_den <- df_combined %>% 
  select(adj_density_bluehead_chub, adj_density_creek_chub,
         adj_density_green_sunfish, adj_density_redbreast_sunfish) %>% 
  pivot_longer(cols = starts_with("adj_"),
               names_to = "opponent", 
               values_to = "density") %>% 
  group_by(opponent) %>% 
  summarize(Mean = mean(density),
            "Standard Deviation" = sd(density)) %>% 
  rename("Species" = "opponent") %>% 
  mutate(Species = case_when(Species == "adj_density_bluehead_chub" ~ "Bluehead chub",
                             Species == "adj_density_creek_chub" ~ "Creek chub",
                             Species == "adj_density_green_sunfish" ~ "Green sunfish",
                             Species == "adj_density_redbreast_sunfish" ~ "Redbreast sunfish"))

## export
print(xtable(tab_den,
             caption = "Mean and standard deviation density (n /m^2) of each each target species.",
             label = "tab:density"),
      tabular.environment = "tabular", # use \begin{tabular}
      sanitize.text.function = function(x) x, # for math mode
      include.rownames = FALSE,
      caption.placement = "top",
      file = "tex/table_density.tex")



# Table of Recaptures -------------------------------------------------------
 
tab_cap <- df_combined %>% 
  group_by(species) %>%
  summarise("Unique" = n_distinct(tag_id), # all unique individuals
            "Unique Recaptures" = n_distinct(tag_id[!is.na(length1)]), # unique recaps
            "Replicate Captures" = n(), # all captured (including replicates)
            "Replicate Recaptures" = sum(!is.na(length1))) %>% # recaptured consecutive individuals (replicates)
          ungroup() %>% 
  rename("Species" = "species") %>% 
  mutate(Species = str_to_sentence(Species) %>% 
           str_replace("_", " ")) 

## export
print(xtable(tab_cap,
             caption = "The number of unique individuals, unique recaptures, captured replicates, and recaptured replicates listed from left to right for each target species.",
             label = "tab:capture"),
      tabular.environment = "tabular", # use \begin{tabular}
      sanitize.text.function = function(x) x, # for math mode
      include.rownames = FALSE,
      caption.placement = "top",
      file = "tex/table_capture.tex")


# Table of Coefficients ---------------------------------------------------

tab_coef <- df_est %>% 
  select(species, para, lower95, upper95, median) %>% 
  rename("Effect" = "para",
         "Lower 95\\% CI" = "lower95",
         "Upper 95\\% CI" = "upper95",
         "Species" = "species",
         "Estimate" = "median") %>% 
  mutate(Species = str_to_sentence(Species) %>% 
         str_replace("_", " ")) %>% 
  ungroup() %>% 
  mutate(Species = ifelse(duplicated(Species), NA, Species)) 

## export
print(xtable(tab_coef,
             caption = "Parameter estimates of the movement model. Median estimates and their associated 95\\% credible intervals (95\\% CI) are reported.",
             label = "tab:coefficients"),
      tabular.environment = "tabular", # use \begin{tabular}
      sanitize.text.function = function(x) x, # for math mode
      include.rownames = FALSE,
      caption.placement = "top",
      file = "tex/table_coefficients.tex")


# Table of Habitat Variables -----------------------------------------------

tab_hab <- df_h_sec %>% 
  pivot_longer(cols = -c(occasion, section),
               names_to = "habitat_variable",
               values_to = "value") %>% 
  group_by(habitat_variable) %>% 
  summarise(Mean = mean(value),
            "Standard Deviation" = sd(value)) %>% 
  mutate(habitat_variable = case_when(habitat_variable == "area" ~ "Mean Section Area m" %p% supsc('2'),
                                      habitat_variable == "width" ~ "Mean Width m",
                                      habitat_variable == "section_length" ~ "Section Length m",
                                      habitat_variable == "area_pool" ~ "Pool Area m" %p% supsc('2'),
                                      habitat_variable == "area_riffle" ~ "Riffle Area m" %p% supsc('2'),
                                      habitat_variable == "area_run" ~ "Run Area m" %p% supsc('2'),
                                      habitat_variable == "area_ucb" ~ "Habitat Refuge Area m" %p% supsc('2'),
                                      habitat_variable == "depth_mean" ~ "Mean Depth cm",
                                      habitat_variable == "velocity_mean" ~ "Mean Velocity m/s",
                                      habitat_variable == "substrate_mean" ~ "Mean Substrate mm")) %>% 
  rename("Habitat Metric" = "habitat_variable") 


## export
print(xtable(tab_hab,
             caption = "Mean and standard deviation across sections and occasions for each habitat variable.",
             label = "tab:habitat"),
      tabular.environment = "tabular", # use \begin{tabular}
      sanitize.text.function = function(x) x, # for math mode
      include.rownames = FALSE,
      caption.placement = "top",
      file = "tex/table_habitat.tex")


# Table for Detection Probabilities ---------------------------------------

options(xtable.comment = FALSE)

df_p <- readRDS("data_fmt/output_cjs.rds") %>% 
  bind_rows() %>% 
  filter(str_detect(para, "zeta")) %>% 
  mutate(species = str_to_sentence(species) %>% 
           str_replace("_", " "),
         season = ifelse(para == "zeta[1]", "Winter", "Summer"),
         estimate = paste0(sprintf("%.2f", `50%`),
                           " [",
                           sprintf("%.2f", `2.5%`),
                           " -- ",
                           sprintf("%.2f", `97.5%`),
                           "]")) %>% 
  select(Species = species,
         Season = season,
         Estimate = estimate) %>% 
  mutate(Species = ifelse(duplicated(Species), NA, Species))

## export
print(xtable(df_p,
             caption = "Seasonal detection probabilities estimated using the spatial Cormack-Jolly-Seber (CJS) model, presented as median estimates with corresponding 95\\% credible intervals in brackets.",
             label = "tab:detection"),
      tabular.environment = "tabular", # use \begin{tabular}
      sanitize.text.function = function(x) x, # for math mode
      include.rownames = FALSE,
      caption.placement = "top",
      file = "tex/table_detection.tex")










         
