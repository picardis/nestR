library(readr)
library(dplyr)

# Load raw data ----

# Load dataset of trusted WOST data
wost_t_raw <- read_csv("~/Documents/PhD/Research/Wood Stork/WOST_repo/nesting/output/trusted_data_0917.csv")

# Keep columns of interest and rename
wost_t_raw <- wost_t_raw %>%
  select(burst, date, long, lat) %>%
  mutate(burst = gsub(" ", "-", burst))

# Load dataset of Jacksonville WOST data
wost_j_raw <- read_csv("~/Documents/PhD/Research/Wood Stork/WOST_repo/nesting/output/jax_known_data.csv")

# Keep columns of interest and rename
wost_j_raw <- wost_j_raw %>%
  select(burst, date, long, lat)

# Load dataset of Kestrel data
kestrels_raw <- read.csv("~/Documents/PhD/Research/Wood Stork/WOST_repo/nesting/output/LEKE/LEKE_nesters.csv",
                         stringsAsFactors = FALSE)

# Keep columns of interest and rename
kestrels_raw <- kestrels_raw %>%
  select(burst, date, long, lat) %>%
  as_tibble()

# Load dataset of Gull data
gulls_raw <- read_csv("~/Documents/PhD/Research/Wood Stork/WOST_repo/nesting/output/MEGU/MEGU_nesters.csv")

# Keep columns of interest and rename
gulls_raw <- gulls_raw %>%
  select(burst, date, long, lat)

# Create example datasets ----

# Select one Wood Stork among the trusted nesters
wost_pt1 <- wost_t_raw %>%
  filter(burst == "1134370-2013")

# Select one Wood Stork among the Jacksonville ones
wost_pt2 <- wost_j_raw %>%
  filter(burst == "414590-2009")

# Create example dataset for Wood Storks
woodstorks <- rbind(wost_pt1, wost_pt2)

# Create example dataset for Kestrels
kestrels <- kestrels_raw %>%
  filter(burst %in% c("16336-2016", "16339-2016"))

# Create example dataset for Gulls
gulls <- gulls_raw %>%
  filter(burst %in% c("URI04-2016", "URI07-2016"))

# Save ----

usethis::use_data(woodstorks)
usethis::use_data(kestrels)
usethis::use_data(gulls)

# Example outputs ----

wost_output_1 <- find_nests(gps_data = woodstorks,
                    sea_start = 305,
                    sea_end = 243,
                    nest_cycle = 110,
                    buffer = 40,
                    min_pts = 2,
                    min_d_fix = 5,
                    min_consec = 2,
                    min_top_att = 1,
                    min_days_att = 1,
                    discard_overlapping = FALSE)

usethis::use_data(wost_output_1)

# Known nest Jacksonville stork ----

jax_known_nest <- data.frame(burst = "414590-2009",
                             long = -81.645249 ,
                             lat = 30.404849)

usethis::use_data(jax_known_nest)
