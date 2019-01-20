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
  filter(burst == "1134370-2013") %>%
  as.data.frame()

# Select one Wood Stork among the Jacksonville ones
wost_pt2 <- wost_j_raw %>%
  filter(burst == "414590-2009") %>%
  as.data.frame()

# Create example dataset for Wood Storks
woodstorks <- rbind(wost_pt1, wost_pt2)

# Create example dataset for Kestrels
kestrels <- kestrels_raw %>%
  filter(burst %in% c("16336-2016", "16339-2016")) %>%
  as.data.frame()

# Create example dataset for Gulls
gulls <- gulls_raw %>%
  filter(burst %in% c("URI04-2016", "URI07-2016")) %>%
  as.data.frame()

# Save

usethis::use_data(woodstorks)
usethis::use_data(kestrels)
usethis::use_data(gulls)

# Example outputs ----

wost_output_1 <- find_nests(gps_data = woodstorks,
                    sea_start = "11-01",
                    sea_end = "08-31",
                    nest_cycle = 110,
                    buffer = 40,
                    min_pts = 2,
                    min_d_fix = 5,
                    min_consec = 2,
                    min_top_att = 1,
                    min_days_att = 1,
                    discard_overlapping = FALSE)

usethis::use_data(wost_output_1, overwrite = TRUE)

wost_output_2 <- find_nests(gps_data = woodstorks,
                            sea_start = "11-01",
                            sea_end = "08-31",
                            nest_cycle = 110,
                            buffer = 40,
                            min_pts = 2,
                            min_d_fix = 5,
                            min_consec = 15,
                            min_top_att = 85,
                            min_days_att = 53,
                            discard_overlapping = TRUE)

usethis::use_data(wost_output_2, overwrite = TRUE)

wost_output_3 <- find_nests(gps_data = woodstorks,
                            sea_start = "11-01",
                            sea_end = "08-31",
                            nest_cycle = 110,
                            buffer = 40,
                            min_pts = 2,
                            min_d_fix = 5,
                            min_consec = 2,
                            min_top_att = 94,
                            min_days_att = 53,
                            discard_overlapping = TRUE)

usethis::use_data(wost_output_3, overwrite = TRUE)

# Known nest Jacksonville stork ----

jax_known_nest <- data.frame(burst = "414590-2009",
                             long = -81.645249 ,
                             lat = 30.404849)

usethis::use_data(jax_known_nest)

# Simulated dataset of 200 nests for CART illustration ----

# Simulate data
set.seed(1)

# Use means of explodata_stork1 and explodata_stork2 as starting points
explodata_bind <- rbind(explodata_stork1, explodata_stork2)
explodata_yes <- explodata_bind %>%
  filter(nest == "yes")
explodata_no <- explodata_bind %>%
  filter(nest == "no")

# Fake burst ids
burst_y <- paste0("B", 100:199)
burst_n <- burst_y

# Fake location ids
loc_id_y <- sample(1:100)
loc_id_n <- sample(101:200)

# Simulate coordinates
long_y <- explodata_yes$long + rnorm(100, mean=1, sd=0.1)
long_n <- explodata_no$long + rnorm(100, mean=1, sd=0.1)
lat_y <- explodata_yes$lat + rnorm(100, mean=1, sd=0.1)
lat_n <- explodata_no$lat + rnorm(100, mean=1, sd=0.1)

# Simulate first and last dates
first_date_y <- explodata_yes$first_date + rnorm(100, mean=1, sd=50)
first_date_n <- explodata_no$first_date + rnorm(100, mean=1, sd=50)
last_date_y <- explodata_yes$last_date + rnorm(100, mean=1, sd=50)
last_date_n <- explodata_no$last_date + rnorm(100, mean=1, sd=50)

# Simulate attempt start and end
attempt_start_y <- explodata_yes$attempt_start + rnorm(100, mean=1, sd=50)
attempt_start_n <- explodata_no$attempt_start + rnorm(100, mean=1, sd=50)
attempt_end_y <- explodata_yes$attempt_end + rnorm(100, mean=1, sd=50)
attempt_end_n <- explodata_no$attempt_end + rnorm(100, mean=1, sd=50)

# Simulate number of total visits
tot_vis_y <- round(rnorm(100,
                         mean=mean(explodata_yes$tot_vis),
                         sd=50), 0)
tot_vis_n <- round(rnorm(100,
                         mean=mean(explodata_no$tot_vis),
                         sd=50), 0)

# Simulate number of days visited
days_vis_y <- round(rnorm(100,
                          mean=mean(explodata_yes$days_vis),
                          sd=20), 0)
days_vis_n <- round(rnorm(100,
                          mean=mean(explodata_no$days_vis),
                          sd=20), 0)

# Simulate consecutive days
consec_days_y <- round(rnorm(100,
                             mean=mean(explodata_yes$consec_days),
                             sd=15), 0)
consec_days_n <- round(rnorm(100,
                             mean=mean(explodata_no$consec_days),
                             sd=10), 0)

# Simulate percent days visited
perc_days_vis_y <- rnorm(100,
                         mean=mean(explodata_yes$perc_days_vis),
                         sd=10)
perc_days_vis_n <- rnorm(100,
                         mean=mean(explodata_no$perc_days_vis),
                         sd=20)

# Simulate percent top visited
perc_top_vis_y <- rnorm(100,
                        mean=mean(explodata_yes$perc_top_vis),
                        sd=10)
perc_top_vis_n <- rnorm(100,
                        mean=mean(explodata_no$perc_top_vis),
                        sd=20)

# Bind all together
explodata_storks_y <- cbind.data.frame(burst = burst_y,
                                       loc_id = loc_id_y,
                                       long = long_y,
                                       lat = lat_y,
                                       first_date = first_date_y,
                                       last_dat = last_date_y,
                                       attempt_start = attempt_start_y,
                                       attempt_end = attempt_end_y,
                                       tot_vis = tot_vis_y,
                                       days_vis = days_vis_y,
                                       consec_days = consec_days_y,
                                       perc_days_vis = perc_days_vis_y,
                                       perc_top_vis = perc_top_vis_y,
                                       nest = rep("yes", 100))
explodata_storks_n <- cbind.data.frame(burst = burst_n,
                                       loc_id = loc_id_n,
                                       long = long_n,
                                       lat = lat_n,
                                       first_date = first_date_n,
                                       last_dat = last_date_n,
                                       attempt_start = attempt_start_n,
                                       attempt_end = attempt_end_n,
                                       tot_vis = tot_vis_n,
                                       days_vis = days_vis_n,
                                       consec_days = consec_days_n,
                                       perc_days_vis = perc_days_vis_n,
                                       perc_top_vis = perc_top_vis_n,
                                       nest = rep("no", 100))
explodata_storks <- rbind(explodata_storks_y, explodata_storks_n)
explodata_storks <- explodata_storks[order(explodata_storks$burst),]

# Fix percents above 100%
explodata_storks$perc_days_vis <- ifelse(explodata_storks$perc_days_vis > 100,
                                         100, explodata_storks$perc_days_vis)
explodata_storks$perc_top_vis <- ifelse(explodata_storks$perc_top_vis > 100,
                                        100, explodata_storks$perc_top_vis)

usethis::use_data(explodata_storks)
