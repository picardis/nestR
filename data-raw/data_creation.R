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
  filter(burst == "721290-2010") %>%
  as.data.frame()

# Create example dataset for Wood Storks
woodstorks <- rbind(wost_pt1, wost_pt2)

# Create example dataset for Kestrels
kestrels <- kestrels_raw %>%
  filter(burst %in% c("16682-2017", "16680-2017")) %>%
  as.data.frame()

# Successful attempts
# 16334 - 45 days vis
# 16336 - 44 days vis
# 16351 - 45 days vis
# 16363 - 40 days vis
# 16371 - 36 days vis
# 16375 - 30 days vis
# 16520 - 44 days vis
# 16527 - 33 days vis
# 16550 - 42 days vis
# 16558 - 40 days vis
# 16588 - 40 days vis
# 16680 - 47 days vis
# 16685 - 34 days vis

# Failed attempts
# 16339 - 28 days vis
# 16341 - 25 days vis
# 16374 - 27 days vis
# 16216 - 24 days vis
# 16557 - 20 days vis
# 16682 - 21 days vis
# 16683 - 31 days vis

# Create example dataset for Gulls
gulls <- gulls_raw %>%
  filter(burst %in% c("URI29-2016", "URI05-2016")) %>%
  as.data.frame()

# URI06-2016 is a good girl but it looks like the tag might have failed
# URI30-2016 crashes - not anymore with shorter season - 45 days visited
# URI28-2016 crashes - still crashes
# URI29-2016 crashes - not anymore with shorter season - 49 days visited
# URI25-2016 crashes - not anymore with shorter season - 47 days visited
# URI04-2016 crashes - not anymore with shorter season - 43 days visited

# Save

usethis::use_data(woodstorks, overwrite = TRUE)
usethis::use_data(kestrels, overwrite = TRUE)
usethis::use_data(gulls, overwrite = TRUE)

# Example outputs for WS ----

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

jax_known_nest <- data.frame(burst = "721290-2010",
                             long = -81.645249 ,
                             lat = 30.404849)

usethis::use_data(jax_known_nest, overwrite = TRUE)

# Simulated dataset of 200 WS nests for CART illustration ----

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

# WS reproductive outcome data ----

wost_nests <- wost_output_2

wost_attempts <- format_attempts(nest_info = wost_nests, nest_cycle = 110)

wost_outcomes <- estimate_outcomes(fixes = wost_attempts$fixes,
                                   visits = wost_attempts$visits,
                                   model = "p_time")

usethis::use_data(wost_outcomes, overwrite = TRUE)

# Example outputs for LK ----

lk_output_1 <- find_nests(gps_data = kestrels,
                          sea_start = "03-31",
                          sea_end = "08-31",
                          nest_cycle = 35,
                          buffer = 40,
                          min_pts = 2,
                          min_d_fix = 15,
                          min_consec = 2,
                          min_top_att = 1,
                          min_days_att = 1,
                          discard_overlapping = FALSE)

usethis::use_data(lk_output_1, overwrite = TRUE)

lk_output_2 <- find_nests(gps_data = kestrels,
                          sea_start = "03-31",
                          sea_end = "08-31",
                          nest_cycle = 35,
                          buffer = 40,
                          min_pts = 2,
                          min_d_fix = 15,
                          min_consec = 6,
                          min_top_att = 70,
                          min_days_att = 65,
                          discard_overlapping = TRUE)

usethis::use_data(lk_output_2, overwrite = TRUE)

# Known kestrel nests ----

lk_known_nests <- data.frame(burst = c("16682-2017", "16680-2017"),
                             long = c(16.416639, 16.552077),
                             lat = c(40.817021, 40.828485))

usethis::use_data(lk_known_nests, overwrite = TRUE)

# LK reproductive outcome data ----

lk_nests <- lk_output_2

lk_attempts <- format_attempts(nest_info = lk_nests, nest_cycle = 35)

lk_outcomes <- estimate_outcomes(fixes = lk_attempts$fixes,
                                 visits = lk_attempts$visits,
                                 model = "null")

usethis::use_data(lk_outcomes, overwrite = TRUE)

# Example outputs for MG ----

mg_output_1 <- find_nests(gps_data = gulls,
                          sea_start = "04-15",
                          sea_end = "08-01",
                          nest_cycle = 40,
                          buffer = 40,
                          min_pts = 2,
                          min_d_fix = 15,
                          min_consec = 2,
                          min_top_att = 1,
                          min_days_att = 1,
                          discard_overlapping = FALSE)

usethis::use_data(mg_output_1, overwrite = TRUE)

mg_output_2 <- find_nests(gps_data = gulls,
                           sea_start = "04-15",
                           sea_end = "08-01",
                           nest_cycle = 40,
                           buffer = 40,
                           min_pts = 2,
                           min_d_fix = 15,
                           min_consec = 10,
                           min_top_att = 80,
                           min_days_att = 90,
                           discard_overlapping = TRUE)

usethis::use_data(mg_output_2, overwrite = TRUE)

# Known gull nests ----

mg_known_nests <- data.frame(burst = c("URI29-2016", "URI05-2016"),
                             long = c(12.32471, 12.32471),
                             lat = c(44.23803, 44.23803))

usethis::use_data(mg_known_nests, overwrite = TRUE)

# MG reproductive outcome data ----

mg_nests <- mg_output_2

mg_attempts <- format_attempts(nest_info = mg_nests, nest_cycle = 40)

mg_outcomes <- estimate_outcomes(fixes = mg_attempts$fixes,
                                 visits = mg_attempts$visits,
                                 model = "p_time")

usethis::use_data(mg_outcomes, overwrite = TRUE)
