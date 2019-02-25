library(readr)
library(dplyr)

# Load raw data ----

# Load dataset of Wood Stork data
ws_raw <- readRDS("~/Documents/PhD/Research/Wood Stork/WOST_repo/nesting/input/ws_gps_data.rds")

# Keep columns of interest
ws_raw <- ws_raw %>%
  select(burst, date, long, lat)

# Load dataset of Kestrel data
lk_raw <- readRDS("~/Documents/PhD/Research/Wood Stork/WOST_repo/nesting/input/lk_gps_data.rds")

# Load dataset of Gull data
mg_raw <- readRDS("~/Documents/PhD/Research/Wood Stork/WOST_repo/nesting/input/mg_gps_data.rds")

# Create example datasets ----

# Create example dataset for Wood Storks
woodstorks <- ws_raw %>%
  filter(burst %in% c("1134370-2013", "721290-2010")) %>%
  as.data.frame()

# Create example dataset for Kestrels
kestrels <- lk_raw %>%
  filter(burst %in% c("16682-2017", "16680-2017")) %>%
  as.data.frame()

# Create example dataset for Gulls
gulls <- mg_raw %>%
  filter(burst %in% c("URI29-2016", "URI05-2016")) %>%
  as.data.frame()

usethis::use_data(woodstorks, overwrite = TRUE)
usethis::use_data(kestrels, overwrite = TRUE)
usethis::use_data(gulls, overwrite = TRUE)

# Example outputs for WS ----

ws_output_1 <- find_nests(gps_data = woodstorks,
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

usethis::use_data(ws_output_1, overwrite = TRUE)

ws_output_2 <- find_nests(gps_data = woodstorks,
                          sea_start = "11-01",
                          sea_end = "08-31",
                          nest_cycle = 110,
                          buffer = 40,
                          min_pts = 2,
                          min_d_fix = 5,
                          min_consec = 17,
                          min_top_att = 81,
                          min_days_att = 1,
                          discard_overlapping = TRUE)

usethis::use_data(ws_output_2, overwrite = TRUE)

ws_output_3 <- find_nests(gps_data = woodstorks,
                          sea_start = "11-01",
                          sea_end = "08-31",
                          nest_cycle = 110,
                          buffer = 40,
                          min_pts = 2,
                          min_d_fix = 5,
                          min_consec = 31,
                          min_top_att = 1,
                          min_days_att = 1,
                          discard_overlapping = TRUE)

usethis::use_data(ws_output_3, overwrite = TRUE)

# Known nest Jacksonville stork ----

jax_known_nest <- data.frame(burst = "721290-2010",
                             long = -81.645249 ,
                             lat = 30.404849)

usethis::use_data(jax_known_nest, overwrite = TRUE)

# Simulated dataset of 200 WS nests for CART illustration ----

# Simulate data
set.seed(1)

# Get exploratory data for the two storks
output_stork1 <- ws_output_1$nests %>%
  filter(burst == "721290-2010")

explodata_stork1 <- get_explodata(candidate_nests = output_stork1,
                                  known_coords = jax_known_nest,
                                  buffer = 40,
                                  pick_overlapping = TRUE)

output_stork2 <- ws_output_1$nests %>%
  filter(burst == "1134370-2013")

id_known <- data.frame(burst = "1134370-2013",
                       loc_id = 2170)

explodata_stork2 <- get_explodata(candidate_nests = output_stork2,
                                  known_ids = id_known,
                                  pick_overlapping = TRUE)

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
consec_days_y <- round(5+rnorm(100,
                             mean=mean(explodata_yes$consec_days),
                             sd=10), 0)
consec_days_n <- round(rnorm(100,
                             mean=mean(explodata_no$consec_days),
                             sd=5), 0)

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
                        sd=30)

# Bind all together
explodata_ws_y <- cbind.data.frame(burst = burst_y,
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
explodata_ws_n <- cbind.data.frame(burst = burst_n,
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
explodata_ws <- rbind(explodata_ws_y, explodata_ws_n)
explodata_ws <- explodata_ws[order(explodata_ws$burst),]

# Fix percents above 100%
explodata_ws$perc_days_vis <- ifelse(explodata_ws$perc_days_vis > 100,
                                         100, explodata_ws$perc_days_vis)
explodata_ws$perc_top_vis <- ifelse(explodata_ws$perc_top_vis > 100,
                                        100, explodata_ws$perc_top_vis)

usethis::use_data(explodata_ws, overwrite = TRUE)

# WS reproductive outcome data ----

ws_nests <- ws_output_2

ws_attempts <- format_attempts(nest_info = ws_nests, nest_cycle = 110)

ws_outcomes <- estimate_outcomes(fixes = ws_attempts$fixes,
                                   visits = ws_attempts$visits,
                                   model = "p_time")

usethis::use_data(ws_outcomes, overwrite = TRUE)

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
