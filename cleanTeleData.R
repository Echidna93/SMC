
# CLEAN TELEMETRY DATA ---------------------------------------------------------


## reformat DateTime column to remove
## extraneous 1899 year
dat$DateTime <- as.POSIXct(paste(dat$Date,format(dat$Time, "%H:%M:%S")),
													 format = "%Y-%m-%d %H:%M:%S")

## create random steps
stps <- amt::make_track(dat %>% filter(!is.na(DateTime)) %>%
													arrange(DateTime),
												.x = "Longitude", .y = "Latitude",
												.t = "DateTime", all_cols = TRUE) %>%
	track_resample() %>% steps_by_burst(keep_cols = "start") %>%
	random_steps(n_control = 10)

# final2 <- stps %>%
# 	# Step 1
# 	tracked_from_to(from = as.POSIXct("2021-06-17 11:00"),
# 									to = as.POSIXct("2021-07-18 08:00")) %>%
# 	# Step 2
# 	remove_capture_effect(start = days(3)) %>%
# 	# Step 4
# 	filter(dop < 10) %>%
# 	# Step 5
# 	flag_duplicates(gamma = minutes(5)) %>%
# 	filter(!duplicate_) %>%
# 	# Step 6
# 	flag_fast_steps(delta = 5000) %>%
# 	filter(!fast_step_) %>%
# 	# Step 7
# 	flag_roundtrips(delta = 5000, epsilon = 3) %>%
# 	filter(!fast_roundtrip_) %>%
# 	# Step 8
# 	flag_defunct_clusters(zeta = 50,
# 												eta = 24,
# 												theta = hours(24)) %>%
# 	filter(!defunct_cluster_)

# Identical?
# identical(final, final2)

# MODELLING --------------------------------------------------------------------

## fit clogit
amt::fit_issf(stps, case_ ~ log(sl_) + cos(ta_) + strata(step_id_))

