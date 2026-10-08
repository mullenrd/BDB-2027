library(tidyverse)
library(janitor)

# Load datasets from subfolder
combine_results <- read_csv(
  "nfl-big-data-bowl-2027/combine_results.csv"
)

combine_tracking <- read_csv(
  "nfl-big-data-bowl-2027/combine_tracking.csv"
)

# Standardize column names
combine_results <- clean_names(combine_results)
combine_tracking <- clean_names(combine_tracking)

# Initial inspection
dim(combine_results)
dim(combine_tracking)

glimpse(combine_results)
glimpse(combine_tracking)

library(tidyverse)

# Missingness summary
missing_results <- combine_results %>%
  summarise(across(everything(), ~sum(is.na(.)))) %>%
  pivot_longer(
    everything(),
    names_to = "variable",
    values_to = "missing_count"
  ) %>%
  mutate(
    pct_missing = round(
      100 * missing_count / nrow(combine_results), 2
    )
  ) %>%
  arrange(desc(pct_missing))

missing_results

# Number of players by position
combine_results %>%
  count(combine_position, sort = TRUE)

# Drill types in tracking data
combine_tracking %>%
  count(drill_type, sort = TRUE)

# Number of unique players
combine_results %>%
  summarise(players = n_distinct(nfl_id))

combine_tracking %>%
  summarise(players = n_distinct(nfl_id))

# Draft-year representation
combine_results %>%
  count(draft_year)

combine_tracking %>%
  count(draft_year)

combine_results %>%
  filter(!is.na(forty)) %>%
  ggplot(aes(
    x = reorder(combine_position, forty, FUN = median),
    y = forty,
    fill = combine_position
  )) +
  geom_boxplot(alpha = 0.75) +
  labs(
    title = "40-Yard Dash Performance by Position",
    x = "Combine Position",
    y = "40-Yard Dash Time (seconds)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

combine_results %>%
  ggplot(aes(
    x = combine_weight,
    y = forty,
    color = combine_position
  )) +
  geom_point(alpha = 0.7, size = 2) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    color = "black"
  ) +
  labs(
    title = "Player Weight vs. 40-Yard Dash Time",
    x = "Weight (lbs)",
    y = "40-Yard Dash Time (seconds)"
  ) +
  theme_minimal()



drill_summary <- combine_tracking %>%
  group_by(drill_type, drill_name) %>%
  summarise(
    n_players = n_distinct(nfl_id),
    n_events = n_distinct(event_id),
    n_observations = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(n_events))

print(drill_summary, n = 50)

combine_tracking %>%
  ggplot(aes(x = s)) +
  geom_histogram(
    bins = 60,
    fill = "steelblue",
    color = "white"
  ) +
  labs(
    title = "Distribution of Recorded Tracking Speeds",
    x = "Recorded Speed",
    y = "Number of Tracking Frames"
  ) +
  theme_minimal()

tracking_event_summary <- combine_tracking %>%
  filter(entity_type == "PLAYER") %>%
  group_by(
    draft_year,
    nfl_id,
    event_id,
    drill_type,
    drill_name,
    attempt
  ) %>%
  summarise(
    max_speed = if (all(is.na(s))) NA_real_ else max(s, na.rm = TRUE),
    mean_speed = if (all(is.na(s))) NA_real_ else mean(s, na.rm = TRUE),
    max_acceleration = if (all(is.na(a))) NA_real_ else max(a, na.rm = TRUE),
    mean_acceleration = if (all(is.na(a))) NA_real_ else mean(a, na.rm = TRUE),
    total_distance = if (all(is.na(dis))) NA_real_ else sum(dis, na.rm = TRUE),
    n_frames = n(),
    .groups = "drop"
  )

glimpse(tracking_event_summary)


combine_results %>% count(combine_position, sort = TRUE)

combine_tracking %>% count(drill_type, sort = TRUE)

drill_summary %>%
  select(drill_type, drill_name, n_players, n_events) %>%
  print(n = 50)

forty_tracking <- tracking_event_summary %>%
  filter(drill_type == "FORTY_YARD_DASH") %>%
  group_by(draft_year, nfl_id) %>%
  summarise(
    max_speed = max(max_speed, na.rm = TRUE),
    max_acceleration = max(max_acceleration, na.rm = TRUE),
    mean_speed = mean(mean_speed, na.rm = TRUE),
    n_attempts = n(),
    .groups = "drop"
  )

forty_analysis <- combine_results %>%
  inner_join(
    forty_tracking,
    by = c("draft_year", "nfl_id")
  ) %>%
  filter(
    !is.na(forty),
    !is.na(max_speed),
    is.finite(max_speed),
    is.finite(max_acceleration)
  )

glimpse(forty_analysis)

ggplot(
  forty_analysis,
  aes(
    x = max_speed,
    y = forty,
    color = combine_position
  )
) +
  geom_point(alpha = 0.7, size = 2) +
  geom_smooth(
    method = "lm",
    se = FALSE
  ) +
  facet_wrap(~ combine_position) +
  labs(
    title = "Maximum Tracking Speed vs. Official 40-Yard Dash",
    subtitle = "NFL Combine 2023–2025",
    x = "Maximum Tracked Speed",
    y = "Official 40-Yard Dash Time (seconds)",
    color = "Position"
  ) +
  theme_minimal()

forty_analysis %>%
  summarise(
    n = n(),
    cor_speed = cor(
      max_speed, forty,
      use = "complete.obs"
    ),
    cor_acceleration = cor(
      max_acceleration, forty,
      use = "complete.obs"
    ),
    cor_weight = cor(
      combine_weight, forty,
      use = "complete.obs"
    )
  )
