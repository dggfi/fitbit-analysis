library(tidyverse)
library(stringr)
options(scipen = 999)

# Month one and month two share:
#  - dailyActivity_merged.csv
#  - heartrate_seconds_merged.csv
#  - hourlyCalories_merged.csv
#  - hourlyIntensities_merged.csv
#  - hourlySteps_merged.csv
#  - minuteCaloriesNarrow_merged.csv
#  - minuteIntensitiesNarrow_merged.csv
#  - minuteMETsNarrow_merged.csv
#  - minuteSleep_merged.csv
#  - minuteStepsNarrow_merged.csv
#  - weightLogInfo_merged.csv

# Shared files must be checked for consistency
compare_tibbles <- function(t1, t2, set_index) {
    t1_col_names <- colnames(t1)
    t2_col_names <- colnames(t2)

    pass_flag <- TRUE
    if (length(t1_col_names) != length(t2_col_names)) {
        print(str_glue("Tibbles in set {set_index} do not have the same number of columns.")) # nolint
        pass_flag <- FALSE
    }

    for (i in seq_along(t1_col_names)) {
        if (is.na(t2_col_names[i])) {
            print(str_glue("Tibbles 2 in set {set_index} is missing column {t1_col_names[i]}")) # nolint
        } else if (t1_col_names[i] != t2_col_names[i]) {
            print(str_glue("Column names in tibbles pair {set_index} are not identical: {t1_col_names[i]} | {t2_col_names[i]}")) # nolint
            pass_flag <- FALSE
        }
    }

    # Actually, this is more fancy and direct...
    str_t1 <- capture.output(str(t1))
    str_t2 <- capture.output(str(t2))
    start_index1 <- grep("\\.\\. cols\\(", str_t1)
    end_index1 <- grep("\\.\\. \\)", str_t1)
    start_index2 <- grep("\\.\\. cols\\(", str_t2)
    end_index2 <- grep("\\.\\. \\)", str_t2)

    if (!identical(str_t1[start_index1:end_index1], str_t2[start_index2:end_index2])) { # nolint
        print(str_glue("Tibbles in set {set_index} are not structurally identical!")) # nolint
        pass_flag <- FALSE
    }

    if (pass_flag) {
        return(TRUE)
    } else {
        return(FALSE)
    }
}

#### SHARED DATA
## note: _n is used to mark data that is narrow, _w for wide

# Activity
m1_daily_activity <- read_csv(
    "datasets/fitbit_mturk_3.12.16-4.11.16/dailyActivity_merged.csv",
    col_types = cols(
        ActivityDate = col_date(format = "%m/%d/%Y")
    ),
    show_col_types = FALSE
) # nolint
m2_daily_activity <- read_csv(
    "datasets/fitbit_mturk_4.12.16-5.12.16/dailyActivity_merged.csv",
    col_types = cols(
        ActivityDate = col_date(format = "%m/%d/%Y")
    ),
    show_col_types = FALSE
) # nolint
daily_activity_tibbles <- list(m1_daily_activity, m2_daily_activity)

# Heart rate
m1_heartrate_seconds <- read_csv(
    "datasets/fitbit_mturk_3.12.16-4.11.16/heartrate_seconds_merged.csv",
    col_types = cols(
        Time = col_datetime(format = "%m/%d/%Y %I:%M:%S %p")
    ),
    show_col_types = FALSE
)
m2_heartrate_seconds <- read_csv(
    "datasets/fitbit_mturk_4.12.16-5.12.16/heartrate_seconds_merged.csv",
    col_types = cols(
        Time = col_datetime(format = "%m/%d/%Y %I:%M:%S %p")
    ),
    show_col_types = FALSE
)
heartrate_seconds_tibbles <- list(m1_heartrate_seconds, m2_heartrate_seconds)

# Calories (in kilocalories)
m1_hourly_calories <- read_csv(
    "datasets/fitbit_mturk_3.12.16-4.11.16/hourlyCalories_merged.csv",
    col_types = cols(
        ActivityHour = col_datetime(format = "%m/%d/%Y %I:%M:%S %p")
    ),
    show_col_types = FALSE
)
m2_hourly_calories <- read_csv(
    "datasets/fitbit_mturk_4.12.16-5.12.16/hourlyCalories_merged.csv",
    col_types = cols(
        ActivityHour = col_datetime(format = "%m/%d/%Y %I:%M:%S %p")
    ),
    show_col_types = FALSE
)
hourly_calories_tibbles <- list(m1_hourly_calories, m2_hourly_calories)

m1_minute_calories_n <- read_csv(
    "datasets/fitbit_mturk_3.12.16-4.11.16/minuteCaloriesNarrow_merged.csv",
    col_types = cols(
        ActivityMinute = col_datetime(format = "%m/%d/%Y %I:%M:%S %p")
    ),
    show_col_types = FALSE
)
m2_minute_calories_n <- read_csv(
    "datasets/fitbit_mturk_4.12.16-5.12.16/minuteCaloriesNarrow_merged.csv",
    col_types = cols(
        ActivityMinute = col_datetime(format = "%m/%d/%Y %I:%M:%S %p")
    ),
    show_col_types = FALSE
)
minute_calories_n_tibbles <- list(m1_minute_calories_n, m2_minute_calories_n)

# Intensities
m1_hourly_intensities <- read_csv(
    "datasets/fitbit_mturk_3.12.16-4.11.16/hourlyIntensities_merged.csv",
    col_types = cols(
        ActivityHour = col_datetime(format = "%m/%d/%Y %I:%M:%S %p")
    ),
    show_col_types = FALSE
)
m2_hourly_intensities <- read_csv(
    "datasets/fitbit_mturk_4.12.16-5.12.16/hourlyIntensities_merged.csv",
    col_types = cols(
        ActivityHour = col_datetime(format = "%m/%d/%Y %I:%M:%S %p")
    ),
    show_col_types = FALSE
)
hourly_intensities_tibbles <- list(m1_hourly_intensities, m2_hourly_intensities)

m1_minute_intensities_n <- read_csv(
    "datasets/fitbit_mturk_3.12.16-4.11.16/minuteIntensitiesNarrow_merged.csv",
    col_types = cols(
        ActivityMinute = col_datetime(format = "%m/%d/%Y %I:%M:%S %p")
    ),
    show_col_types = FALSE
)
m2_minute_intensities_n <- read_csv(
    "datasets/fitbit_mturk_4.12.16-5.12.16/minuteIntensitiesNarrow_merged.csv",
    col_types = cols(
        ActivityMinute = col_datetime(format = "%m/%d/%Y %I:%M:%S %p")
    ),
    show_col_types = FALSE
)
minute_intensities_n_tibbles <- list(m1_minute_intensities_n, m2_minute_intensities_n) # nolint

# Steps
m1_hourly_steps <- read_csv(
    "datasets/fitbit_mturk_3.12.16-4.11.16/hourlySteps_merged.csv",
    col_types = cols(
        ActivityHour = col_datetime(format = "%m/%d/%Y %I:%M:%S %p")
    ),
    show_col_types = FALSE
)
m2_hourly_steps <- read_csv(
    "datasets/fitbit_mturk_4.12.16-5.12.16/hourlySteps_merged.csv",
    col_types = cols(
        ActivityHour = col_datetime(format = "%m/%d/%Y %I:%M:%S %p")
    ),
    show_col_types = FALSE
)
hourly_steps_tibbles <- list(m1_hourly_steps, m2_hourly_steps)

m1_minute_steps_n <- read_csv(
    "datasets/fitbit_mturk_3.12.16-4.11.16/minuteStepsNarrow_merged.csv",
    col_types = cols(
        ActivityMinute = col_datetime(format = "%m/%d/%Y %I:%M:%S %p")
    ),
    show_col_types = FALSE
)
m2_minute_steps_n <- read_csv(
    "datasets/fitbit_mturk_4.12.16-5.12.16/minuteStepsNarrow_merged.csv",
    col_types = cols(
        ActivityMinute = col_datetime(format = "%m/%d/%Y %I:%M:%S %p")
    ),
    show_col_types = FALSE
)
minute_steps_tibbles <- list(m1_minute_steps_n, m2_minute_steps_n)

# METs (divide these values by 10)
m1_minute_mets_n <- read_csv(
    "datasets/fitbit_mturk_3.12.16-4.11.16/minuteMETsNarrow_merged.csv",
    col_types = cols(
        ActivityMinute = col_datetime(format = "%m/%d/%Y %I:%M:%S %p")
    ),
    show_col_types = FALSE
)
m2_minute_mets_n <- read_csv(
    "datasets/fitbit_mturk_4.12.16-5.12.16/minuteMETsNarrow_merged.csv",
    col_types = cols(
        ActivityMinute = col_datetime(format = "%m/%d/%Y %I:%M:%S %p")
    ),
    show_col_types = FALSE
)
mets_n_tibbles <- list(m1_minute_mets_n, m2_minute_mets_n)

# Sleep
m1_minute_sleep <- read_csv(
    "datasets/fitbit_mturk_3.12.16-4.11.16/minuteSleep_merged.csv",
    col_types = cols(
        date = col_datetime(format = "%m/%d/%Y %I:%M:%S %p")
    ),
    show_col_types = FALSE
)
m2_minute_sleep <- read_csv(
    "datasets/fitbit_mturk_4.12.16-5.12.16/minuteSleep_merged.csv",
    col_types = cols(
        date = col_datetime(format = "%m/%d/%Y %I:%M:%S %p")
    ),
    show_col_types = FALSE
)
minute_sleep_tibbles <- list(m1_minute_sleep, m2_minute_sleep)

# Weight - User reported
m1_weight_log <- read_csv(
    "datasets/fitbit_mturk_3.12.16-4.11.16/weightLogInfo_merged.csv",
    col_types = cols(
        Date = col_datetime(format = "%m/%d/%Y %I:%M:%S %p")
    ),
    show_col_types = FALSE
)
m2_weight_log <- read_csv(
    "datasets/fitbit_mturk_4.12.16-5.12.16/weightLogInfo_merged.csv",
    col_types = cols(
        Date = col_datetime(format = "%m/%d/%Y %I:%M:%S %p")
    ),
    show_col_types = FALSE
)
weight_log_tibbles <- list(m1_weight_log, m2_weight_log)

shared_tibbles <- list(
    daily_activity_tibbles,
    heartrate_seconds_tibbles,
    hourly_calories_tibbles,
    hourly_intensities_tibbles,
    hourly_steps_tibbles,
    minute_calories_n_tibbles,
    minute_intensities_n_tibbles,
    mets_n_tibbles,
    minute_sleep_tibbles,
    minute_steps_tibbles,
    weight_log_tibbles
)

#### ADDITIONAL/SUPPLEMENTARY DATA

# Month one is missing:
#  - dailyCalories_merged.csv
#  - dailyIntensities_merged.csv
#  - dailySteps_merged.csv
#  - minuteCaloriesWide_merged.csv
#  - minuteIntensitiesWide_merged.csv
#  - minuteStepsWide_merged.csv
#  - sleepDay_merged.csv

m2_daily_calories <- read_csv(
    "datasets/fitbit_mturk_4.12.16-5.12.16/dailyCalories_merged.csv",
    col_types = cols(
        ActivityDay = col_datetime(format = "%m/%d/%Y")
    ),
    show_col_types = FALSE
)

m2_daily_intensities <- read_csv(
    "datasets/fitbit_mturk_4.12.16-5.12.16/dailyIntensities_merged.csv",
    col_types = cols(
        ActivityDay = col_datetime(format = "%m/%d/%Y")
    ),
    show_col_types = FALSE
)

m2_daily_steps <- read_csv(
    "datasets/fitbit_mturk_4.12.16-5.12.16/dailySteps_merged.csv",
    col_types = cols(
        ActivityDay = col_datetime(format = "%m/%d/%Y")
    ),
    show_col_types = FALSE
)

# Narrowed -- m2_minute_calories_n already exists, second version for comparison
m2_minute_calories_w <- read_csv(
    "datasets/fitbit_mturk_4.12.16-5.12.16/minuteCaloriesWide_merged.csv",
    col_types = cols(
        ActivityHour = col_datetime(format = "%m/%d/%Y %I:%M:%S %p")
    ),
    show_col_types = FALSE
)
m2_minute_calories_n_v2 <- m2_minute_calories_w %>%
    pivot_longer(
        cols = starts_with("Calories"),
        names_to = "ActivityMinute",
        values_to = "Calories"
    ) %>%
    mutate(
        ActivityMinute = ActivityHour + minutes(as.integer(substr(ActivityMinute, 9, 10))), # nolint
        ActivityHour = NULL,
        .before = "Calories"
    )

## Narrowed -- m2_minute_intensities_n already exists, v2 for comparison
m2_minute_intensities_w <- read_csv(
    "datasets/fitbit_mturk_4.12.16-5.12.16/minuteIntensitiesWide_merged.csv",
    col_types = cols(
        ActivityHour = col_datetime(format = "%m/%d/%Y %I:%M:%S %p")
    ),
    show_col_types = FALSE
)
m2_minute_intensities_n_v2 <- m2_minute_intensities_w %>%
    pivot_longer(
        cols = starts_with("Intensity"),
        names_to = "ActivityMinute",
        values_to = "Intensity"
    ) %>%
    mutate(
        ActivityMinute = ActivityHour + minutes(as.integer(substr(ActivityMinute, 10, 11))), # nolint,
        ActivityHour = NULL,
        .before = "Intensity"
    )

# Narrowed -- m2_minute_steps_n already exists, v2 for comparison
m2_minute_steps_w <- read_csv(
    "datasets/fitbit_mturk_4.12.16-5.12.16/minuteStepsWide_merged.csv",
    col_types = cols(
        ActivityHour = col_datetime(format = "%m/%d/%Y %I:%M:%S %p")
    ),
)
m2_minute_steps_n_v2 <- m2_minute_steps_w %>%
    pivot_longer(
        cols = starts_with("Steps"),
        names_to = "ActivityMinute",
        values_to = "Steps"
    ) %>%
    mutate(
        ActivityMinute = ActivityHour + minutes(as.integer(substr(ActivityMinute, 6, 7))), # nolint
        ActivityHour = NULL,
        .before = "Steps"
    )

m2_daily_sleep <- read_csv(
    "datasets/fitbit_mturk_4.12.16-5.12.16/sleepDay_merged.csv",
    col_types = cols(
        SleepDay = col_datetime(format = "%m/%d/%Y %I:%M:%S %p")
    ),
)

#### TESTS
####
all_tests_passed <- TRUE

all_tibbles <- list(
    m1_daily_activity,
    m2_daily_activity,
    m1_heartrate_seconds,
    m2_heartrate_seconds,
    m1_hourly_calories,
    m2_hourly_calories,
    m1_minute_calories_n,
    m2_minute_calories_n,
    m1_hourly_intensities,
    m2_hourly_intensities,
    m1_minute_intensities_n,
    m2_minute_intensities_n,
    m1_hourly_steps,
    m2_hourly_steps,
    m1_minute_steps_n,
    m2_minute_steps_n,
    m1_minute_mets_n,
    m2_minute_mets_n,
    m1_minute_sleep, # has duplicates (525)
    m2_minute_sleep, # has duplicates (543)
    m1_weight_log,
    m2_weight_log,
    m2_daily_calories,
    m2_daily_intensities,
    m2_daily_steps,
    m2_minute_calories_n_v2,
    m2_minute_intensities_n_v2,
    m2_minute_steps_n_v2,
    m2_daily_sleep # has duplicates (29)
)

## Uncomment to check for duplication
# for (i in seq_along(all_tibbles)) {
#     tib_obj <- all_tibbles[[i]]
#     original_n_rows <- nrow(tib_obj)
#     distinct_n_rows <- nrow(distinct(tib_obj))
#     if (original_n_rows != distinct_n_rows) {
#         print(str_glue("Tibble at index {i} has {abs(original_n_rows - distinct_n_rows)} duplicate rows ({original_n_rows} v. {distinct_n_rows}):")) # nolint
#     }
# }

## Uncomment to check if shared datasets of two months are structurally the same
# for (i in seq_along(shared_tibbles)) {
#     tibble_pair <- shared_tibbles[[i]]
#     passed <- compare_tibbles(tibble_pair[[1]], tibble_pair[[2]], i)
#     if (!passed) all_tests_passed <- FALSE
# }

## Uncomment to see if there are any NA values
# for (i in seq_along(all_tibbles)) {
#     map(all_tibbles[i], function(tib_obj) {
#         result <- sapply(tib_obj, function(x) sum(is.na(x)))
#         print(result)
#     })
# }

if (all_tests_passed) {
    print("All tests passed!")
} else {
    print("Some tests failed.")
}

#### TRANSFORMATIONS
####
# Remove duplication
m1_minute_sleep <- distinct(m1_minute_sleep)
m2_minute_sleep <- distinct(m2_minute_sleep)
m2_daily_sleep <- distinct(m2_daily_sleep)

# m2_minute_calories_n and m2_minute_calories_n_v2
# m2_minute_intensities_n and m2_minute_intensities_n_v2
# m2_minute_steps_n and m2_minute_steps_n_v2
# are each missing data from the other

m2_minute_calories_n <- union(m2_minute_calories_n, m2_minute_calories_n_v2)
m2_minute_intensities_n <- union(m2_minute_intensities_n, m2_minute_intensities_n_v2) # nolint
m2_minute_steps_n <- union(m2_minute_steps_n, m2_minute_steps_n_v2)

# group m2_minute_calories_union and compare against m2_daily_calories
# group m2_minute_intensities_union and compare against m2_daily_intensities
# group m2_minute_steps_union and compare against m2_daily_steps

#### Build *_daily_activity / fill in the gaps

## Need for each month:
# daily calories
# daily intensities
# daily mets
# daily steps

## Other
# daily sleep

## Month 1 datasets (visual reminder)
#  - dailyActivity
#  - hourlyCalories
#  - hourlyIntensities
#  - hourlySteps
#  - minuteCaloriesNarrow
#  - minuteIntensitiesNarrow
#  - minuteMETsNarrow
#  - minuteSleep
#  - minuteStepsNarrow
#  - heartrate_seconds
#  - weightLogInfo

## Month 1 needs:
# daily calories - done
# daily mets - done
# daily steps - done
# daily intensities - done
# daily sleep - done

## Month 1 Calories
m1_daily_calories <- m1_minute_calories_n %>%
    mutate(
        ActivityDay = date(ActivityMinute),
        ActivityMinute = NULL
    ) %>%
    group_by(Id, ActivityDay) %>%
    summarise(Calories = sum(Calories)) %>%
    rename(ActivityDate = "ActivityDay")


## Month 1 METs // Divide by 10
m1_daily_mets <- m1_minute_mets_n %>%
    mutate(
        ActivityDay = date(ActivityMinute),
        ActivityMinute = NULL,
        METs = (METs / 10)
    ) %>%
    group_by(Id, ActivityDay) %>%
    summarise(METs = sum(METs >= 2.4)) %>%
    rename(ActivityDate = "ActivityDay")

## Month 1 Sleep
m1_daily_sleep <- m1_minute_sleep %>%
    rename(
        LogId = "logId",
        SleepDay = "date"
    ) %>%
    mutate(
        SleepDay = date(SleepDay)
    ) %>%
    group_by(Id, SleepDay) %>%
    summarize(
        TotalSleepRecords = n_distinct(LogId),
        TotalMinutesAsleep = sum(value == 1),
        TotalMinutesRestless = sum(value == 2),
        TotalTimeInBed = n(),
        Efficiency = (100 * TotalMinutesAsleep / TotalTimeInBed),
        Inefficiency = (100 * TotalMinutesRestless / TotalTimeInBed)
    ) %>%
    rename(ActivityDate = "SleepDay")

## Month 1 Steps
m1_daily_steps <- m1_minute_steps_n %>%
    mutate(
        ActivityDay = date(ActivityMinute),
        ActivityMinute = NULL
    ) %>%
    group_by(Id, ActivityDay) %>%
    summarise(StepTotal = sum(Steps)) %>%
    rename(ActivityDate = "ActivityDay")

## Month 1 Intensities
# 0 = Sedentary, 1 = Light, 2 = Moderate, 3 = Very Active
m1_daily_intensities <- m1_minute_intensities_n %>%
    mutate(
        ActivityDay = date(ActivityMinute),
        ActivityMinute = NULL
    ) %>%
    group_by(Id, ActivityDay) %>%
    summarise(
        SedentaryMinutes = sum(Intensity == 0),
        LightlyActiveMinutes = sum(Intensity == 1),
        FairlyActiveMinutes = sum(Intensity == 2),
        VeryActiveMinutes = sum(Intensity == 3)
    ) %>%
    rename(ActivityDate = "ActivityDay")

## Month 1 Data Collage
m1_daily_activity_final <- full_join(
    m1_daily_calories, m1_daily_mets,
    by = join_by(Id, ActivityDate)
) %>%
    # full_join(m1_daily_sleep, by = join_by(Id, ActivityDate)) %>% Omit sleep
    full_join(m1_daily_steps, by = join_by(Id, ActivityDate)) %>%
    full_join(m1_daily_intensities, by = join_by(Id, ActivityDate)) %>%
    rename(TotalSteps = "StepTotal")

## Month 2 datasets (visual reminder)
#  - dailyActivity
#  - dailyCalories
#  - dailyIntensities
#  - dailySteps
#  - sleepDay
#  - hourlyCalories
#  - hourlyIntensities
#  - hourlySteps
#  - minuteCaloriesNarrow
#  - minuteIntensitiesNarrow
#  - minuteMETsNarrow
#  - minuteSleep
#  - minuteStepsNarrow
#  - minuteCaloriesWide
#  - minuteIntensitiesWide
#  - minuteStepsWide
#  - heartrate_seconds
#  - weightLogInfo

# Month 2 needs:
# calories, intensities, and steps rebuilt to conform to unionized data
# daily mets
# daily sleep (needs sleep efficiency column)

## Month 2 Calories
m2_daily_calories <- m2_minute_calories_n %>%
    mutate(
        ActivityDay = date(ActivityMinute),
        ActivityMinute = NULL
    ) %>%
    group_by(Id, ActivityDay) %>%
    summarise(Calories = sum(Calories)) %>%
    rename(ActivityDate = "ActivityDay")

# Month 2 METs // Divide by 10
m2_daily_mets <- m2_minute_mets_n %>%
    mutate(
        ActivityDay = date(ActivityMinute),
        ActivityMinute = NULL,
        METs = (METs / 10)
    ) %>%
    group_by(Id, ActivityDay) %>%
    summarise(METs = sum(METs >= 2.4)) %>%
    rename(ActivityDate = "ActivityDay")

## Mother 2 Steps
m2_daily_steps <- m2_minute_steps_n %>%
    mutate(
        ActivityDay = date(ActivityMinute),
        ActivityMinute = NULL
    ) %>%
    group_by(Id, ActivityDay) %>%
    summarise(StepTotal = sum(Steps)) %>%
    rename(ActivityDate = "ActivityDay")

## Month 2 Intensities
m2_daily_intensities <- m2_minute_intensities_n %>%
    mutate(
        ActivityDay = date(ActivityMinute),
        ActivityMinute = NULL
    ) %>%
    group_by(Id, ActivityDay) %>%
    summarise(
        SedentaryMinutes = sum(Intensity == 0),
        LightlyActiveMinutes = sum(Intensity == 1),
        FairlyActiveMinutes = sum(Intensity == 2),
        VeryActiveMinutes = sum(Intensity == 3)
    ) %>%
    rename(ActivityDate = "ActivityDay")

## Month 2 Sleep
# value column 1 = asleep 2 = restless 3 = awake
# group by logId, use restless to calculate sleep efficiency
# m2_daily_sleep <- m2_daily_sleep %>%
#     mutate(
#         Efficiency = (100 * TotalMinutesAsleep / TotalTimeInBed)
#     ) %>%
#     rename(ActivityDate = "SleepDay")
m2_daily_sleep <- m2_minute_sleep %>%
    rename(
        LogId = "logId",
        SleepDay = "date"
    ) %>%
    mutate(
        SleepDay = date(SleepDay)
    ) %>%
    group_by(Id, SleepDay) %>%
    summarize(
        TotalSleepRecords = n_distinct(LogId),
        TotalMinutesAsleep = sum(value == 1),
        TotalMinutesRestless = sum(value == 2),
        TotalTimeInBed = n(),
        Efficiency = (100 * TotalMinutesAsleep / TotalTimeInBed),
        Inefficiency = (100 * TotalMinutesRestless / TotalTimeInBed)
    ) %>%
    rename(ActivityDate = "SleepDay")

## Month 2 Data Collage
m2_daily_activity_final <- full_join(
    m2_daily_calories, m2_daily_mets,
    by = join_by(Id, ActivityDate)
) %>%
    # full_join(m2_daily_sleep, by = join_by(Id, ActivityDate)) %>% Omit sleep
    full_join(m2_daily_steps, by = join_by(Id, ActivityDate)) %>%
    full_join(m2_daily_intensities, by = join_by(Id, ActivityDate)) %>%
    rename(TotalSteps = "StepTotal")

## Uncomment for a difference comparison between original and new activity sheets
# m1_daily_activity_trimmed <- m1_daily_activity %>%
#     mutate(
#         TotalDistance = NULL,
#         TrackerDistance = NULL,
#         LoggedActivitiesDistance = NULL,
#         VeryActiveDistance = NULL,
#         ModeratelyActiveDistance = NULL,
#         LightActiveDistance = NULL,
#         SedentaryActiveDistance = NULL,
#     )

# m2_daily_activity_trimmed <- m2_daily_activity %>%
#     mutate(
#         TotalDistance = NULL,
#         TrackerDistance = NULL,
#         LoggedActivitiesDistance = NULL,
#         VeryActiveDistance = NULL,
#         ModeratelyActiveDistance = NULL,
#         LightActiveDistance = NULL,
#         SedentaryActiveDistance = NULL,
#     )

# m1_daily_activity_final_trimmed <- m1_daily_activity_final %>% #nolint
#     mutate(
#         METs = NULL
#     )

# m2_daily_activity_final_trimmed <- m2_daily_activity_final %>% #nolint
#     mutate(
#         METs = NULL
#     )

# m1_diffs <- symdiff(m1_daily_activity_trimmed, m1_daily_activity_final_trimmed)
# m2_diffs <- symdiff(m2_daily_activity_trimmed, m2_daily_activity_final_trimmed)
# m1_daily_activity_unified <- union(m1_daily_activity_trimmed, m1_daily_activity_final_trimmed)
# view(m1_daily_activity_unified)
## A union of these datasets produces duplicates as
## a result of very small degree in calculation
## (For example, 1817 calories vs 1817.0229 calories)

all_daily_activity <- union(m1_daily_activity_final, m2_daily_activity_final) %>% # nolint
    distinct(Id, ActivityDate, .keep_all = TRUE)

full_period_summary <- all_daily_activity %>%
    group_by(Id) %>%
    summarise(
        TotalDaysMonitored = n(),
        FirstReportingDate = min(ActivityDate),
        LastReportingDate = max(ActivityDate),
        TotalDaysInPeriod = abs(as.numeric(difftime(FirstReportingDate, LastReportingDate))), # nolint
        PercentageDaysReporting = (100 * TotalDaysMonitored) / TotalDaysInPeriod, # nolint
        #
        TotalCaloriesBurned = sum(Calories),
        AverageCaloriesBurned = mean(Calories),
        HighestCaloriesBurned = max(Calories),
        #
        TotalMETs = sum(METs, na.rm = TRUE),
        AverageMETs = mean(METs, na.rm = TRUE),
        HighestMETs = max(METs, na.rm = TRUE),
        #
        TotalSteps = sum(TotalSteps),
        AverageSteps = mean(TotalSteps),
        HighestSteps = max(TotalSteps),
        #
        TotalSedentaryMinutes = sum(SedentaryMinutes),
        AverageSedentaryMinutes = mean(SedentaryMinutes),
        HighestSedentaryMinutes = max(SedentaryMinutes),
        #
        TotalLightlyActiveMinutes = sum(LightlyActiveMinutes),
        AverageLightlyActiveMinutes = mean(LightlyActiveMinutes),
        HighestLightlyActiveMinutes = max(LightlyActiveMinutes),
        #
        TotalFairlyActiveMinutes = sum(FairlyActiveMinutes),
        AverageFairlyActiveMinutes = mean(FairlyActiveMinutes),
        HighestFairlyActiveMinutes = max(FairlyActiveMinutes),
        FairlyActiveBurnoutRatio = round(100 * (HighestFairlyActiveMinutes / TotalFairlyActiveMinutes), digits = 1), # nolint
        #
        TotalVeryActiveMinutes = sum(VeryActiveMinutes),
        AverageVeryActiveMinutes = mean(VeryActiveMinutes),
        HighestVeryActiveMinutes = max(VeryActiveMinutes),
        VeryActiveBurnoutRatio = round(100 * (HighestVeryActiveMinutes / TotalVeryActiveMinutes), digits = 1), # nolint
    ) %>%
    filter(Id != 2891001357) %>%
    # Some PercentageDaysReporting entries are > 100%,
    # likely due to a quirk in how days in the period are calculated
    mutate(
        PercentageDaysReporting = case_when(
            PercentageDaysReporting > 100 ~ 100,
            PercentageDaysReporting <= 100 ~ round(PercentageDaysReporting, digits = 1) # nolint
        ),
        ReportingGroup = case_when(
            PercentageDaysReporting == 100 ~ "=100%",
            PercentageDaysReporting >= 90 & PercentageDaysReporting < 100 ~ "90-99%", # nolint
            PercentageDaysReporting >= 80 & PercentageDaysReporting < 90 ~ "80-89%", # nolint
            PercentageDaysReporting < 80 ~ "<80%"
        )
    )

participant_summary <- full_period_summary %>%
    summarise(
        NumberOfParticipants = n(),
        ShortestPeriodParticipation = min(TotalDaysInPeriod),
        LongestPeriodParticipation = max(TotalDaysInPeriod),
        PercantageFullyReporting = 100 * sum(PercentageDaysReporting == 100) / NumberOfParticipants # nolint
    )

# 85% of our participants committed fully to the program (reporting every day.)
# Of those who didn't, the lowest percentage of days reported was only 85%.
# Note: It's hard to say if the high reporting rate is a sign of enthusiasm
# for the device. It could be that people indepdently enthusiastic about their
# health are drawn to using such devices, or that the participants really wanted
# that $10 provided by Amazon.

participant_pie <- full_period_summary %>%
    group_by(ReportingGroup) %>%
    summarise(
        fraction = n() / nrow(.),
    ) %>%
    mutate(
        ymax = cumsum(fraction),
        ymin = c(0, head(ymax, n = -1)),
        labelPosition = (ymax + ymin) / 2,
        label = paste0("(", round(100 * fraction, digits = 2), "%)"), # nolint
    ) %>%
    ggplot(aes(
        ymax = ymax,
        ymin = ymin,
        xmax = 4,
        xmin = 3,
        fill = ReportingGroup
    )) +
    geom_rect() +
    geom_text(
        x = 2.2,
        aes(y = labelPosition, label = label, color = ReportingGroup, fontface = "bold"), # nolint
        show.legend = FALSE,
        size = 8
    ) +
    scale_fill_brewer(palette = 3) +
    scale_color_brewer(palette = 3) +
    coord_polar(theta = "y") +
    xlim(c(-1, 4)) +
    theme_void() +
    theme(
        legend.title = element_text(size = 26),
        legend.key.size = unit(2, "cm"),
        legend.text = element_text(size = 22)
    ) +
    labs(fill = "Days Reporting")
participant_pie




# BMI
all_weight_logs <- union(m1_weight_log, m2_weight_log)

first_measurement_dates <- all_weight_logs %>%
    select(Id, Date, WeightKg, BMI) %>%
    group_by(Id) %>%
    filter(
        Date == min(Date)
    ) %>%
    rename(
        FirstWeightKg = "WeightKg",
        FirstBMI = "BMI"
    )

last_measurement_dates <- all_weight_logs %>%
    select(Id, Date, WeightKg, BMI) %>%
    group_by(Id) %>%
    filter(
        Date == max(Date)
    ) %>%
    rename(
        LastWeightKg = "WeightKg",
        LastBMI = "BMI"
    )

all_measurement_changes <- first_measurement_dates %>%
    full_join(last_measurement_dates, by = join_by(Id)) %>%
    mutate(
        WeightKgChange = LastWeightKg - FirstWeightKg,
        BMIChange = LastBMI - FirstBMI
    )

all_weight_summary <- all_weight_logs %>%
    select(Id, Date, WeightKg, BMI) %>%
    group_by(Id) %>%
    summarise(
        TotalMeasurements = n()
    ) %>%
    full_join(all_measurement_changes, by = join_by(Id))

all_weight_changes <- all_weight_summary %>%
    filter(WeightKgChange >= .5 | WeightKgChange <= -.5)

weight_changes_graph <- all_weight_changes %>%
    ggplot(
        mapping = aes(
            x = as.character(Id),
            y = WeightKgChange,
            fill = as.factor(Id)
        ),
    ) +
    geom_bar(
        stat = "identity",
        width = .8,
    ) +
    labs(
        title = "Changes in Participant Weight (kg)",
        subtitle = "all changes > .5 kg",
        x = "Participant ID",
        y = "Weight Change (kg)"
    ) +
    geom_hline(yintercept = 0, lwd = 0.8, colour = "#272525") +
    theme(
        legend.position = "none",
        text = element_text(size = 24),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"),
        axis.text.x.bottom = element_text(angle = 25, size = 20, face = "bold")
    )
weight_changes_graph

## Of all participants, only 13 reported their weight,
## and of those who reported their weight, only 4
## saw a change greater than half a pound over the
## study period.

overweight_participants_logs <- all_weight_summary %>%
    filter(FirstBMI > 25 & TotalMeasurements > 1) %>%
    select(Id) %>%
    left_join(all_weight_logs, by = join_by(Id))

## At least nine participants are overweight (BMI > 25),
## with 2 being obese. Assuming these participants had
## weight loss targets, we can compare their change in
## weight against the number of times they reported.

bmi_v_measurements_graph <- overweight_participants_logs %>%
    ggplot(
        mapping = aes(
            x = Date,
            y = BMI,
            fill = "white",
            color = as.factor(Id),
            size = 3
        )
    ) +
    labs(
        title = "BMI Over Time (Overweight)",
        subtitle = "each point a BMI log"
    ) +
    geom_point() +
    theme(
        legend.position = "none",
        text = element_text(size = 20),
        plot.title = element_text(size = 32, face = "bold"),
        axis.title.x = element_text(size = 28, face = "bold"),
        axis.title.y = element_text(size = 28, face = "bold"),
        axis.text.x.bottom = element_text(size = 22, face = "bold")
    )
bmi_v_measurements_graph

## Participants that measured their weight showed a very slight
## decrease in their weight, by the end of the study, although
## this is hard to separate from natural variations in weight.
## One poor individual saw an increase in 4kg in 3 days and
## then never reported again.

normal_weight_participants_logs <- all_weight_summary %>%
    filter(FirstBMI <= 25 & TotalMeasurements > 1) %>%
    select(Id) %>%
    left_join(all_weight_logs, by = join_by(Id))

### A graph of "healthy" BMI participants
normal_bmi_v_t_graph <- normal_weight_participants_logs %>%
    ggplot(
        mapping = aes(
            x = Date,
            y = BMI,
            fill = "white",
            color = as.factor(Id),
            size = 4
        )
    ) +
    labs(
        title = "BMI Over Time for BMI <= 25",
        subtitle = "each point a BMI log"
    ) +
    geom_point() +
    theme(
        legend.position = "none",
        text = element_text(size = 20),
        plot.title = element_text(size = 32, face = "bold"),
        axis.title.x = element_text(size = 28, face = "bold"),
        axis.title.y = element_text(size = 28, face = "bold"),
        axis.text.x.bottom = element_text(size = 22, face = "bold")
    )
normal_bmi_v_t_graph


# calories per step during light activity,
all_minute_calories <- union(m1_minute_calories_n, m2_minute_calories_n)
all_minute_steps <- union(m1_minute_steps_n, m2_minute_steps_n)

all_minute_light_activity <- union(m1_minute_intensities_n, m2_minute_intensities_n) %>% # nolint
    filter(Intensity == 1)

all_minute_fairly_activity <- union(m1_minute_intensities_n, m2_minute_intensities_n) %>% # nolint
    filter(Intensity == 2)

all_minute_very_activity <- union(m1_minute_intensities_n, m2_minute_intensities_n) %>% # nolint
    filter(Intensity == 3)

all_minute_activity <- all_minute_light_activity %>%
    union(all_minute_fairly_activity) %>%
    union(all_minute_very_activity)

cal_v_step_v_light_activty <- all_minute_calories %>%
    full_join(all_minute_steps, by = join_by(Id, ActivityMinute)) %>%
    full_join(all_minute_light_activity, by = join_by(Id, ActivityMinute)) %>%
    drop_na() %>%
    group_by(Id) %>%
    summarise(
        TotalMinutes = n(),
        TotalSteps = sum(Steps),
        TotalCalories = sum(Calories)
    ) %>%
    filter(Id != 289100135)

cal_v_step_v_fairly_activty <- all_minute_calories %>%
    full_join(all_minute_steps, by = join_by(Id, ActivityMinute)) %>%
    full_join(all_minute_fairly_activity, by = join_by(Id, ActivityMinute)) %>%
    drop_na() %>%
    group_by(Id) %>%
    summarise(
        TotalMinutes = n(),
        TotalSteps = sum(Steps),
        TotalCalories = sum(Calories)
    ) %>%
    filter(Id != 289100135)

cal_v_step_v_very_activty <- all_minute_calories %>%
    full_join(all_minute_steps, by = join_by(Id, ActivityMinute)) %>%
    full_join(all_minute_very_activity, by = join_by(Id, ActivityMinute)) %>%
    drop_na() %>%
    group_by(Id) %>%
    summarise(
        TotalMinutes = n(),
        TotalSteps = sum(Steps),
        TotalCalories = sum(Calories)
    ) %>%
    filter(Id != 289100135)

cal_v_step_v_all_activty <- all_minute_calories %>%
    full_join(all_minute_steps, by = join_by(Id, ActivityMinute)) %>%
    full_join(all_minute_activity, by = join_by(Id, ActivityMinute)) %>%
    drop_na() %>%
    group_by(Id) %>%
    summarise(
        TotalMinutes = n(),
        TotalSteps = sum(Steps),
        TotalCalories = sum(Calories)
    ) %>%
    filter(Id != 289100135)

## Join with BMI
all_weighted_participants <- all_weight_summary %>%
    select(Id, LastBMI) %>%
    rename(BMI = "LastBMI")

light_activity_v_bmi <- cal_v_step_v_light_activty %>%
    left_join(all_weighted_participants, by = join_by(Id)) %>%
    drop_na()

fairly_activity_v_bmi <- cal_v_step_v_fairly_activty %>%
    left_join(all_weighted_participants, by = join_by(Id)) %>%
    drop_na()

very_activity_v_bmi <- cal_v_step_v_very_activty %>%
    left_join(all_weighted_participants, by = join_by(Id)) %>%
    drop_na()

all_activity_v_bmi <- cal_v_step_v_all_activty %>%
    left_join(all_weighted_participants, by = join_by(Id)) %>%
    drop_na()

light_v_bmi_graph <- light_activity_v_bmi %>%
    ggplot(
        aes(
            x = BMI,
            y = TotalSteps,
            fill = as.factor(Id)
        )
    ) +
    geom_bar(
        stat = "identity",
        width = .8
    ) +
    # scale_fill_brewer(palette = 3) +
    # scale_color_brewer(palette = 3) +
    labs(
        title = "Light Activity Vs. BMI",
        y = "Total Light Activity Steps"
    ) +
    theme(
        legend.position = "none",
        text = element_text(size = 20),
        plot.title = element_text(size = 32, face = "bold"),
        axis.title.x = element_text(size = 28, face = "bold"),
        axis.title.y = element_text(size = 28, face = "bold"),
        axis.text.x.bottom = element_text(size = 22, face = "bold")
    )
light_v_bmi_graph

fairly_v_bmi_graph <- fairly_activity_v_bmi %>%
    ggplot(
        aes(
            x = BMI,
            y = TotalSteps,
            fill = as.factor(Id)
        )
    ) +
    geom_bar(
        stat = "identity",
        width = .8
    ) +
    # scale_fill_brewer(palette = 3) +
    # scale_color_brewer(palette = 3) +
    labs(
        title = "Fairly Active Steps Vs. BMI",
        y = "Total Fairly Active Steps"
    ) +
    theme(
        legend.position = "none",
        text = element_text(size = 20),
        plot.title = element_text(size = 32, face = "bold"),
        axis.title.x = element_text(size = 28, face = "bold"),
        axis.title.y = element_text(size = 28, face = "bold"),
        axis.text.x.bottom = element_text(size = 22, face = "bold")
    )
fairly_v_bmi_graph

very_v_bmi_graph <- very_activity_v_bmi %>%
    ggplot(
        aes(
            x = BMI,
            y = TotalSteps,
            fill = as.factor(Id)
        )
    ) +
    geom_bar(
        stat = "identity",
        width = .8
    ) +
    # scale_fill_brewer(palette = 3) +
    # scale_color_brewer(palette = 3) +
    labs(
        title = "Very Active Steps Vs. BMI",
        y = "Total Very Active Steps"
    ) +
    theme(
        legend.position = "none",
        text = element_text(size = 20),
        plot.title = element_text(size = 32, face = "bold"),
        axis.title.x = element_text(size = 28, face = "bold"),
        axis.title.y = element_text(size = 28, face = "bold"),
        axis.text.x.bottom = element_text(size = 22, face = "bold")
    )
very_v_bmi_graph

all_v_bmi_graph <- all_activity_v_bmi %>%
    ggplot(
        aes(
            x = BMI,
            y = TotalSteps,
            fill = as.factor(Id)
        )
    ) +
    geom_bar(
        stat = "identity",
        width = .8
    ) +
    # scale_fill_brewer(palette = 3) +
    # scale_color_brewer(palette = 3) +
    labs(
        title = "Total Steps Vs. BMI",
        y = "Total Very Active Steps"
    ) +
    theme(
        legend.position = "none",
        text = element_text(size = 20),
        plot.title = element_text(size = 32, face = "bold"),
        axis.title.x = element_text(size = 28, face = "bold"),
        axis.title.y = element_text(size = 28, face = "bold"),
        axis.text.x.bottom = element_text(size = 22, face = "bold")
    )
all_v_bmi_graph

## During light activity, all participants show
## comparable speeds.
light_steps_v_min_v_bmi_graph <- light_activity_v_bmi %>%
    ggplot(
        aes(
            x = BMI,
            y = (TotalSteps / TotalMinutes),
            fill = as.factor(Id)
        )
    ) +
    geom_bar(
        stat = "identity",
        width = .8
    ) +
    # scale_fill_brewer(palette = 3) +
    # scale_color_brewer(palette = 3) +
    labs(
        title = "Light Steps / Minute Vs. BMI",
        y = "Steps/minute"
    ) +
    theme(
        legend.position = "none",
        text = element_text(size = 20),
        plot.title = element_text(size = 32, face = "bold"),
        axis.title.x = element_text(size = 28, face = "bold"),
        axis.title.y = element_text(size = 28, face = "bold"),
        axis.text.x.bottom = element_text(size = 22, face = "bold")
    )
light_steps_v_min_v_bmi_graph

## Interestingly, differences are even more narrow
## during intense activity.
## This suggests that, rather than being lazy,
## heavier individuals complete ambulation
## cycles at similar rates, but do not enjoy the same acceleration
active_steps_v_min_v_bmi_graph <- very_activity_v_bmi %>%
    ggplot(
        aes(
            x = BMI,
            y = (TotalSteps / TotalMinutes),
            fill = as.factor(Id)
        )
    ) +
    geom_bar(
        stat = "identity",
        width = .8
    ) +
    # scale_fill_brewer(palette = 3) +
    # scale_color_brewer(palette = 3) +
    labs(
        title = "Very Active Steps / Minute Vs. BMI",
        y = "Steps/minute"
    ) +
    theme(
        legend.position = "none",
        text = element_text(size = 20),
        plot.title = element_text(size = 32, face = "bold"),
        axis.title.x = element_text(size = 28, face = "bold"),
        axis.title.y = element_text(size = 28, face = "bold"),
        axis.text.x.bottom = element_text(size = 22, face = "bold")
    )
active_steps_v_min_v_bmi_graph

## People with lower BMIs tend to exercise significantly more
fairly_steps_v_min_v_bmi_graph <- fairly_activity_v_bmi %>%
    ggplot(
        aes(
            x = BMI,
            y = (TotalSteps / TotalMinutes),
            fill = as.factor(Id)
        )
    ) +
    geom_bar(
        stat = "identity",
        width = .8
    ) +
    # scale_fill_brewer(palette = 3) +
    # scale_color_brewer(palette = 3) +
    labs(
        title = "Fairly Active Steps / Minute Vs. BMI",
        y = "Steps/minute"
    ) +
    theme(
        legend.position = "none",
        text = element_text(size = 20),
        plot.title = element_text(size = 32, face = "bold"),
        axis.title.x = element_text(size = 28, face = "bold"),
        axis.title.y = element_text(size = 28, face = "bold"),
        axis.text.x.bottom = element_text(size = 22, face = "bold")
    )
fairly_steps_v_min_v_bmi_graph

# Next, we'll look at participant burnout.
# Find highest intensity workout,
# then find the date of that workout,
# Get ratio of (sum of steps before peak / days before peak),
# Get ratio of (sum of steps after peak / days after peak),
# Subtract first ratio from second,
# If there is a 50% decline in steps over the last 7 days,
# Participant may be in a stage of burnout

# A participant who is experiencing burnout may
# be engaged in more physical activity
# than they can tolerate (mentally or phsyically) or
# can allow time for. Crudely, we can identify
# burnout by looking at peaks in activity. This analysis looks
# at the day with the highest elevation and observes if
# there is a decline (>33%) in physical activity in the
# following week compared to their lifetime average.

# Identify burnout using METs.

# Similarly, we can identify sudden bursts of motivation.

all_daily_mets <- union(m1_daily_mets, m2_daily_mets) %>%
    filter(Id != 2891001357 & METs > 0)

peak_mets_dates <- all_daily_mets %>%
    filter(METs == max(METs) & Id != 2891001357)

all_daily_mets <- all_daily_mets %>%
    inner_join(peak_mets_dates, by = join_by(Id))

prepeak_periods <- all_daily_mets %>%
    filter(ActivityDate.x <= ActivityDate.y) %>%
    select(-ActivityDate.y, -METs.y) %>%
    rename(METs = "METs.x", ActivityDate = "ActivityDate.x")

postpeak_periods <- all_daily_mets %>%
    filter(ActivityDate.x > ActivityDate.y) %>%
    select(-ActivityDate.y, -METs.y) %>%
    rename(METs = "METs.x", ActivityDate = "ActivityDate.x")

prepeak_summary <- prepeak_periods %>%
    group_by(Id) %>%
    summarise(
        PrepeakPeriodDays = n(),
        PrepeakTotalMETs = sum(METs),
        PrepeakAverage = PrepeakTotalMETs / PrepeakPeriodDays
    )

postpeak_summary <- postpeak_periods %>%
    group_by(Id) %>%
    summarise(
        PostpeakPeriodDays = n(),
        PostpeakTotalMets = sum(METs),
        PostpeakAverage = PostpeakTotalMets / PostpeakPeriodDays
    )

burnout_summary <- prepeak_summary %>%
    full_join(postpeak_summary, by = join_by(Id)) %>%
    filter(PostpeakPeriodDays >= 7) %>%
    filter(PrepeakAverage > (PostpeakAverage * 1.33)) # 33% Decline factor

burnout_daily_mets <- all_daily_mets %>%
    inner_join(burnout_summary, by = join_by(Id)) %>%
    rename(METs = "METs.x", ActivityDate = "ActivityDate.x")

burnout_daily_mets$week_num <- strftime(burnout_daily_mets$ActivityDate, format = "%V") # nolint

## Use this...
burnout_summary_temp <- burnout_summary %>%
    select(Id)

burnout_weekly_mets_temp <- burnout_daily_mets %>%
    select(Id, ActivityDate, METs, week_num) %>%
    group_by(Id, week_num) %>%
    summarise(
        ActivityDate = min(ActivityDate),
        AverageMETs = mean(METs)
    ) %>%
    mutate(
        week_num = NULL,
        doColor = FALSE
    )

peak_mets_dates_temp <- peak_mets_dates %>%
    rename(AverageMETs = "METs") %>%
    right_join(burnout_summary_temp, by = join_by(Id)) %>%
    mutate(
        doColor = TRUE
    )

burnout_weekly_graph_data <- union(burnout_weekly_mets_temp, peak_mets_dates_temp) # nolint

burnout_graph <- burnout_weekly_graph_data %>%
    ggplot(
        aes(
            x = ActivityDate,
            y = AverageMETs,
            color = as.factor(Id),
            fill = "Burnout Risks"
        )
    ) +
    geom_line(linewidth = 2) +
    geom_point(shape = 21, fill = "white", size = 5, stroke = 2) + # Base
    geom_point(aes(color = as.factor(Id))) + # Dotted center
    geom_point(
        data = peak_mets_dates_temp,
        aes(
            x = ActivityDate,
            y = AverageMETs,
            color = as.factor(Id),
            size = 8,
            stroke = 1,
            alpha = 1
        )
    ) +
    labs(
        title = "Weekly Average METs",
        subtitle = "as a predictor of burnout",
        fill = "Burnout Risks",
        y = "METs",
        x = ""
    ) +
    theme(
        legend.position = "none",
        text = element_text(size = 20),
        plot.title = element_text(size = 32, face = "bold"),
        axis.title.x = element_text(size = 28, face = "bold"),
        axis.title.y = element_text(size = 28, face = "bold"),
        axis.text.y.left = element_text(size = 20, face = "bold"),
        axis.text.x.bottom = element_text(size = 22, face = "bold"),
        legend.title = element_text(size = 24, face = "bold"),
        legend.text = element_text(size = 16, face = "bold"),
    )
burnout_graph


general_peaks_graph <- peak_mets_dates %>%
    ggplot(
        aes(
            x = ActivityDate,
            y = METs,
            color = as.factor(Id),
            fill = "Burnout Risks"
        )
    ) +
    geom_point(shape = 21, fill = "white", size = 5, stroke = 2) + # Base
    geom_point(aes(color = as.factor(Id))) + # Dotted center
    labs(
        title = "Time of Peak METs",
        subtitle = "each point a participant",
        y = "METs",
        x = ""
    ) +
    scale_fill_brewer(palette = 3) +
    # scale_color_brewer(palette = 3) +
    theme(
        legend.position = "none",
        text = element_text(size = 20),
        plot.title = element_text(size = 32, face = "bold"),
        axis.title.x = element_text(size = 28, face = "bold"),
        axis.title.y = element_text(size = 28, face = "bold"),
        axis.text.y.left = element_text(size = 20, face = "bold"),
        axis.text.x.bottom = element_text(size = 22, face = "bold"),
        legend.title = element_text(size = 24, face = "bold"),
        legend.text = element_text(size = 16, face = "bold"),
    )
general_peaks_graph

burnout_peaks_graph <- peak_mets_dates_temp %>%
    ggplot(
        aes(
            x = ActivityDate,
            y = AverageMETs,
            color = as.factor(Id),
            fill = "Burnout Risks"
        )
    ) +
    geom_point(shape = 21, fill = "white", size = 5, stroke = 2) + # Base
    geom_point(aes(color = as.factor(Id))) + # Dotted center
    labs(
        title = "Time of Peak METs",
        subtitle = "each point a participant at risk of burnout",
        y = "METs",
        x = ""
    ) +
    scale_fill_brewer(palette = 3) +
    theme(
        legend.position = "none",
        text = element_text(size = 20),
        plot.title = element_text(size = 32, face = "bold"),
        axis.title.x = element_text(size = 28, face = "bold"),
        axis.title.y = element_text(size = 28, face = "bold"),
        axis.text.y.left = element_text(size = 20, face = "bold"),
        axis.text.x.bottom = element_text(size = 22, face = "bold"),
        legend.title = element_text(size = 24, face = "bold"),
        legend.text = element_text(size = 16, face = "bold"),
    )
burnout_peaks_graph


## Identified seven individuals at risk of burning out.
## Most peaks early, with >30 days since their last
## peak METs performance.

all_heartrate_seconds <- union(m1_heartrate_seconds, m2_heartrate_seconds) %>%
    rename(ActivityMinute = "Time", Heartrate = "Value")

## Reminder:
# all_minute_light_activity
# all_minute_fairly_activity
# all_minute_very_activity
# all_minute_activity

all_minute_sedentary_activity <- union(m1_minute_intensities_n, m2_minute_intensities_n) %>% # nolint
    filter(Intensity == 0) %>%
    mutate(Intensity = NULL)

all_minute_sleep <- union(m1_minute_sleep, m2_minute_sleep) %>%
    select(Id, date) %>%
    rename(ActivityMinute = "date") %>%
    mutate(ActivityMinute = ActivityMinute + seconds(30))

all_sedentary_no_sleep <- setdiff(all_minute_sedentary_activity, all_minute_sleep) # nolint

## To prevent definition clobbering
all_daily_mets_temp <- union(m1_daily_mets, m2_daily_mets) %>%
    filter(Id != 2891001357 & METs > 0)

heartrate_collage_sedentary <- all_heartrate_seconds %>%
    inner_join(
        all_sedentary_no_sleep,
        by = join_by(Id, ActivityMinute)
    ) %>%
    mutate(
        ActivityDate = date(ActivityMinute),
        ActivityMinute = NULL
    ) %>%
    group_by(Id, ActivityDate) %>%
    summarise(
        AverageHeartrate = round(mean(Heartrate))
    ) %>%
    inner_join(
        all_daily_mets_temp,
        by = join_by(Id, ActivityDate)
    )

heartrate_collage_period <- heartrate_collage_sedentary

heartrate_collage_period$month_num <- strftime(heartrate_collage_period$ActivityDate, format = "%m") # nolint

heartrate_collage_period <- heartrate_collage_period %>%
    group_by(Id, month_num) %>%
    summarise(
        AverageHeartrate = round(mean(AverageHeartrate)),
        AverageMETs = round(mean(METs))
    ) %>%
    mutate(
        month_num = NULL
    )

heartrate_graph <- heartrate_collage_period %>%
    ggplot(
        mapping = aes(
            x = AverageMETs,
            y = AverageHeartrate,
            fill = "white",
            color = as.factor(Id),
            size = 3
        )
    ) +
    labs(
        title = "Heartrate vs. Activity Levels",
        subtitle = "during sedentary, non-sleeping hours",
        y = "Average Daily Heartrate",
        x = "Average Daily METs"
    ) +
    geom_point() +
    theme(
        legend.position = "none",
        text = element_text(size = 20),
        plot.title = element_text(size = 32, face = "bold"),
        axis.title.x = element_text(size = 28, face = "bold"),
        axis.title.y = element_text(size = 28, face = "bold"),
        axis.text.y.left = element_text(size = 20, face = "bold"),
        axis.text.x.bottom = element_text(size = 22, face = "bold")
    )
heartrate_graph

activity_summary_temp <- full_period_summary %>%
    select(
        Id,
        AverageSedentaryMinutes,
        AverageLightlyActiveMinutes,
        AverageFairlyActiveMinutes,
        AverageVeryActiveMinutes
    )

heartrate_collage_period_temp <- heartrate_collage_period %>%
    inner_join(
        activity_summary_temp,
        by = join_by(Id)
    )

heartrate_lightly_graph <- heartrate_collage_period_temp %>%
    ggplot(
        mapping = aes(
            x = AverageLightlyActiveMinutes,
            y = AverageHeartrate,
            fill = "white",
            color = as.factor(Id),
            size = 3
        )
    ) +
    labs(
        title = "Heartrate vs. Light Activity",
        # subtitle = "",
        y = "Average Daily Heartrate",
        x = "Average Minutes of Light Activity"
    ) +
    geom_point() +
    theme(
        legend.position = "none",
        text = element_text(size = 20),
        plot.title = element_text(size = 32, face = "bold"),
        axis.title.x = element_text(size = 28, face = "bold"),
        axis.title.y = element_text(size = 28, face = "bold"),
        axis.text.y.left = element_text(size = 20, face = "bold"),
        axis.text.x.bottom = element_text(size = 22, face = "bold")
    )
heartrate_lightly_graph

heartrate_fairly_graph <- heartrate_collage_period_temp %>%
    ggplot(
        mapping = aes(
            x = AverageFairlyActiveMinutes,
            y = AverageHeartrate,
            fill = "white",
            color = as.factor(Id),
            size = 3
        )
    ) +
    labs(
        title = "Heartrate vs. Fairly Active Time",
        # subtitle = "",
        y = "Average Daily Heartrate",
        x = "Average Minutes Fairly Active"
    ) +
    geom_point() +
    theme(
        legend.position = "none",
        text = element_text(size = 20),
        plot.title = element_text(size = 32, face = "bold"),
        axis.title.x = element_text(size = 28, face = "bold"),
        axis.title.y = element_text(size = 28, face = "bold"),
        axis.text.y.left = element_text(size = 20, face = "bold"),
        axis.text.x.bottom = element_text(size = 22, face = "bold")
    )
heartrate_fairly_graph

heartrate_very_graph <- heartrate_collage_period_temp %>%
    ggplot(
        mapping = aes(
            x = AverageVeryActiveMinutes,
            y = AverageHeartrate,
            fill = "white",
            color = as.factor(Id),
            size = 3
        )
    ) +
    labs(
        title = "Heartrate vs. Very Active Time",
        # subtitle = "",
        y = "Average Daily Heartrate",
        x = "Average Minutes Very Active"
    ) +
    geom_point() +
    theme(
        legend.position = "none",
        text = element_text(size = 20),
        plot.title = element_text(size = 32, face = "bold"),
        axis.title.x = element_text(size = 28, face = "bold"),
        axis.title.y = element_text(size = 28, face = "bold"),
        axis.text.y.left = element_text(size = 20, face = "bold"),
        axis.text.x.bottom = element_text(size = 22, face = "bold")
    )
heartrate_very_graph

## Higher activity levels generally associated with
## a lower heart rate--activity trains the heart,
## making it stronger, therefore needing to beat
## less often.

## However, if true, the amount of time actively
## appears to somewhat correlate with the strength
## of the heart.
## It's likely that the effects of good health
## are compounding: a stronger heart means
## you are more able to engage in more physically
## intense activities for longer periods of time,
## which in turn leads to an even stronger heart.

## Sleep efficiency correlates with levels of activity (METs)
## How does sleep? Do more active participants get better
## sleep?

m1_daily_sleep_temp <- m1_daily_sleep %>%
    select(
        Id,
        ActivityDate,
        TotalMinutesAsleep,
        TotalMinutesRestless,
        Efficiency,
        Inefficiency
    )

m2_daily_sleep_temp <- m2_daily_sleep %>%
    select(
        Id,
        ActivityDate,
        TotalMinutesAsleep,
        TotalMinutesRestless,
        Efficiency,
        Inefficiency
    )

all_daily_sleep <- union(m1_daily_sleep_temp, m2_daily_sleep_temp) %>%
    group_by(Id) %>%
    summarise(
        AverageMinutesAsleep = mean(TotalMinutesAsleep),
        AverageMinutesRestless = mean(TotalMinutesRestless),
        AverageEfficiency = mean(Efficiency),
        AverageInefficiency = mean(Inefficiency)
    )

heart_sleep_collage <- inner_join(
    all_daily_sleep, heartrate_collage_period,
    by = join_by(Id)
)

sleep_efficiency_v_activity <- heart_sleep_collage %>%
    ggplot(
        mapping = aes(
            x = AverageEfficiency,
            y = AverageMETs,
            fill = "white",
            color = as.factor(Id),
            size = 3
        )
    ) +
    labs(
        title = "Activity vs. Sleep Efficiency",
        subtitle = "Efficiency = (Minutes Asleep / Minutes in Bed)",
        y = "Activity Levels",
        x = "Efficiency (%)"
    ) +
    geom_point() +
    theme(
        legend.position = "none",
        text = element_text(size = 20),
        plot.title = element_text(size = 32, face = "bold"),
        axis.title.x = element_text(size = 28, face = "bold"),
        axis.title.y = element_text(size = 28, face = "bold"),
        axis.text.y.left = element_text(size = 20, face = "bold"),
        axis.text.x.bottom = element_text(size = 22, face = "bold")
    )
sleep_efficiency_v_activity

sleep_inefficiency_v_activity <- heart_sleep_collage %>%
    ggplot(
        mapping = aes(
            x = AverageInefficiency,
            y = AverageMETs,
            fill = "white",
            color = as.factor(Id),
            size = 3
        )
    ) +
    labs(
        title = "Activity vs. Sleep Efficiency",
        subtitle = "Infficiency = (Minutes Restless / Minutes in Bed)",
        y = "Activity Levels",
        x = "Inefficiency (%)"
    ) +
    geom_point() +
    theme(
        legend.position = "none",
        text = element_text(size = 20),
        plot.title = element_text(size = 32, face = "bold"),
        axis.title.x = element_text(size = 28, face = "bold"),
        axis.title.y = element_text(size = 28, face = "bold"),
        axis.text.y.left = element_text(size = 20, face = "bold"),
        axis.text.x.bottom = element_text(size = 22, face = "bold")
    )
sleep_inefficiency_v_activity

time_asleep_v_activity <- heart_sleep_collage %>%
    ggplot(
        mapping = aes(
            x = AverageMETs,
            y = AverageMinutesAsleep,
            fill = "white",
            color = as.factor(Id),
            size = 3
        )
    ) +
    labs(
        title = "Activity vs. Time Asleep",
        subtitle = "total minutes asleep (not just in bed)",
        x = "Activity Levels",
        y = "Time Asleep",
    ) +
    geom_point() +
    theme(
        legend.position = "none",
        text = element_text(size = 20),
        plot.title = element_text(size = 32, face = "bold"),
        axis.title.x = element_text(size = 28, face = "bold"),
        axis.title.y = element_text(size = 28, face = "bold"),
        axis.text.y.left = element_text(size = 20, face = "bold"),
        axis.text.x.bottom = element_text(size = 22, face = "bold")
    )
time_asleep_v_activity

## There is a group for whom activity does not seem to impact time alseep.

# Look into
#  - Does logging encourage better habits?

## Time of day most active
m1_minute_mets_temp <- m1_minute_mets_n %>%
    mutate(METs = METs / 10) # why Fitabase. just why

m2_minute_mets_temp <- m2_minute_mets_n %>%
    mutate(METs = METs / 10)

all_active_minute_mets <- union(m1_minute_mets_temp, m2_minute_mets_temp)

hourly_mets_summary <- all_active_minute_mets %>%
    filter(METs >= 2.4) %>%
    mutate(
        Minutes = as_datetime((hour(ActivityMinute) * 60 * 60) + minute(ActivityMinute) * 60) # nolint
    ) %>%
    group_by(Id, Minutes) %>%
    summarise(
        AverageMETs = mean(METs)
    )

# max_mets <- hourly_mets_summary %>%
#     summarise(
#         MaxAverageMETs = max(AverageMETs)
#     )

hourlys_mets_by_id_graph <- hourly_mets_summary %>%
    ggplot(aes(
        x = Minutes,
        y = AverageMETs
    )) +
    geom_bin_2d() +
        labs(
        fill = "Count",
        title = "Intensity of Activity Throughout Day",
        subtitle = "for light and greater intensities",
        x = "Time of Day",
        y = "METs"
    ) +
    scale_x_datetime(
        date_labels = "%H:%M",
    ) +
    theme(
        text = element_text(size = 20),
        plot.title = element_text(size = 32, face = "bold"),
        axis.title.x = element_text(size = 28, face = "bold"),
        axis.title.y = element_text(size = 28, face = "bold"),
        axis.text.y.left = element_text(size = 20, face = "bold"),
        axis.text.y.right = element_text(size = 20, face = "bold"),
        axis.text.x.bottom = element_text(size = 22, face = "bold"),
    ) +
    coord_polar(theta = "x", clip = "off") +
    ylim(c(2.5, 13))
hourlys_mets_by_id_graph