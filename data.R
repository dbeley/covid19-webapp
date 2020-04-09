# Chargement des données ----
# df_c <-
#   read.csv(
#     "/home/david/Documents/COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv"
#   )
# df_d <-
#   read.csv(
#     "/home/david/Documents/COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv"
#   )
# df_r <-
#   read.csv(
#     "/home/david/Documents/COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv"
#   )

df_c <- read.csv(
  text = getURL(
    "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
  ),
  stringsAsFactors = FALSE
)
df_d <- read.csv(
  text = getURL(
    "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"
  ),
  stringsAsFactors = FALSE
)
df_r <- read.csv(
  text = getURL(
    "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv"
  ),
  stringsAsFactors = FALSE
)
current_year <- year(Sys.Date())
current_month <- month(Sys.Date())

df_c <-
  pivot_longer(
    df_c,
    cols = 5:ncol(df_c),
    names_to = "Date",
    values_to = "Confirmed"
  )
df_d <-
  pivot_longer(
    df_d,
    cols = 5:ncol(df_d),
    names_to = "Date",
    values_to = "Deaths"
  )
df_r <-
  pivot_longer(
    df_r,
    cols = 5:ncol(df_r),
    names_to = "Date",
    values_to = "Recovered"
  )

df_c <- df_c %>%
  mutate(Date = as.Date(substr(Date, 2, length(Date)), format = "%m.%d.%y"),
         Lat = round(Lat, 2),
         Long = round(Long, 2))
df_d <- df_d %>%
  mutate(Date = as.Date(substr(Date, 2, length(Date)), format = "%m.%d.%y"),
         Lat = round(Lat, 2),
         Long = round(Long, 2))
df_r <- df_r %>%
  mutate(Date = as.Date(substr(Date, 2, length(Date)), format = "%m.%d.%y"),
         Lat = round(Lat, 2),
         Long = round(Long, 2))

df <- df_c %>% inner_join(df_d) %>% inner_join(df_r)

df <- df %>%
  mutate(
    # Date = as.Date(substr(Date, 2, length(Date)), format = "%m.%d.%y"),
    Active.Cases = Confirmed - (Deaths + Recovered),
    Country.Formatted =
      ifelse(
        Province.State == "",
        Country.Region,
        ifelse(
          Province.State == Country.Region,
          Country.Region,
          paste(Country.Region, " (", Province.State, ")", sep = "")
        )
      )
  ) %>%
  arrange(desc(Date))

rm(df_c, df_d, df_r)

# df_a <- df %>%
#   group_by(Country.Formatted, Country.Region, Date) %>%
#   summarize(Active.Cases = sum(Active.Cases)) %>%
#   arrange(desc(Active.Cases)) %>%
#   ungroup()

countries <- unique(
  df %>%
    group_by(Country.Region) %>%
    filter(Date == max(Date)) %>%
    arrange(desc(Active.Cases)) %>%
    pull(Country.Region)
)

dates <- unique(df$Date)

df100 <- df %>%
  group_by(Country.Region, Date) %>%
  summarize(Confirmed = sum(Confirmed)) %>%
  filter(Confirmed > 100) %>%
  group_by(Country.Region) %>%
  summarize(jour100 = Date[which.min(Confirmed > 100)]) %>%
  inner_join(df) %>%
  filter(Date > jour100) %>%
  arrange(Date) %>%
  group_by(Country.Region, Date) %>%
  summarize(Confirmed = sum(Confirmed),
            Deaths = sum(Deaths)) %>%
  group_by(Country.Region) %>%
  mutate(Day = row_number())

countries100 <- unique(df100 %>%
                         arrange(desc(Deaths)) %>%
                         pull(Country.Region))

most_affected_countries100 <- function(n) {
  countries100[1:n]
}