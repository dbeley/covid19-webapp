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

df_c <- read.csv(text=getURL(
  "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv"
), stringsAsFactors = FALSE)
df_d <- read.csv(text=getURL(
  "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv"
), stringsAsFactors = FALSE)
df_r <- read.csv(text=getURL(
  "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv"
), stringsAsFactors = FALSE)
current_year <- year(Sys.Date())
current_month <- month(Sys.Date())

df_c <- pivot_longer(df_c, cols=5:ncol(df_c), names_to="Date", values_to="Confirmed")
df_d <- pivot_longer(df_d, cols=5:ncol(df_d), names_to="Date", values_to="Deaths")
df_r <- pivot_longer(df_r, cols=5:ncol(df_r), names_to="Date", values_to="Recovered")

df <- df_c %>% inner_join(df_d) %>% inner_join(df_r)
df <- df %>%
  mutate(
    Date = as.Date(substr(Date, 2, length(Date)), format = "%m.%d.%y"),
    Active.Cases = Confirmed - (Deaths + Recovered)
  ) %>%
  arrange(desc(Date))

rm(df_c, df_d, df_r)

df_a <- df %>%
  group_by(Country.Region, Date) %>%
  summarize(Active.Cases = sum(Active.Cases)) %>%
  arrange(desc(Active.Cases)) %>%
  ungroup()

countries <- unique(
  df %>%
    group_by(Country.Region) %>%
    filter(Date == max(Date)) %>%
    arrange(desc(Deaths)) %>%
    pull(Country.Region)
)

dates <- unique(df$Date)
