most_affected_countries <- function(n) {
  df %>%
    filter(Date == max(Date)) %>%
    arrange(desc(Active.Cases)) %>%
    slice(1:n) %>%
    pull(Country.Region)
}

df_reactive <- reactive({
  df %>%
    group_by(Country.Region, Date) %>%
    summarize(
      Confirmed = sum(Confirmed),
      Deaths = sum(Deaths),
      Recovered = sum(Recovered),
      Active.Cases = sum(Active.Cases)
    )
})

df_reactive100 <- reactive({
  if (!!input$plotType100 == "Confirmed")
  {
df %>%
  filter(Country.Region %in% !!input$countries100) %>%
  group_by(Country.Region, Date) %>%
  summarize(Confirmed = sum(Confirmed)) %>%
  filter(Confirmed > !!input$slider100) %>%
  group_by(Country.Region) %>%
  summarize(jour100 = Date[which.min(Confirmed > !!input$slider100)]) %>%
  inner_join(df) %>%
  filter(Date > jour100) %>%
  arrange(Date) %>%
  group_by(Country.Region, Date) %>%
  summarize(Confirmed = sum(Confirmed)) %>%
  group_by(Country.Region) %>%
  mutate(Day = row_number())
  }
  else if (!!input$plotType100 == "Deaths")
  {
df %>%
  filter(Country.Region %in% !!input$countries100) %>%
  group_by(Country.Region, Date) %>%
  summarize(Deaths = sum(Deaths)) %>%
  filter(Deaths > !!input$slider100) %>%
  group_by(Country.Region) %>%
  summarize(jour100 = Date[which.min(Deaths > !!input$slider100)]) %>%
  inner_join(df) %>%
  filter(Date > jour100) %>%
  arrange(Date) %>%
  group_by(Country.Region, Date) %>%
  summarize(Deaths = sum(Deaths)) %>%
  group_by(Country.Region) %>%
  mutate(Day = row_number())
  }
})

df_jour_reactive <- reactive({
  df_reactive() %>%
    filter(Date == !!input$inputdate) %>%
    select(Country.Region, Active.Cases, Confirmed, Deaths, Recovered) %>%
    arrange(desc(Active.Cases))
})

df_pays_reactive <- reactive({
  df_reactive() %>%
    filter(Country.Region == !!input$countries2) %>%
    select(Country.Region, Date, Active.Cases, Confirmed, Deaths, Recovered) %>%
    arrange(desc(Date))
})

plot100 <- reactive({
  d <- df_reactive100()
  if (nrow(d) == 0)
    return(NULL)
  if (input$plotType100 == "Deaths") {
    ggplotly(ggplot(d, aes(Day, Deaths, color = Country.Region)) +
               geom_point() + geom_line()
             + labs(x=paste("Days since ", input$slider100, " deceased cases", color="Country", sep="")),
             height = 600)
  }
  else if (input$plotType100 == "Confirmed") {
    ggplotly(ggplot(d, aes(Day, Confirmed, color = Country.Region)) +
               geom_point() + geom_line()
             + labs(x=paste("Days since ", input$slider100, " confirmed cases", color="Country", sep="")),
             height = 600)
  }
})

plot1 <- reactive({
  d <- df_reactive() %>%
    filter(Date > !!input$dateRange[1] &
             Date <= !!(input$dateRange[2] + 1)) %>%
    filter(Country.Region %in% !!input$countries)
  if (nrow(d) == 0)
    return(NULL)
  if (input$plotType == "Active.Cases") {
    ggplotly(
      ggplot(d,
             aes(Date, Active.Cases, color = Country.Region)) +
        geom_point() + geom_line() + labs(y = "Active Cases", color = "Country"),
      height = 600
    )
  }
  else if (input$plotType == "Deaths") {
    ggplotly(ggplot(d, aes(Date, Deaths, color = Country.Region)) +
               geom_point() + geom_line() +
               labs(color="Country"),
             height = 600)
  }
  else if (input$plotType == "Recovered") {
    ggplotly(ggplot(d, aes(Date, Recovered, color = Country.Region)) +
               geom_point() + geom_line() +
               labs(color="Country"),
             height = 600)
  }
  else if (input$plotType == "Confirmed") {
    ggplotly(ggplot(d, aes(Date, Confirmed, color = Country.Region)) +
               geom_point() + geom_line() +
               labs(color="Country"),
             height = 600)
  }
})

plotNewCases <- reactive({
  d <- df_reactive() %>%
    mutate(Diff.Confirmed = Confirmed - lag(Confirmed),
           Diff.Active.Cases = Active.Cases - lag(Active.Cases),
           Diff.Deaths = Deaths - lag(Deaths),
           Diff.Recovered = Recovered - lag(Recovered)) %>%
    filter(Date > !!input$dateRangeNewCases[1] &
             Date <= !!(input$dateRangeNewCases[2] + 1)) %>%
    filter(Country.Region %in% !!input$countriesNewCases)

  if (nrow(d) == 0)
    return(NULL)
  if (input$plotTypeNewCases == "Active.Cases") {
    ggplotly(
      ggplot(d,
             aes(Date, Diff.Active.Cases, fill = Country.Region, label = Diff.Active.Cases)) +
        geom_col(position="dodge") + labs(y="Active Cases", fill="Country") +
        geom_text(position = position_stack(vjust=0.1), check_overlap=T),
      height = 600
    )
  }
  else if (input$plotTypeNewCases == "Deaths") {
    ggplotly(ggplot(d, aes(Date, Diff.Deaths, fill = Country.Region, label = Diff.Deaths)) +
               geom_col(position="dodge") + labs(y="Deaths", fill="Country") +
               geom_text(position = position_stack(vjust=0.1), check_overlap=T),
             height = 600)
  }
  else if (input$plotTypeNewCases == "Recovered") {
    ggplotly(ggplot(d, aes(Date, Diff.Recovered, fill = Country.Region, label = Diff.Recovered)) +
               geom_col(position="dodge") + labs(y="Recovered", fill="Country") +
               geom_text(position = position_stack(vjust=0.1), check_overlap=T),
             height = 600)
  }
  else if (input$plotTypeNewCases == "Confirmed") {
    ggplotly(ggplot(d, aes(Date, Diff.Confirmed, fill = Country.Region, label = Diff.Confirmed)) +
               geom_col(position="dodge") + labs(y="Confirmed", fill="Country") +
               geom_text(position = position_stack(vjust=0.1), check_overlap=T),
             height = 600)
  }
})

df_map_reactive <- reactive({
  df %>%
    group_by(Country.Region, Country.Formatted, Date, Lat, Long) %>%
    summarize(
      Confirmed = sum(Confirmed),
      Deaths = sum(Deaths),
      Recovered = sum(Recovered),
      Active.Cases = sum(Active.Cases)
    ) %>%
    # filter(Date == max(df$Date))
    filter(Date == !!input$mapinputdate)
})

plotmap <- reactive({
  d <- df %>%
    filter(Date == max(Date)) %>%
    group_by(Country.Region, Country.Formatted, Date, Lat, Long) %>%
    summarize(Active.Cases = sum(Active.Cases))
  pal <- colorNumeric(palette = "YlOrRd", domain = d$Active.Cases)
  leaflet() %>%
    addTiles() %>%
    setView(2.2, 48, 3) %>%
    addCircles(
      data = d,
      lat =  ~ Lat,
      lng =  ~ Long,
      weight = 1,
      radius =  ~ sqrt(Active.Cases) * 5000,
      popup =  ~ paste(Country.Formatted, ": ", Active.Cases, " active cases.", sep =
                         ""),
      fillOpacity = 0.7,
      color = ~ pal(Active.Cases)
    )
})

observe({
  proxy <- leafletProxy("plotmap")
  d <- df_map_reactive()
  if (input$mapType == "Confirmed") {
    pal <- colorNumeric(palette = "YlOrRd", domain = d$Confirmed)
    proxy %>%
      clearShapes() %>%
      addCircles(
        data = d,
        lat =  ~ Lat,
        lng =  ~ Long,
        weight = 1,
        radius =  ~ sqrt(Confirmed) * 5000,
        popup =  ~ paste(Country.Formatted, ": ", Confirmed, " confirmed cases.", sep =
                           ""),
        fillOpacity = 0.7,
        color = ~ pal(Confirmed)
      )
  }
  else if (input$mapType == "Deaths") {
    pal <- colorNumeric(palette = "YlOrRd", domain = d$Deaths)
    proxy %>%
      clearShapes() %>%
      addCircles(
        data = d,
        lat =  ~ Lat,
        lng =  ~ Long,
        weight = 1,
        radius =  ~ sqrt(Deaths) * 5000,
        popup =  ~ paste(Country.Formatted, ": ", Deaths, " deceased cases.", sep =
                           ""),
        fillOpacity = 0.7,
        color = ~ pal(Deaths)
      )
  }
  else if (input$mapType == "Recovered") {
    pal <- colorNumeric(palette = "YlGn", domain = d$Recovered)
    proxy %>%
      clearShapes() %>%
      addCircles(
        data = d,
        lat =  ~ Lat,
        lng =  ~ Long,
        weight = 1,
        radius =  ~ sqrt(Recovered) * 5000,
        popup =  ~ paste(Country.Formatted, ": ", Recovered, " recovered cases.", sep =
                           ""),
        fillOpacity = 0.7,
        color = ~ pal(Recovered)
      )
  }
  else if (input$mapType == "Active.Cases") {
    pal <- colorNumeric(palette = "YlOrRd", domain = d$Active.Cases)
    proxy %>%
      clearShapes() %>%
      addCircles(
        data = d,
        lat =  ~ Lat,
        lng =  ~ Long,
        weight = 1,
        radius =  ~ sqrt(Active.Cases) * 5000,
        popup =  ~ paste(Country.Formatted, ": ", Active.Cases, " active cases.", sep =
                           ""),
        fillOpacity = 0.7,
        color = ~ pal(Active.Cases)
      )
  }
})

# df_date <- df %>%
#   group_by(Country.Region, Date, Lat, Long) %>%
#   summarize(
#     Confirmed = sum(Confirmed),
#     Deaths = sum(Deaths),
#     Recovered = sum(Recovered),
#     Active.Cases = sum(Active.Cases)
#   ) %>%
#   filter(Date == max(df$Date))
#
# leaflet() %>%
#   addTiles() %>%
#   setView(2.2, 48, 2) %>%
#   addCircles(
#     data = df_date,
#     lat =  ~ Lat,
#     lng =  ~ Long,
#     weight = 1,
#     radius =  ~ sqrt(Confirmed) * 5000,
#     popup =  ~ Confirmed
#   )