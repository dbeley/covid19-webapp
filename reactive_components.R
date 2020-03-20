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
    ) %>%
    filter(Country.Region %in% !!input$countries) %>%
    filter(Date > !!input$dateRange[1] &
             Date <= !!(input$dateRange[2] + 1))
})

df_jour_reactive <- reactive({
  df %>%
    filter(Date == !!input$inputdate) %>%
    group_by(Country.Region) %>%
    summarize(
      Confirmed = sum(Confirmed),
      Deaths = sum(Deaths),
      Recovered = sum(Recovered),
      Active.Cases = sum(Active.Cases)
    ) %>%
    select(Country.Region, Confirmed, Deaths, Recovered) %>%
    arrange(desc(Deaths))
})

df_pays_reactive <- reactive({
  df %>%
    filter(Country.Region == !!input$countries2) %>%
    group_by(Country.Region, Date) %>%
    summarize(
      Confirmed = sum(Confirmed),
      Deaths = sum(Deaths),
      Recovered = sum(Recovered),
      Active.Cases = sum(Active.Cases)
    ) %>%
    select(Country.Region, Date, Confirmed, Deaths, Recovered) %>%
    arrange(desc(Date))
})

plot1 <- reactive({
  if (input$plotType == "Active.Cases") {
    ggplotly(
      ggplot(
        df_reactive(),
        aes(Date, Active.Cases, color = Country.Region)
      ) +
        geom_point() + geom_line() + labs(y = "Active Cases"),
      height = 600
    )
  }
  else if (input$plotType == "Deaths") {
    ggplotly(ggplot(df_reactive(), aes(Date, Deaths, color = Country.Region)) +
               geom_point() + geom_line(),
             height = 600)
  }
  else if (input$plotType == "Recovered") {
    ggplotly(ggplot(df_reactive(), aes(Date, Recovered, color = Country.Region)) +
               geom_point() + geom_line(),
             height = 600)
  }
})

df_map_reactive <- reactive({
  df %>%
    group_by(Country.Region, Date, Lat, Long) %>%
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
  if (input$mapType == "Confirmed") {
    leaflet() %>%
      addTiles() %>%
      setView(2.2, 48, 2) %>%
      addCircles(
        data = df_map_reactive(),
        lat =  ~ Lat,
        lng =  ~ Long,
        weight = 1,
        radius =  ~ sqrt(Confirmed) * 5000,
        popup =  ~ paste(Country.Region, ":", Confirmed, " confirmed.")
      )
  }
  else if (input$mapType == "Deaths") {
    leaflet() %>%
      addTiles() %>%
      setView(2.2, 48, 2) %>%
      addCircles(
        data = df_map_reactive(),
        lat =  ~ Lat,
        lng =  ~ Long,
        weight = 1,
        radius =  ~ sqrt(Deaths) * 5000,
        popup =  ~ paste(Country.Region, ":", Deaths, " deaths.")
      )
  }
  else if (input$mapType == "Recovered") {
    leaflet() %>%
      addTiles() %>%
      setView(2.2, 48, 2) %>%
      addCircles(
        data = df_map_reactive(),
        lat =  ~ Lat,
        lng =  ~ Long,
        weight = 1,
        radius =  ~ sqrt(Recovered) * 5000,
        popup =  ~ paste(Country.Region, ":", Recovered, " recovered.")
      )
  }
  else if (input$mapType == "Active.Cases") {
    leaflet() %>%
      addTiles() %>%
      setView(2.2, 48, 2) %>%
      addCircles(
        data = df_map_reactive(),
        lat =  ~ Lat,
        lng =  ~ Long,
        weight = 1,
        radius =  ~ sqrt(Active.Cases) * 5000,
        popup =  ~ paste(Country.Region, ":", Active.Cases, " active cases.")
      )
  }
})

df_date <- df %>%
  group_by(Country.Region, Date, Lat, Long) %>%
  summarize(
    Confirmed = sum(Confirmed),
    Deaths = sum(Deaths),
    Recovered = sum(Recovered),
    Active.Cases = sum(Active.Cases)
  ) %>%
  filter(Date == max(df$Date))

leaflet() %>%
  addTiles() %>%
  setView(2.2, 48, 2) %>%
  addCircles(
    data = df_date,
    lat =  ~ Lat,
    lng =  ~ Long,
    weight = 1,
    radius =  ~ sqrt(Confirmed) * 5000,
    popup =  ~ Confirmed
  )