# Career Data for Chuck and Marv

career_data <- writer_data %>%
  #filter out list for two authors
  filter(Writer_Name %in% c("Chuck Dixon", "Marv Wolfman")) %>%

  #Drop any blank years to keep the timeline perfectly neat
  filter(!is.na(Pub_Year)) %>%

  #Group by the Year and the Writer
  group_by(Pub_Year, Writer_Name) %>%

  # Counting issues,using n_distinct() to count the unique titles
  summarise(
    Total_Issues = n(),
    Total_Titles = n_distinct(Title_Full),
    .groups = "drop"
  ) %>%

  #  i am displaying 0 for any Na's
  complete(Pub_Year, Writer_Name, fill = list(Total_Issues = 0, Total_Titles = 0))


#creating a line chart with plotly
# Issues Line Chart
issues_chart <- plot_ly(data = career_data, x = ~Pub_Year) %>%
  add_lines(
    y = ~Total_Issues,
    color = ~Writer_Name,
    colors = c("steelblue", "darkorange"), # Beautiful, distinct colors
    text = ~paste(Writer_Name, "<br>Year:", Pub_Year, "<br>Issues:", Total_Issues),
    hoverinfo = "text",
    legendgroup = ~Writer_Name # This links the legend so clicking an author toggles both charts!
  ) %>%
  layout(yaxis = list(title = "Total Issues"))

#  Titles Line Chart
titles_chart <- plot_ly(data = career_data, x = ~Pub_Year) %>%
  add_lines(
    y = ~Total_Titles,
    color = ~Writer_Name,
    colors = c("steelblue", "darkorange"),
    text = ~paste(Writer_Name, "<br>Year:", Pub_Year, "<br>Titles:", Total_Titles),
    hoverinfo = "text",
    legendgroup = ~Writer_Name,
    showlegend = FALSE #  hid this legend so it doesnt have duplicate names on the side
  ) %>%
  layout(yaxis = list(title = "Total Titles"))

# Stack them into one single visualisation
career_comparison_chart <- subplot(
  issues_chart, titles_chart,
  nrows = 2,           # Stack them in 2 rows
  shareX = TRUE,       # share the same timeline
  titleY = TRUE        # Keep Y-axis labels
) %>%
  layout(
    title = "Career Comparison: Chuck Dixon vs. Marv Wolfman",
    xaxis = list(title = "Publication Year", type = "linear"),
    xaxis = list(title = "Publication Year"),
    hovermode = "x unified" # a line displayed across both
  )

# Call chart
career_comparison_chart
