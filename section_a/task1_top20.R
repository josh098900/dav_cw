
library(plotly) #so i can make it interactive, the number of issues is viewable on hover


top_20_titles <- issues %>%
  #  filtering out the reminder note and the 'b' separators (as i read through the excel file i realised R would count the line breaks as issues which would ruin the data)

  filter(Issue_UID != "Reminder" & Issue != "b") %>%
  mutate(Pub_Year = as.numeric(Pub_Year)) %>%
  left_join(titles_overview, by = c("Issue_UID" = "U_SeriesID")) %>%
  filter(RunType == "Continuing") %>%

  # using str_trim to remove any invisible spaces at the edges of the names
  mutate(Title_Full = str_trim(Title_Full)) %>%

  group_by(Title_Full) %>%
  summarise(
    Total_Issues = n(),
    Start_Year = min(Pub_Year, na.rm = TRUE),
    End_Year = max(Pub_Year, na.rm = TRUE)
  ) %>%
  arrange(desc(Total_Issues)) %>%
  slice_max(order_by = Total_Issues, n = 20)

#interactive chart
top20_bar_chart <- plot_ly(
  data = top_20_titles,

  #  issues on the X axis, titles on the Y axis
  x = ~Total_Issues,

  # The reorder() function used to make the highest at the top
  y = ~reorder(Title_Full, Total_Issues),

  # bar chart, and 'h' means horizontal
  type = 'bar',
  orientation = 'h',

  text = ~paste("Active from:", Start_Year, "to", End_Year),
  hoverinfo = 'text+x',

  # colour of bars
  marker = list(color = 'pink')
) %>%

  # The layout section for clear labelling
  layout(
    title = "Top 20 Most Popular Continuing DC Titles",
    xaxis = list(title = "Total Number of Issues Published"),
    yaxis = list(title = "Titles")
  )

# calling object
#top20_bar_chart
