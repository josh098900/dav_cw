
# DATA NOTE — David Reed (David Vern Reed) (i will add this to section c)
# During exploration, i discovered that the lead_writers lookup table contains
# TWO entries for this writer:
# SW_2205 — correctly recorded with 9 issues attributed
# NA UID  — duplicate entry with no UID, notes read "See David Vern [Reed]"
# and "Uses both Vern and Vern-Reed"
# The NA UID entry caused all issues with no writer recorded (Write_Lead = NA)
# to incorrectly join to this row,inflating his count to 64
# and placing him falsely in the top 10.
# Fix applied: filter(!is.na(Write_Lead)) before the join removes all
# unattributed issues, leaving his correct count of 9 under SW_2205.







writer_data <- issues %>%
  filter(Issue_UID != "Reminder" & Issue != "b") %>%
  filter(!is.na(Write_Lead)) %>%


  #split any IDs separated by a semicolon and a space
  separate_rows(Write_Lead, sep = "; ") %>%

  # join the writer names
  left_join(lead_writers, by = c("Write_Lead" = "UID")) %>%
  left_join(titles_overview, by = c("Issue_UID" = "U_SeriesID")) %>%
  mutate(Writer_Name = paste(Name_1st, Name_Last)) %>%
  mutate(Title_Full = str_trim(Title_Full))

top_10_names <- writer_data %>%
  #added the filter after i discovered the name at index 4 was Na
  filter(Writer_Name != "NA NA") %>%
  group_by(Writer_Name) %>%
  summarise(Total_Issues = n()) %>%
  arrange(desc(Total_Issues)) %>%
  slice_max(order_by = Total_Issues, n = 10) %>%
  pull(Writer_Name) #pull (this pulls the names into a list)

top_writers_breakdown <- writer_data %>%
  filter(Writer_Name %in% top_10_names) %>%

  group_by(Writer_Name, Title_Full) %>%
  summarise(Issue_Count = n(), .groups = "drop")



#using a tree map, i used a barchart initially but discovered the data was not really readable


# Create 'Children' (The Comic Titles)
treemap_children <- top_writers_breakdown %>%
  mutate(
    id = paste(Writer_Name, Title_Full, sep = " - "), # unique name for every box
    labels = Title_Full,
    parents = Writer_Name,  # Who the comic belongs to
    values = Issue_Count    # How big the box should be
  ) %>%
  select(id, labels, parents, values)

# Create 'Parents' (The Writers)
treemap_parents <- top_writers_breakdown %>%
  group_by(Writer_Name) %>%
  summarise(values = sum(Issue_Count), .groups = "drop") %>%
  mutate(
    id = Writer_Name,
    labels = Writer_Name,
    parents = "Top 10 Writers"
  ) %>%
  select(id, labels, parents, values)

# Create 'Root' (The Main Background Box)
treemap_root <- data.frame(
  id = "Top 10 Writers",
  labels = "Top 10 Writers",
  parents = "", # The root has no parent so left blank
  values = sum(treemap_parents$values)
)

# bind all into one dataset
treemap_data <- bind_rows(treemap_root, treemap_parents, treemap_children)


# implement it

prolific_writers_treemap <- plot_ly(
  data = treemap_data,
  type = "treemap",
  ids = ~id,
  labels = ~labels,
  parents = ~parents,
  values = ~values,
  branchvalues = "total",            # size of boxes
  textinfo = "label+value",          # the name and the issue count displayed
  hoverinfo = "label+value+percent parent" # hover effect showing percentages
) %>%
  layout(
    title = "Body of Work: Top 10 Most Prolific Writers and their DC Titles",
    margin = list(t = 50, b = 25, l = 25, r = 25)
  )

# Call object
prolific_writers_treemap

