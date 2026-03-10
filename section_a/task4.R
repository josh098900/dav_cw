
# Task Most Prolific Cover Artists
#stuff i discovered through exploring the file
# Need to count BOTH CvrA_Artist (main 'A' covers) and CvrVarArtists (variant covers)
# CvrVarArtists format: "2-1_SA_1236; 1-2_SA_1239" prefix is print/variant info, SA_XXXX is the artist ID

# Gather A cover artist entries
a_covers <- issues %>%
  filter(Issue_UID != "Reminder" & Issue != "b") %>%
  filter(!is.na(CvrA_Artist) & CvrA_Artist != "NA" & CvrA_Artist != "b") %>%
  left_join(titles_overview, by = c("Issue_UID" = "U_SeriesID")) %>%
  mutate(Title_Full = str_trim(Title_Full)) %>%
  select(Artist_ID = CvrA_Artist, Title_Full)

# Gathered variant cover artist entries
# Split semicolon separated entries, then extract the ID from each
var_covers <- issues %>%
  filter(Issue_UID != "Reminder" & Issue != "b") %>%
  filter(!is.na(CvrVarArtists) & CvrVarArtists != "NA") %>%
  left_join(titles_overview, by = c("Issue_UID" = "U_SeriesID")) %>%
  mutate(Title_Full = str_trim(Title_Full)) %>%
  select(CvrVarArtists, Title_Full) %>%
  separate_rows(CvrVarArtists, sep = ";\\s*") %>%
  # extract the artist ID from the variant string
  mutate(Artist_ID = str_extract(CvrVarArtists, "SA_\\d+")) %>%
  filter(!is.na(Artist_ID)) %>%
  select(Artist_ID, Title_Full)

# Combine both sources into one dataset
all_covers <- bind_rows(a_covers, var_covers)

# Join artist names from the lookup table
all_covers <- all_covers %>%
  left_join(cover_artists %>% filter(!is.na(UID)), by = c("Artist_ID" = "UID")) %>%
  mutate(Artist_Name = str_trim(paste(
    if_else(is.na(Name_1st), "", Name_1st),
    if_else(is.na(Name_Last), "", Name_Last)
  ))) %>%
  filter(Artist_Name != "")

#Find the top 10 most prolific cover artists
top_10_artist_names <- all_covers %>%
  group_by(Artist_Name) %>%
  summarise(Total_Covers = n()) %>%
  arrange(desc(Total_Covers)) %>%
  slice_max(order_by = Total_Covers, n = 10) %>%
  pull(Artist_Name)

#Break down by artist and title for the treemap
top_artists_breakdown <- all_covers %>%
  filter(Artist_Name %in% top_10_artist_names) %>%
  group_by(Artist_Name, Title_Full) %>%
  summarise(Cover_Count = n(), .groups = "drop")

# Build the treemap (same structure as task i did in task2)

# Create 'Children' (The Comic Titles)
treemap_children <- top_artists_breakdown %>%
  mutate(
    id = paste(Artist_Name, Title_Full, sep = " - "),
    labels = Title_Full,
    parents = Artist_Name,
    values = Cover_Count
  ) %>%
  select(id, labels, parents, values)

# Create 'Parents' (The Artists)
treemap_parents <- top_artists_breakdown %>%
  group_by(Artist_Name) %>%
  summarise(values = sum(Cover_Count), .groups = "drop") %>%
  mutate(
    id = Artist_Name,
    labels = Artist_Name,
    parents = "Top 10 Cover Artists"
  ) %>%
  select(id, labels, parents, values)

# Create 'Root' (The Main Background Box)
treemap_root <- data.frame(
  id = "Top 10 Cover Artists",
  labels = "Top 10 Cover Artists",
  parents = "",
  values = sum(treemap_parents$values)
)

# Bind all into one dataset
treemap_data <- bind_rows(treemap_root, treemap_parents, treemap_children)

# Build interactive treemap
prolific_artists_treemap <- plot_ly(
  data = treemap_data,
  type = "treemap",
  ids = ~id,
  labels = ~labels,
  parents = ~parents,
  values = ~values,
  branchvalues = "total",
  textinfo = "label+value",
  hoverinfo = "label+value+percent parent"
) %>%
  layout(
    title = "Body of Work: Top 10 Most Prolific Cover Artists and their DC Titles",
    margin = list(t = 50, b = 25, l = 25, r = 25)
  )

# Call object
prolific_artists_treemap
