
# Career Paths of Cover Artists, George Perez, Tony Daniel, Lee Bermejo
# Cover type shown in hover via ggplotly()

library(ggplot2)
library(tidyverse)
library(dplyr)
library(plotly)

# The three artists we're tracking
target_artists <- c("George Perez", "Tony Salvador Daniel", "Lee Bermejo")

#gather A cover entries
a_cover_data <- issues %>%
  filter(Issue_UID != "Reminder" & Issue != "b") %>%
  filter(!is.na(CvrA_Artist) & CvrA_Artist != "NA" & CvrA_Artist != "b") %>%
  left_join(cover_artists %>% filter(!is.na(UID)), by = c("CvrA_Artist" = "UID")) %>%
  mutate(Artist_Name = str_trim(paste(
    if_else(is.na(Name_1st), "", Name_1st),
    if_else(is.na(Name_Last), "", Name_Last)
  ))) %>%
  filter(Artist_Name %in% target_artists) %>%
  left_join(titles_overview, by = c("Issue_UID" = "U_SeriesID")) %>%
  mutate(
    Pub_Year = as.numeric(Pub_Year),
    Title_Full = str_trim(Title_Full),
    Cover_Type = "A Cover"
  ) %>%
  filter(!is.na(Pub_Year)) %>%
  select(Artist_Name, Title_Full, Pub_Year, Cover_Type)

# Gather variant cover entries
var_cover_data <- issues %>%
  filter(Issue_UID != "Reminder" & Issue != "b") %>%
  filter(!is.na(CvrVarArtists) & CvrVarArtists != "NA") %>%
  left_join(titles_overview, by = c("Issue_UID" = "U_SeriesID")) %>%
  mutate(
    Pub_Year = as.numeric(Pub_Year),
    Title_Full = str_trim(Title_Full)
  ) %>%
  filter(!is.na(Pub_Year)) %>%
  select(CvrVarArtists, Title_Full, Pub_Year) %>%
  separate_rows(CvrVarArtists, sep = ";\\s*") %>%
  mutate(
    Artist_ID = str_extract(CvrVarArtists, "SA_\\d+"),
    Prefix = str_extract(CvrVarArtists, "^\\d+-\\d+")
  ) %>%
  filter(!is.na(Artist_ID)) %>%
  left_join(cover_artists %>% filter(!is.na(UID)), by = c("Artist_ID" = "UID")) %>%
  mutate(Artist_Name = str_trim(paste(
    if_else(is.na(Name_1st), "", Name_1st),
    if_else(is.na(Name_Last), "", Name_Last)
  ))) %>%
  filter(Artist_Name %in% target_artists) %>%
  mutate(Cover_Type = if_else(
    str_starts(Prefix, "1"), "1st Print Variant", "2nd+ Print"
  )) %>%
  select(Artist_Name, Title_Full, Pub_Year, Cover_Type)

#  Combine
all_career_covers <- bind_rows(a_cover_data, var_cover_data)


# Summarise totals per artist per year
career_totals <- all_career_covers %>%
  group_by(Pub_Year, Artist_Name) %>%
  summarise(
    Total_Covers = n(),
    Total_Titles = n_distinct(Title_Full),
    .groups = "drop"
  )

# Also get cover type counts per artist per year for the tooltip
cover_type_counts <- all_career_covers %>%
  group_by(Pub_Year, Artist_Name, Cover_Type) %>%
  summarise(Count = n(), .groups = "drop") %>%
  pivot_wider(names_from = Cover_Type, values_from = Count, values_fill = 0)

# Join the cover type breakdown into the totals for tooltip
career_totals <- career_totals %>%
  left_join(cover_type_counts, by = c("Pub_Year", "Artist_Name")) %>%
  mutate(
    `A Cover` = replace_na(`A Cover`, 0),
    `1st Print Variant` = replace_na(`1st Print Variant`, 0),
    `2nd+ Print` = replace_na(`2nd+ Print`, 0)
  )

# Build tooltip with cover type percentages
career_totals <- career_totals %>%
  mutate(tooltip = paste0(
    Artist_Name,
    "\nYear: ", Pub_Year,
    "\nTotal Covers: ", Total_Covers,
    "\nTitles: ", Total_Titles,
    "\n---",
    "\nA Covers: ", `A Cover`, " (", round(`A Cover` / Total_Covers * 100), "%)",
    "\n1st Print Variants: ", `1st Print Variant`, " (", round(`1st Print Variant` / Total_Covers * 100), "%)",
    "\n2nd+ Prints: ", `2nd+ Print`, " (", round(`2nd+ Print` / Total_Covers * 100), "%)"
  ))

# Fill missing years so all artists share the full timeline
all_years <- seq(min(career_totals$Pub_Year), max(career_totals$Pub_Year))
career_totals <- expand.grid(
  Pub_Year = all_years,
  Artist_Name = target_artists,
  stringsAsFactors = FALSE
) %>%
  left_join(career_totals, by = c("Pub_Year", "Artist_Name")) %>%
  mutate(Total_Covers = replace_na(Total_Covers, 0))

# Build the grouped bar chart
p <- ggplot(career_totals %>% filter(Total_Covers > 0),
            aes(x = Pub_Year, y = Total_Covers, fill = Artist_Name, text = tooltip)) +
  geom_col(position = position_dodge(preserve = "single"), width = 0.8) +
  scale_fill_manual(values = c(
    "George Perez" = "steelblue",
    "Tony Salvador Daniel" = "darkorange",
    "Lee Bermejo" = "firebrick"
  )) +
  labs(
    title = "Career Paths: George Perez, Tony Daniel & Lee Bermejo",
    x = "Publication Year",
    y = "Number of Covers",
    fill = "Artist"
  ) +
  theme_minimal()

#  ggplotly, hover shows the full cover type breakdown
career_chart <- ggplotly(p, tooltip = "text")

# Call
#career_chart
