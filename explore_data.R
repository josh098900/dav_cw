library(tidyverse)
library(readxl)


sheet_names <- excel_sheets("data/DAV_CW_CrackersDoMatter_DS.xlsx")
print(sheet_names)

#loading sheets into their own dataframes
titles_overview <- read_excel("data/DAV_CW_CrackersDoMatter_DS.xlsx", sheet = "DC_TitlesOverview")
issues <- read_excel("data/DAV_CW_CrackersDoMatter_DS.xlsx", sheet = "DC_Issues")
cover_artists <- read_excel("data/DAV_CW_CrackersDoMatter_DS.xlsx", sheet = "Shared_CoverArtist")
lead_writers <- read_excel("data/DAV_CW_CrackersDoMatter_DS.xlsx", sheet = "Shared_LeadWriter")
story_arcs <- read_excel("data/DAV_CW_CrackersDoMatter_DS.xlsx", sheet = "DC_StoryArc")
characters <- read_excel("data/DAV_CW_CrackersDoMatter_DS.xlsx", sheet = "DC_Characters")
affiliations <- read_excel("data/DAV_CW_CrackersDoMatter_DS.xlsx", sheet = "DC_Affiliations")

glimpse(titles_overview)
glimpse(issues)
