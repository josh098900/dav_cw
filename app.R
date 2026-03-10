library(shiny)
library(plotly)
library(tidyverse)
library(readxl)
issues <- read_excel("data/DAV_CW_CrackersDoMatter_DS.xlsx", sheet = "DC_Issues")
titles_overview <- read_excel("data/DAV_CW_CrackersDoMatter_DS.xlsx", sheet = "DC_TitlesOverview")
# Data Analytics & Visualisation Coursework 2025/2026

# Load data
source("section_a/task1_top20.R")
source("section_a/task2.R")
source("section_a/task3.R")
source("section_a/task4.R")
source("section_a/task5.R")
# UI
ui <- fluidPage(
  titlePanel("DC Comics — Data Analytics Dashboard"),
  tabsetPanel(
    tabPanel("A1 — Popular Titles",
             plotlyOutput("titles_chart", height = "800px")
    ),
    tabPanel("A2 — Prolific Writers",
             plotlyOutput("writers_chart", height = "800px")
    ),
    tabPanel("A3 — Dixon vs Wolfman",
             plotlyOutput("career_chart", height = "800px")
    ),
    tabPanel("A4 - Prolific Cover Artists",
             plotlyOutput("artists_chart", height = "800px")
             ),
    tabPanel("A5 - cover Artist Careers",
             plotlyOutput("career_artist_chart", height = "800px")
             )
  )
)

# --- Server ------------------------------------------------------------------
server <- function(input, output) {

  output$titles_chart <- renderPlotly({
    top20_bar_chart
  })

  output$writers_chart <- renderPlotly({prolific_writers_treemap
    })

  output$career_chart <- renderPlotly({
    career_comparison_chart
  })

  output$artists_chart <- renderPlotly({
    prolific_artists_treemap
  })

  output$career_artist_chart <- renderPlotly({
    career_chart
  })

}

# --- Run ---------------------------------------------------------------------
shinyApp(ui = ui, server = server)
