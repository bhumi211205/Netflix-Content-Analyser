
# ============================================================
# NETFLIX CONTENT ANALYSER
# R + Shiny + Big Data Analysis
# ============================================================

# ------------------------------------------------------------
# REQUIRED PACKAGES
# ------------------------------------------------------------

library(shiny)
library(shinydashboard)
library(dplyr)
library(tidyr)
library(stringr)
library(plotly)
library(lubridate)
library(scales)

# ------------------------------------------------------------
# DATA FILES
# ------------------------------------------------------------

movies_file <- "netflix_movies_detailed_up_to_2025.csv"
tv_file     <- "netflix_tv_shows_detailed_up_to_2025.csv"

if (!file.exists(movies_file)) {
  stop(
    paste0(
      "Movie dataset not found: ", movies_file,
      "\nMake sure it is in the same folder as app.R."
    )
  )
}

if (!file.exists(tv_file)) {
  stop(
    paste0(
      "TV dataset not found: ", tv_file,
      "\nMake sure it is in the same folder as app.R."
    )
  )
}

# ------------------------------------------------------------
# LOAD DATA
# ------------------------------------------------------------

movies <- read.csv(
  movies_file,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

tv <- read.csv(
  tv_file,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

movies$type <- "Movie"
tv$type <- "TV Show"

# Add columns that are missing from TV dataset
if (!"budget" %in% names(tv)) {
  tv$budget <- NA_real_
}

if (!"revenue" %in% names(tv)) {
  tv$revenue <- NA_real_
}

# Add columns that are missing from movie dataset
if (!"budget" %in% names(movies)) {
  movies$budget <- NA_real_
}

if (!"revenue" %in% names(movies)) {
  movies$revenue <- NA_real_
}

# ------------------------------------------------------------
# COMBINE DATASETS
# ------------------------------------------------------------

common_columns <- union(
  names(movies),
  names(tv)
)

for (col in common_columns) {

  if (!col %in% names(movies)) {
    movies[[col]] <- NA
  }

  if (!col %in% names(tv)) {
    tv[[col]] <- NA
  }
}

netflix <- bind_rows(
  movies[, common_columns],
  tv[, common_columns]
)

# ------------------------------------------------------------
# DATA CLEANING
# ------------------------------------------------------------

character_columns <- c(
  "title",
  "director",
  "cast",
  "country",
  "genres",
  "language",
  "rating",
  "duration"
)

for (col in character_columns) {

  if (col %in% names(netflix)) {

    netflix[[col]] <- as.character(
      netflix[[col]]
    )

    netflix[[col]] <- str_trim(
      netflix[[col]]
    )

    netflix[[col]][
      netflix[[col]] == ""
    ] <- NA
  }
}

numeric_columns <- c(
  "release_year",
  "popularity",
  "vote_count",
  "vote_average",
  "budget",
  "revenue"
)

for (col in numeric_columns) {

  if (col %in% names(netflix)) {

    netflix[[col]] <- suppressWarnings(
      as.numeric(netflix[[col]])
    )
  }
}

# Date conversion
netflix$date_added <- suppressWarnings(
  as.Date(netflix$date_added)
)

# ------------------------------------------------------------
# REMOVE DUPLICATES
# ------------------------------------------------------------

if ("show_id" %in% names(netflix)) {

  netflix <- netflix %>%
    filter(!is.na(show_id)) %>%
    distinct(show_id, .keep_all = TRUE)

} else {

  netflix <- netflix %>%
    distinct(title, type, .keep_all = TRUE)
}

# ------------------------------------------------------------
# COLORS
# ------------------------------------------------------------

NETFLIX_RED <- "#E50914"
BLACK <- "#000000"
DARK <- "#111111"
DARKER <- "#181818"
WHITE <- "#FFFFFF"
GRID <- "#333333"

chart_colors <- c(
  "#E50914",
  "#00A8E8",
  "#FFD700",
  "#9B59B6",
  "#00C9A7",
  "#FF7F50",
  "#F39C12",
  "#2ECC71",
  "#E67E22",
  "#3498DB",
  "#E84393",
  "#6C5CE7",
  "#1ABC9C",
  "#FD79A8",
  "#FDCB6E"
)

# ------------------------------------------------------------
# COMMON PLOTLY THEME
# ------------------------------------------------------------

dark_layout <- function(
    p,
    x_title = NULL,
    y_title = NULL,
    x_log = FALSE,
    y_log = FALSE,
    show_legend = TRUE
) {

  x_axis <- list(
    color = WHITE,
    gridcolor = GRID,
    zerolinecolor = GRID,
    linecolor = GRID,
    title = x_title,
    automargin = TRUE
  )

  y_axis <- list(
    color = WHITE,
    gridcolor = GRID,
    zerolinecolor = GRID,
    linecolor = GRID,
    title = y_title,
    automargin = TRUE
  )

  if (x_log) {
    x_axis$type <- "log"
    x_axis$dtick <- 1
  }

  if (y_log) {
    y_axis$type <- "log"
    y_axis$dtick <- 1
  }

  p %>%
    layout(
      paper_bgcolor = BLACK,
      plot_bgcolor = BLACK,

      font = list(
        color = WHITE,
        family = "Arial"
      ),

      xaxis = x_axis,
      yaxis = y_axis,

      legend = list(
        font = list(
          color = WHITE
        ),
        bgcolor = BLACK
      ),

      margin = list(
        l = 70,
        r = 30,
        t = 50,
        b = 80
      ),

      hoverlabel = list(
        bgcolor = "#222222",
        font = list(
          color = WHITE
        )
      ),

      showlegend = show_legend
    )
}

# ------------------------------------------------------------
# DATA PREPARATION
# ------------------------------------------------------------

genre_data <- netflix %>%
  filter(!is.na(genres)) %>%
  separate_rows(
    genres,
    sep = ",|\\|"
  ) %>%
  mutate(
    genres = str_trim(genres)
  ) %>%
  filter(
    genres != "",
    !is.na(genres)
  )

country_data <- netflix %>%
  filter(!is.na(country)) %>%
  separate_rows(
    country,
    sep = ",|\\|"
  ) %>%
  mutate(
    country = str_trim(country)
  ) %>%
  filter(
    country != "",
    !is.na(country)
  )

# ------------------------------------------------------------
# SUMMARY VALUES
# ------------------------------------------------------------

movie_count <- sum(
  netflix$type == "Movie",
  na.rm = TRUE
)

tv_count <- sum(
  netflix$type == "TV Show",
  na.rm = TRUE
)

total_content <- nrow(netflix)

country_count <- length(
  unique(
    country_data$country
  )
)

genre_count <- length(
  unique(
    genre_data$genres
  )
)

director_count <- length(
  unique(
    netflix$director[
      !is.na(netflix$director)
    ]
  )
)

actor_count <- length(
  unique(
    netflix$cast[
      !is.na(netflix$cast)
    ]
  )
)

# ------------------------------------------------------------
# CSS
# ------------------------------------------------------------

custom_css <- "
body {
  background-color: #000000 !important;
  color: white !important;
}

.content-wrapper {
  background-color: #000000 !important;
}

.right-side {
  background-color: #000000 !important;
}

.main-header .logo {
  background-color: #000000 !important;
  color: #E50914 !important;
  font-weight: bold;
  font-size: 22px;
}

.main-header .navbar {
  background-color: #000000 !important;
}

.main-sidebar {
  background-color: #050505 !important;
}

.sidebar-menu > li > a {
  color: #DDDDDD !important;
}

.sidebar-menu > li.active > a,
.sidebar-menu > li:hover > a {
  color: white !important;
  background-color: #E50914 !important;
}

.content-header h1 {
  color: white !important;
}

.box {
  background-color: #111111 !important;
  border: 1px solid #222222 !important;
  color: white !important;
}

.box-header {
  color: white !important;
  background-color: #111111 !important;
}

.box-title {
  color: white !important;
  font-weight: bold !important;
}

.box-body {
  background-color: #111111 !important;
  color: white !important;
}

.small-box {
  border-radius: 12px !important;
  overflow: hidden;
  border: none !important;
  background-color: #181818 !important;
}

.small-box h3,
.small-box p {
  color: white !important;
}

.small-box .icon {
  color: rgba(255,255,255,0.15) !important;
}

.netflix-title {
  color: #E50914;
  font-size: 40px;
  font-weight: bold;
  margin-bottom: 5px;
}

.netflix-subtitle {
  color: #CCCCCC;
  font-size: 16px;
  margin-bottom: 25px;
}

.section-title {
  color: white;
  font-size: 24px;
  font-weight: bold;
  margin-top: 25px;
  margin-bottom: 15px;
}

.hero {
  background: #000000;
  padding: 35px;
  border-radius: 12px;
  margin-bottom: 25px;
  border: 1px solid #222222;
}

.hero h1 {
  color: #E50914;
  font-size: 42px;
  font-weight: bold;
}

.hero p {
  color: #DDDDDD;
  font-size: 17px;
}

.stat-card {
  background: #111111;
  border: 1px solid #222222;
  border-radius: 12px;
  padding: 22px;
  text-align: center;
  margin-bottom: 20px;
  min-height: 125px;
}

.stat-number {
  font-size: 32px;
  font-weight: bold;
  color: #E50914;
}

.stat-label {
  color: #CCCCCC;
  font-size: 14px;
  margin-top: 8px;
}

.dataTables_wrapper {
  color: white !important;
}

table.dataTable {
  color: white !important;
  background-color: #111111 !important;
}

table {
  color: white !important;
}

td, th {
  color: white !important;
  background-color: #111111 !important;
}

select,
input {
  background-color: #222222 !important;
  color: white !important;
  border: 1px solid #444444 !important;
}

label {
  color: white !important;
}

.plotly {
  background-color: #000000 !important;
}

.js-plotly-plot {
  background-color: #000000 !important;
}

.plot-container {
  background-color: #000000 !important;
}

.modebar {
  background-color: transparent !important;
}
"

# ------------------------------------------------------------
# USER INTERFACE
# ------------------------------------------------------------

ui <- dashboardPage(

  dashboardHeader(
    title = "NETFLIX CONTENT ANALYSER"
  ),

  dashboardSidebar(

    sidebarMenu(

      menuItem(
        "Home",
        tabName = "home",
        icon = icon("home")
      ),

      menuItem(
        "Movies",
        tabName = "movies",
        icon = icon("film")
      ),

      menuItem(
        "TV Shows",
        tabName = "tvshows",
        icon = icon("tv")
      ),

      menuItem(
        "Genres",
        tabName = "genres",
        icon = icon("tags")
      ),

      menuItem(
        "Countries",
        tabName = "countries",
        icon = icon("globe")
      ),

      menuItem(
        "Ratings",
        tabName = "ratings",
        icon = icon("star")
      ),

      menuItem(
        "People",
        tabName = "people",
        icon = icon("users")
      ),

      menuItem(
        "Popularity",
        tabName = "popularity",
        icon = icon("fire")
      ),

      menuItem(
        "Financial Analysis",
        tabName = "financial",
        icon = icon("dollar-sign")
      ),

      menuItem(
        "BDA Analysis",
        tabName = "bda",
        icon = icon("bar-chart")
      )
    )
  ),

  dashboardBody(

    tags$head(
      tags$style(
        HTML(custom_css)
      )
    ),

    tabItems(

      # ======================================================
      # HOME
      # ======================================================

      tabItem(
        tabName = "home",

        div(
          class = "hero",

          h1(
            "Netflix Content Analyser"
          ),

          p(
            "Explore movies and TV shows using data analysis, "
            ,"statistical analysis and interactive visualisation."
          )
        ),

        h3(
          class = "section-title",
          "Netflix Content Overview"
        ),

        fluidRow(

          column(
            3,
            div(
              class = "stat-card",
              div(
                class = "stat-number",
                format(
                  movie_count,
                  big.mark = ","
                )
              ),
              div(
                class = "stat-label",
                "NO. OF MOVIES"
              )
            )
          ),

          column(
            3,
            div(
              class = "stat-card",
              div(
                class = "stat-number",
                format(
                  tv_count,
                  big.mark = ","
                )
              ),
              div(
                class = "stat-label",
                "NO. OF TV SHOWS"
              )
            )
          ),

          column(
            3,
            div(
              class = "stat-card",
              div(
                class = "stat-number",
                format(
                  total_content,
                  big.mark = ","
                )
              ),
              div(
                class = "stat-label",
                "TOTAL CONTENT"
              )
            )
          ),

          column(
            3,
            div(
              class = "stat-card",
              div(
                class = "stat-number",
                format(
                  country_count,
                  big.mark = ","
                )
              ),
              div(
                class = "stat-label",
                "COUNTRIES"
              )
            )
          )
        ),

        fluidRow(

          column(
            4,
            div(
              class = "stat-card",
              div(
                class = "stat-number",
                format(
                  genre_count,
                  big.mark = ","
                )
              ),
              div(
                class = "stat-label",
                "GENRES"
              )
            )
          ),

          column(
            4,
            div(
              class = "stat-card",
              div(
                class = "stat-number",
                format(
                  director_count,
                  big.mark = ","
                )
              ),
              div(
                class = "stat-label",
                "DIRECTORS"
              )
            )
          ),

          column(
            4,
            div(
              class = "stat-card",
              div(
                class = "stat-number",
                format(
                  actor_count,
                  big.mark = ","
                )
              ),
              div(
                class = "stat-label",
                "ACTORS"
              )
            )
          )
        ),

        h3(
          class = "section-title",
          "Content Distribution"
        ),

        fluidRow(

          column(
            6,
            box(
              width = 12,
              title = "Movies vs TV Shows",
              plotlyOutput(
                "home_type_chart",
                height = "430px"
              )
            )
          ),
        ),

        h3(
          class = "section-title",
          "Explore the Tabs to Analyze Movies, TV Shows, Genres, Countries, Ratings, People, Popularity and Financials"
        )   
      ),

      # ======================================================
      # MOVIES
      # ======================================================

      tabItem(
        tabName = "movies",

        h2("Movies"),

        fluidRow(
          column(
            6,
            box(
              width = 12,
              title = "Movie Language Distribution",
              plotlyOutput("movie_language", height = "450px")
            )
          ),
          column(
            6,
            box(
              width = 12,
              title = "Highest-Popularity Movies",
              tableOutput("top_movies")
            )
          )
        )
      ),

      # TV SHOWS
      # ======================================================

      tabItem(
        tabName = "tvshows",

        h2("TV Shows"),

        fluidRow(
          column(
            6,
            box(
              width = 12,
              title = "TV Show Language Distribution",
              plotlyOutput("tv_language", height = "450px")
            )
          ),
          column(
            6,
            box(
              width = 12,
              title = "Highest-Popularity TV Shows",
              tableOutput("top_tv")
            )
          )
        )
      ),

      # GENRES
      # ======================================================

      tabItem(
        tabName = "genres",

        h2("Genre Analysis"),

        fluidRow(

          column(
            6,
            box(
              width = 12,
              title = "Most Common Genres",
              plotlyOutput(
                "genres_all",
                height = "500px"
              )
            )
          ),

          column(
            6,
            box(
              width = 12,
              title = "Genres by Content Type",
              plotlyOutput(
                "genres_type",
                height = "500px"
              )
            )
          )
        )
      ),

      # ======================================================
      # COUNTRIES
      # ======================================================

      tabItem(
        tabName = "countries",

        h2("Country Analysis"),

        fluidRow(

          column(
            6,
            box(
              width = 12,
              title = "Countries Producing the Most Content",
              plotlyOutput(
                "countries_chart",
                height = "550px"
              )
            )
          ),

          column(
            6,
            box(
              width = 12,
              title = "Movies vs TV Shows by Country",
              plotlyOutput(
                "countries_type",
                height = "550px"
              )
            )
          )
        )
      ),

      # ======================================================
      # RATINGS
      # ======================================================

      tabItem(
        tabName = "ratings",

        h2("Audience Rating Analysis"),

        fluidRow(

          column(
            6,
            box(
              width = 12,
              title = "Most Common Audience Ratings",
              plotlyOutput(
                "ratings_chart",
                height = "500px"
              )
            )
          ),

          column(
            6,
            box(
              width = 12,
              title = "Audience Ratings by Content Type",
              plotlyOutput(
                "ratings_type",
                height = "500px"
              )
            )
          )
        ),

        fluidRow(

          column(
            12,
            box(
              width = 12,
              title = "Highest-Rated Titles with Meaningful Vote Counts",
              tableOutput(
                "top_rated"
              )
            )
          )
        )
      ),

      # ======================================================
      # PEOPLE
      # ======================================================

      tabItem(
        tabName = "people",

        h2("People Analysis"),

        fluidRow(

          column(
            6,
            box(
              width = 12,
              title = "Directors with Most Titles",
              plotlyOutput(
                "directors",
                height = "500px"
              )
            )
          ),

          column(
            6,
            box(
              width = 12,
              title = "Actors Appearing Most Frequently",
              plotlyOutput(
                "actors",
                height = "500px"
              )
            )
          )
        )
      ),

      # ======================================================
      # POPULARITY
      # ======================================================

      tabItem(
        tabName = "popularity",

        h2("Popularity Analysis"),

        fluidRow(

          column(
            6,
            box(
              width = 12,
              title = "Most Popular Titles",
              tableOutput(
                "popular_titles"
              )
            )
          ),

          column(
            6,
            box(
              width = 12,
              title = "Popularity by Content Type",
              plotlyOutput(
                "popularity_type",
                height = "450px"
              )
            )
          )
        ),

        fluidRow(

          column(
            12,
            box(
              width = 12,
              title = "Relationship Between Vote Count and Popularity",
              plotlyOutput(
                "votes_popularity",
                height = "550px"
              )
            )
          )
        )
      ),

      # ======================================================
      # FINANCIAL ANALYSIS
      # ======================================================

      tabItem(
        tabName = "financial",

        h2("Movie Financial Analysis"),

        fluidRow(

          column(
            6,
            box(
              width = 12,
              title = "Highest-Budget Movies",
              tableOutput(
                "budget_table"
              )
            )
          ),

          column(
            6,
            box(
              width = 12,
              title = "Highest-Revenue Movies",
              tableOutput(
                "revenue_table"
              )
            )
          )
        ),

        fluidRow(

          column(
            12,
            box(
              width = 12,
              title = "Relationship Between Movie Budget and Revenue",
              plotlyOutput(
                "budget_revenue",
                height = "550px"
              )
            )
          )
        )
      ),

      # ======================================================
      # BDA ANALYSIS
      # ======================================================

      tabItem(
        tabName = "bda",

        h2("Big Data Analysis"),

        fluidRow(
          column(
            6,
            box(
              width = 12,
              title = "Average Revenue by Genre",
              plotlyOutput("bda_genre_revenue", height = "450px")
            )
          ),
          column(
            6,
            box(
              width = 12,
              title = "Average Audience Rating by Genre",
              plotlyOutput("bda_genre_rating", height = "450px")
            )
          )
        ),

        fluidRow(
          column(
            6,
            box(
              width = 12,
              title = "Top Revenue-Efficient Movies",
              tableOutput("bda_efficiency")
            )
          ),
          column(
            6,
            box(
              width = 12,
              title = "High-Engagement Titles",
              plotlyOutput("bda_engagement", height = "450px")
            )
          )
        )
      )
      )
    )
  )

# ============================================================
# SERVER
# ============================================================

server <- function(
    input,
    output,
    session
) {

  # ----------------------------------------------------------
  # HOME: MOVIES VS TV SHOWS
  # ----------------------------------------------------------

  output$home_type_chart <- renderPlotly({

    data <- data.frame(
      type = c(
        "Movies",
        "TV Shows"
      ),
      count = c(
        movie_count,
        tv_count
      )
    )

    plot_ly(
      data = data,
      labels = ~type,
      values = ~count,
      type = "pie",
      hole = 0.48,
      textinfo = "label+percent",
      textfont = list(
        color = WHITE,
        size = 15
      ),
      marker = list(
        colors = c(
          NETFLIX_RED,
          "#00A8E8"
        ),
        line = list(
          color = BLACK,
          width = 3
        )
      ),
      hovertemplate =
        paste0(
          "<b>%{label}</b>",
          "<br>Titles: %{value:,}",
          "<br>Share: %{percent}",
          "<extra></extra>"
        )
    ) %>%

      layout(
        paper_bgcolor = BLACK,
        plot_bgcolor = BLACK,

        font = list(
          color = WHITE
        ),

        legend = list(
          bgcolor = BLACK,
          font = list(
            color = WHITE
          )
        ),

        margin = list(
          l = 20,
          r = 20,
          t = 20,
          b = 20
        )
      )
  })

  # ----------------------------------------------------------
  # HOME: TOP GENRES
  # ----------------------------------------------------------

  # ----------------------------------------------------------
  # MOVIES: GENRES
  # ----------------------------------------------------------

  output$movie_language <- renderPlotly({
    data <- netflix %>%
      filter(type == "Movie", !is.na(language), language != "") %>%
      separate_rows(language, sep = ",|\\|") %>%
      mutate(language = str_trim(language)) %>%
      filter(language != "") %>%
      count(language, name = "titles") %>%
      arrange(desc(titles)) %>%
      slice_head(n = 10)

    plot_ly(data = data, x = ~titles, y = ~reorder(language, titles),
            type = "bar", orientation = "h",
            marker = list(color = rep(chart_colors, length.out = nrow(data))),
            hovertemplate = paste0("<b>%{y}</b><br>Movies: %{x:,}<extra></extra>")) %>%
      dark_layout(x_title = "Number of Movies", y_title = "", show_legend = FALSE)
  })

  output$top_movies <- renderTable({

    netflix %>%
      filter(
        type == "Movie",
        !is.na(popularity)
      ) %>%
      arrange(
        desc(popularity)
      ) %>%
      transmute(
        Title = title,
        `Release Year` = as.integer(release_year),
        `Audience Rating` =
          round(vote_average, 2),
        Popularity =
          round(popularity, 2)
      ) %>%
      slice_head(n = 15)
  })

  # ----------------------------------------------------------
  # TV SHOWS: GENRES
  # ----------------------------------------------------------

  output$tv_language <- renderPlotly({
    data <- netflix %>%
      filter(type == "TV Show", !is.na(language), language != "") %>%
      separate_rows(language, sep = ",|\\|") %>%
      mutate(language = str_trim(language)) %>%
      filter(language != "") %>%
      count(language, name = "titles") %>%
      arrange(desc(titles)) %>%
      slice_head(n = 10)

    plot_ly(data = data, x = ~titles, y = ~reorder(language, titles),
            type = "bar", orientation = "h",
            marker = list(color = rep(chart_colors, length.out = nrow(data))),
            hovertemplate = paste0("<b>%{y}</b><br>TV Shows: %{x:,}<extra></extra>")) %>%
      dark_layout(x_title = "Number of TV Shows", y_title = "", show_legend = FALSE)
  })

  output$top_tv <- renderTable({

    netflix %>%
      filter(
        type == "TV Show",
        !is.na(popularity)
      ) %>%
      arrange(
        desc(popularity)
      ) %>%
      transmute(
        Title = title,
        `Release Year` = as.integer(release_year),
        `Audience Rating` =
          round(vote_average, 2),
        Popularity =
          round(popularity, 2)
      ) %>%
      slice_head(n = 15)
  })

  # ----------------------------------------------------------
  # ALL GENRES
  # ----------------------------------------------------------

  output$genres_all <- renderPlotly({

    data <- genre_data %>%
      count(
        genres,
        name = "titles"
      ) %>%
      arrange(
        desc(titles)
      ) %>%
      slice_head(n = 15)

    plot_ly(
      data = data,
      x = ~titles,
      y = ~reorder(genres, titles),
      type = "bar",
      orientation = "h",
      marker = list(
        color = rep(
          chart_colors,
          length.out = nrow(data)
        )
      ),
      hovertemplate =
        paste0(
          "<b>%{y}</b>",
          "<br>Titles: %{x:,}",
          "<extra></extra>"
        )
    ) %>%

      dark_layout(
        x_title = "Number of Titles",
        y_title = "",
        show_legend = FALSE
      )
  })

  # ----------------------------------------------------------
  # GENRES BY TYPE
  # ----------------------------------------------------------

  output$genres_type <- renderPlotly({

    top_genres <- genre_data %>%
      count(
        genres,
        name = "total"
      ) %>%
      arrange(
        desc(total)
      ) %>%
      slice_head(n = 10) %>%
      pull(genres)

    data <- genre_data %>%
      filter(
        genres %in% top_genres
      ) %>%
      count(
        genres,
        type,
        name = "titles"
      )

    plot_ly(
      data = data,
      x = ~titles,
      y = ~reorder(genres, titles),
      color = ~type,
      type = "bar",
      orientation = "h",
      colors = c(
        "Movie" = NETFLIX_RED,
        "TV Show" = "#00A8E8"
      ),
      hovertemplate =
        paste0(
          "<b>%{y}</b>",
          "<br>Type: %{fullData.name}",
          "<br>Titles: %{x:,}",
          "<extra></extra>"
        )
    ) %>%

      dark_layout(
        x_title = "Number of Titles",
        y_title = "",
        show_legend = TRUE
      )
  })

  # ----------------------------------------------------------
  # COUNTRIES
  # ----------------------------------------------------------

  output$countries_chart <- renderPlotly({

    data <- country_data %>%
      count(
        country,
        name = "titles"
      ) %>%
      arrange(
        desc(titles)
      ) %>%
      slice_head(n = 15)

    plot_ly(
      data = data,
      x = ~titles,
      y = ~reorder(country, titles),
      type = "bar",
      orientation = "h",
      marker = list(
        color = rep(
          chart_colors,
          length.out = nrow(data)
        )
      ),
      hovertemplate =
        paste0(
          "<b>%{y}</b>",
          "<br>Titles: %{x:,}",
          "<extra></extra>"
        )
    ) %>%

      dark_layout(
        x_title = "Number of Titles",
        y_title = "",
        show_legend = FALSE
      )
  })

  # ----------------------------------------------------------
  # COUNTRIES BY TYPE
  # ----------------------------------------------------------

  output$countries_type <- renderPlotly({

    top_countries <- country_data %>%
      count(
        country,
        name = "total"
      ) %>%
      arrange(
        desc(total)
      ) %>%
      slice_head(n = 10) %>%
      pull(country)

    data <- country_data %>%
      filter(
        country %in% top_countries
      ) %>%
      count(
        country,
        type,
        name = "titles"
      )

    plot_ly(
      data = data,
      x = ~titles,
      y = ~reorder(country, titles),
      color = ~type,
      type = "bar",
      orientation = "h",
      colors = c(
        "Movie" = NETFLIX_RED,
        "TV Show" = "#00A8E8"
      ),
      hovertemplate =
        paste0(
          "<b>%{y}</b>",
          "<br>Type: %{fullData.name}",
          "<br>Titles: %{x:,}",
          "<extra></extra>"
        )
    ) %>%

      dark_layout(
        x_title = "Number of Titles",
        y_title = "",
        show_legend = TRUE
      )
  })

  # ----------------------------------------------------------
  # RATINGS
  # ----------------------------------------------------------

  output$ratings_chart <- renderPlotly({

    data <- netflix %>%
      filter(
        !is.na(vote_average),
        vote_average > 0
      ) %>%
      mutate(
        rating_bucket =
          floor(vote_average * 2) / 2
      ) %>%
      count(
        rating_bucket,
        name = "titles"
      ) %>%
      arrange(rating_bucket)

    plot_ly(
      data = data,
      x = ~rating_bucket,
      y = ~titles,
      type = "bar",
      marker = list(
        color = rep(
          chart_colors,
          length.out = nrow(data)
        )
      ),
      hovertemplate =
        paste0(
          "Rating: %{x:.1f}",
          "<br>Titles: %{y:,}",
          "<extra></extra>"
        )
    ) %>%

      dark_layout(
        x_title = "Audience Rating",
        y_title = "Number of Titles",
        show_legend = FALSE
      )
  })

  # ----------------------------------------------------------
  # RATINGS BY TYPE
  # ----------------------------------------------------------

  output$ratings_type <- renderPlotly({

    data <- netflix %>%
      filter(
        !is.na(vote_average),
        vote_average > 0
      ) %>%
      mutate(
        rating_bucket =
          floor(vote_average * 2) / 2
      ) %>%
      count(
        rating_bucket,
        type,
        name = "titles"
      )

    plot_ly(
      data = data,
      x = ~rating_bucket,
      y = ~titles,
      color = ~type,
      type = "bar",
      colors = c(
        "Movie" = NETFLIX_RED,
        "TV Show" = "#00A8E8"
      ),
      hovertemplate =
        paste0(
          "Rating: %{x:.1f}",
          "<br>Type: %{fullData.name}",
          "<br>Titles: %{y:,}",
          "<extra></extra>"
        )
    ) %>%

      dark_layout(
        x_title = "Audience Rating",
        y_title = "Number of Titles",
        show_legend = TRUE
      )
  })

  # ----------------------------------------------------------
  # TOP RATED TITLES
  # ----------------------------------------------------------

  output$top_rated <- renderTable({

    netflix %>%
      filter(
        !is.na(vote_average),
        vote_average > 0,
        !is.na(vote_count),
        vote_count >= 100
      ) %>%
      arrange(
        desc(vote_average),
        desc(vote_count)
      ) %>%
      transmute(
        Title = title,
        Type = type,
        `Audience Rating` =
          round(vote_average, 2),
        `Vote Count` =
          scales::comma(vote_count)
      ) %>%
      slice_head(n = 15)
  })

  # ----------------------------------------------------------
  # DIRECTORS
  # ----------------------------------------------------------

  output$directors <- renderPlotly({

    data <- netflix %>%
      filter(
        !is.na(director)
      ) %>%
      separate_rows(
        director,
        sep = ",|\\|"
      ) %>%
      mutate(
        director =
          str_trim(director)
      ) %>%
      filter(
        director != ""
      ) %>%
      count(
        director,
        name = "titles"
      ) %>%
      arrange(
        desc(titles)
      ) %>%
      slice_head(n = 15)

    plot_ly(
      data = data,
      x = ~titles,
      y = ~reorder(director, titles),
      type = "bar",
      orientation = "h",
      marker = list(
        color = NETFLIX_RED
      ),
      hovertemplate =
        paste0(
          "<b>%{y}</b>",
          "<br>Titles: %{x:,}",
          "<extra></extra>"
        )
    ) %>%

      dark_layout(
        x_title = "Number of Titles",
        y_title = "",
        show_legend = FALSE
      )
  })

  # ----------------------------------------------------------
  # ACTORS
  # ----------------------------------------------------------

  output$actors <- renderPlotly({

    data <- netflix %>%
      filter(
        !is.na(cast)
      ) %>%
      separate_rows(
        cast,
        sep = ",|\\|"
      ) %>%
      mutate(
        cast =
          str_trim(cast)
      ) %>%
      filter(
        cast != ""
      ) %>%
      count(
        cast,
        name = "appearances"
      ) %>%
      arrange(
        desc(appearances)
      ) %>%
      slice_head(n = 15)

    plot_ly(
      data = data,
      x = ~appearances,
      y = ~reorder(cast, appearances),
      type = "bar",
      orientation = "h",
      marker = list(
        color = "#9B59B6"
      ),
      hovertemplate =
        paste0(
          "<b>%{y}</b>",
          "<br>Appearances: %{x:,}",
          "<extra></extra>"
        )
    ) %>%

      dark_layout(
        x_title = "Number of Appearances",
        y_title = "",
        show_legend = FALSE
      )
  })

  # ----------------------------------------------------------
  # POPULAR TITLES
  # ----------------------------------------------------------

  output$popular_titles <- renderTable({

    netflix %>%
      filter(
        !is.na(popularity)
      ) %>%
      arrange(
        desc(popularity)
      ) %>%
      transmute(
        Title = title,
        Type = type,
        Popularity =
          round(popularity, 2),
        `Vote Count` =
          scales::comma(vote_count),
        `Audience Rating` =
          round(vote_average, 2)
      ) %>%
      slice_head(n = 15)
  })

  # ----------------------------------------------------------
  # POPULARITY BY TYPE
  # ----------------------------------------------------------

  output$popularity_type <- renderPlotly({

    data <- netflix %>%
      filter(
        !is.na(popularity),
        popularity > 0
      )

    plot_ly(
      data = data,
      y = ~popularity,
      x = ~type,
      color = ~type,
      type = "box",
      colors = c(
        "Movie" = NETFLIX_RED,
        "TV Show" = "#00A8E8"
      ),
      boxpoints = FALSE,
      hovertemplate =
        paste0(
          "Type: %{x}",
          "<br>Popularity: %{y:.2f}",
          "<extra></extra>"
        )
    ) %>%

      dark_layout(
        x_title = "",
        y_title = "Popularity",
        y_log = TRUE,
        show_legend = FALSE
      )
  })

 # ----------------------------------------------------------
# VOTES VS POPULARITY
# ----------------------------------------------------------

output$votes_popularity <- renderPlotly({

  data <- netflix %>%
    filter(
      !is.na(vote_count),
      !is.na(popularity),
      vote_count > 0,
      popularity > 0
    )

  plot_ly(
    data = data,
    x = ~vote_count,
    y = ~popularity,
    color = ~type,
    type = "scatter",
    mode = "markers",
    colors = c(
      "Movie" = NETFLIX_RED,
      "TV Show" = "#00A8E8"
    ),
    marker = list(
      size = 6,
      opacity = 0.55
    ),
    text = ~paste0(
      "<b>", title, "</b>",
      "<br>Type: ", type,
      "<br>Votes: ", format(vote_count, big.mark = ","),
      "<br>Popularity: ", round(popularity, 2)
    ),
    hoverinfo = "text"
  ) %>%

    dark_layout(
      x_title = "Vote Count",
      y_title = "Popularity",
      x_log = TRUE,
      y_log = TRUE,
      show_legend = TRUE
    )
})

  # ----------------------------------------------------------
  # FINANCIAL: BUDGET TABLE
  # ----------------------------------------------------------

  output$budget_table <- renderTable({

    netflix %>%
      filter(
        type == "Movie",
        !is.na(budget),
        budget > 0
      ) %>%
      arrange(
        desc(budget)
      ) %>%
      transmute(
        Title = title,
        Budget =
          scales::dollar(
            budget,
            accuracy = 1
          ),
        Revenue =
          scales::dollar(
            revenue,
            accuracy = 1
          )
      ) %>%
      slice_head(n = 15)
  })

  # ----------------------------------------------------------
  # FINANCIAL: REVENUE TABLE
  # ----------------------------------------------------------

  output$revenue_table <- renderTable({

    netflix %>%
      filter(
        type == "Movie",
        !is.na(revenue),
        revenue > 0
      ) %>%
      arrange(
        desc(revenue)
      ) %>%
      transmute(
        Title = title,
        Budget =
          scales::dollar(
            budget,
            accuracy = 1
          ),
        Revenue =
          scales::dollar(
            revenue,
            accuracy = 1
          )
      ) %>%
      slice_head(n = 15)
  })

  # ----------------------------------------------------------
  # BUDGET VS REVENUE
  # ----------------------------------------------------------

  output$budget_revenue <- renderPlotly({

    data <- netflix %>%
      filter(
        type == "Movie",
        !is.na(budget),
        !is.na(revenue),
        budget > 0,
        revenue > 0
      )

    plot_ly(
      data = data,
      x = ~budget,
      y = ~revenue,
      type = "scatter",
      mode = "markers",
      marker = list(
        color = NETFLIX_RED,
        size = 7,
        opacity = 0.55
      ),
      text = ~title,
      hovertemplate =
        paste0(
          "<b>%{text}</b>",
          "<br>Budget: $%{x:,.0f}",
          "<br>Revenue: $%{y:,.0f}",
          "<extra></extra>"
        )
    ) %>%

      dark_layout(
        x_title = "Movie Budget",
        y_title = "Movie Revenue",
        x_log = TRUE,
        y_log = TRUE,
        show_legend = FALSE
      )
  })

  # ----------------------------------------------------------
  # BDA: AVERAGE REVENUE BY GENRE
  # ----------------------------------------------------------

  output$bda_genre_revenue <- renderPlotly({
    data <- genre_data %>%
      filter(type == "Movie", !is.na(revenue), revenue > 0) %>%
      group_by(genres) %>%
      summarise(
        Average_Revenue = mean(revenue, na.rm = TRUE),
        Movies = n(),
        .groups = "drop"
      ) %>%
      filter(Movies >= 5) %>%
      arrange(desc(Average_Revenue)) %>%
      slice_head(n = 10)

    plot_ly(data = data, x = ~Average_Revenue, y = ~reorder(genres, Average_Revenue),
            type = "bar", orientation = "h",
            marker = list(color = rep(chart_colors, length.out = nrow(data))),
            text = ~Movies,
            hovertemplate = paste0("<b>%{y}</b><br>Average revenue: $%{x:,.0f}<br>Movies: %{text:,}<extra></extra>")) %>%
      dark_layout(x_title = "Average Movie Revenue", y_title = "", show_legend = FALSE)
  })

  # ----------------------------------------------------------
  # BDA: AVERAGE AUDIENCE RATING BY GENRE
  # ----------------------------------------------------------

  output$bda_genre_rating <- renderPlotly({
    data <- genre_data %>%
      filter(!is.na(vote_average), vote_average > 0) %>%
      group_by(genres) %>%
      summarise(
        Average_Rating = mean(vote_average, na.rm = TRUE),
        Titles = n(),
        .groups = "drop"
      ) %>%
      filter(Titles >= 20) %>%
      arrange(desc(Average_Rating)) %>%
      slice_head(n = 10)

    plot_ly(data = data, x = ~Average_Rating, y = ~reorder(genres, Average_Rating),
            type = "bar", orientation = "h",
            marker = list(color = rep(chart_colors, length.out = nrow(data))),
            text = ~Titles,
            hovertemplate = paste0("<b>%{y}</b><br>Average rating: %{x:.2f}<br>Titles: %{text:,}<extra></extra>")) %>%
      dark_layout(x_title = "Average Audience Rating", y_title = "", show_legend = FALSE)
  })

  # ----------------------------------------------------------
  # BDA: REVENUE EFFICIENCY
  # ----------------------------------------------------------

  output$bda_efficiency <- renderTable({
    eligible <- netflix %>%
      filter(type == "Movie", !is.na(budget), budget > 0, !is.na(revenue), revenue > 0)
    threshold <- median(eligible$budget, na.rm = TRUE)

    eligible %>%
      filter(budget >= threshold) %>%
      mutate(
        Revenue_Multiple = revenue / budget
      ) %>%
      arrange(desc(Revenue_Multiple)) %>%
      transmute(
        Title = title,
        Budget = scales::dollar(budget, accuracy = 1),
        Revenue = scales::dollar(revenue, accuracy = 1),
        `Revenue / Budget` = paste0(round(Revenue_Multiple, 2), "x")
      ) %>%
      slice_head(n = 10)
  })

  # ----------------------------------------------------------
  # BDA: HIGH-ENGAGEMENT TITLES
  # ----------------------------------------------------------

  output$bda_engagement <- renderPlotly({
    data <- netflix %>%
      filter(!is.na(vote_count), vote_count > 0, !is.na(vote_average), vote_average > 0) %>%
      arrange(desc(vote_count)) %>%
      slice_head(n = 15) %>%
      arrange(vote_count)

    plot_ly(data = data, x = ~vote_count, y = ~vote_average,
            type = "scatter", mode = "markers", color = ~type,
            colors = c("Movie" = NETFLIX_RED, "TV Show" = "#00A8E8"),
            marker = list(size = 9, opacity = 0.75), text = ~title,
            hovertemplate = paste0("<b>%{text}</b><br>Votes: %{x:,}<br>Audience rating: %{y:.2f}<extra></extra>")) %>%
      dark_layout(x_title = "Vote Count", y_title = "Audience Rating", x_log = TRUE, show_legend = TRUE)
  })

}

# ============================================================
# START SHINY APPLICATION
# ============================================================

shinyApp(
  ui = ui,
  server = server
)

