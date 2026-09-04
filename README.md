# Netflix Content Analyser

A Big Data Analysis project that explores Netflix Movies and TV Shows using **R, R Shiny, and data analysis techniques**. The project transforms Netflix content data into an interactive dashboard that helps users discover patterns in content types, genres, countries, languages, audience ratings, popularity, people, and movie financial performance.

## 📌 Project Overview

The **Netflix Content Analyser** provides an interactive Netflix-style dashboard for analysing Movies and TV Shows.

The project focuses on extracting meaningful insights from Netflix datasets through:

* Data cleaning and preprocessing
* Exploratory data analysis
* Grouping and aggregation
* Statistical analysis
* Content comparison
* Popularity and audience analysis
* Financial analysis of movies
* Interactive data visualization

The final results are presented through an interactive **R Shiny dashboard**.

## 🎯 Objectives

The main objectives of this project are:

1. Analyse Netflix Movies and TV Shows.
2. Compare Movies and TV Shows across different characteristics.
3. Identify the most common genres and languages.
4. Analyse content production across countries.
5. Study audience ratings and popularity.
6. Identify frequently appearing directors and actors.
7. Analyse relationships between votes and popularity.
8. Analyse movie budget and revenue.
9. Provide an interactive dashboard for exploring the results.

## 🛠️ Technologies Used

* **R**
* **R Shiny**
* **Plotly**
* **dplyr**
* **ggplot2**
* **tidyr**
* **readr**
* **Shiny Dashboard**
* **CSV datasets**

## 📊 Dataset

The project uses two Netflix datasets:

* `netflix_movies_detailed_up_to_2025.csv`
* `netflix_tv_shows_detailed_up_to_2025.csv`

The datasets contain information such as:

* Title
* Director
* Cast
* Country
* Release Year
* Rating
* Duration
* Genres
* Language
* Popularity
* Vote Count
* Vote Average
* Budget
* Revenue

### Important Dataset Handling

The movie dataset does not contain usable movie-duration values, so movie duration is not artificially generated.

Similarly, the TV dataset contains the same season value for all records, so season-count analysis is not treated as a meaningful analytical result.

## 📈 Dashboard Analysis

The dashboard contains multiple analytical sections.

### 🏠 Overview

* Movies vs TV Shows composition

### 🎬 Movies

* Movie language distribution
* Most popular movies

### 📺 TV Shows

* TV show language distribution
* Most popular TV shows

### 🎭 Genres

* Most common genres
* Genre distribution by content type

### 🌍 Countries

* Countries producing the most content
* Movies vs TV Shows by country

### ⭐ Ratings

* Audience rating distribution
* Audience ratings by content type
* Highest-rated titles with meaningful vote counts

### 👥 People

* Directors with the most titles
* Actors appearing most frequently

### 🔥 Popularity

* Most popular titles
* Popularity by content type
* Relationship between vote count and popularity

### 💰 Financial Analysis

* Highest-budget movies
* Highest-revenue movies
* Relationship between movie budget and revenue

### 📊 Big Data Analysis

Additional aggregated analysis includes:

* Average revenue by genre
* Average audience rating by genre
* Revenue-efficient movies
* High-engagement titles

## 🖥️ Dashboard Features

The dashboard provides:

* Netflix-inspired dark interface
* Interactive graphs
* Interactive Plotly visualizations
* Hover information
* Analytical tables
* Separate analysis sections
* Content-type comparisons
* Data-driven insights

## 🚀 How to Run the Project

### 1. Install R

Download and install R from the official R website.

### 2. Install Required Packages

Open R or RStudio and run:

```r
install.packages(c(
  "shiny",
  "shinydashboard",
  "plotly",
  "dplyr",
  "ggplot2",
  "tidyr",
  "readr"
))
```

### 3. Clone the Repository

```bash
git clone https://github.com/YOUR-USERNAME/Netflix-Content-Analyser.git
```

Move into the project directory:

```bash
cd Netflix-Content-Analyser
```

### 4. Run the Shiny Application

Open `app.R` in RStudio and click **Run App**.

Alternatively, run:

```r
shiny::runApp()
```

## 📁 Project Structure

```text
Netflix-Content-Analyser/
│
├── app.R
├── netflix_movies_detailed_up_to_2025.csv
├── netflix_tv_shows_detailed_up_to_2025.csv
└── README.md
```

## 🔬 Big Data Analysis Approach

The project demonstrates the analytical workflow used in Big Data Analysis:

```text
Raw Netflix Data
       ↓
Data Cleaning
       ↓
Data Transformation
       ↓
Data Aggregation
       ↓
Statistical Analysis
       ↓
Visualization
       ↓
Interactive Shiny Dashboard
```

The current implementation focuses on the **R/Shiny analytics layer** using the provided datasets. Distributed processing technologies such as Apache Spark or Hadoop can be incorporated as a future scalability enhancement.

## 🔮 Future Scope

Possible future improvements include:

* Apache Spark-based distributed processing
* Hadoop/HDFS integration
* Larger Netflix datasets
* Recommendation system
* Genre-based content recommendation
* Advanced machine learning models
* Sentiment analysis of reviews
* Predictive popularity analysis
* Cloud deployment
* Real-time data processing

## 👩‍💻 Project

**Project:** Netflix Content Analyser
**Domain:** Big Data Analysis / Data Analytics
**Language:** R
**Framework:** R Shiny
**Visualization:** Plotly

## 📄 License

This project is intended for educational and academic purposes.
