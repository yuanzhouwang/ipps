# Run from the project root (open ipps.Rproj in RStudio).
library(rvest)
library(dplyr)
library(lubridate)

# Currently only works for BRecorder website

base_url <- "https://www.brecorder.com/trends"
# -------------------------------------------- #
# Adjust keyword as needed
keyword <- "ipps"
# Adjust for number of pages of results needed
pages <- 1000
# -------------------------------------------- #

scrape_articles <- function(url) {
  # Read HTML
  page <- read_html(url)
  
  # Get all article blocks
  articles <- page %>% html_nodes("article.story")
  
  # Extract info from each article
  data <- lapply(articles, function(article) {
    title_node <- article %>% html_node("h2.story__title a.story__link")
    title <- title_node %>% html_text(trim = TRUE)
    link <- title_node %>% html_attr("href") %>% xml2::url_absolute(url)
    
    # Extract publication date from title attribute inside span.timestamp--time
    date_raw <- article %>%
      html_node("span.timestamp--time") %>%
      html_attr("title")
    
    # Parse the date string
    pub_date <- as.Date(substr(date_raw, 1, 10))
    
    data.frame(
      title = title,
      link = link,
      pub_date = pub_date,
      stringsAsFactors = FALSE
    )
  })
  
  return(bind_rows(data))
}

all_articles <- data.frame()
for(i in 1:pages) {
  cat("Scraping page ", i, "\n")
  url <- paste0(c(base_url, keyword, i, "10"), collapse = "/")
  articles <- scrape_articles(url)
  if(nrow(articles) == 0) {
    break
  }
  else {
    all_articles <- bind_rows(all_articles, articles)
  }
  Sys.sleep(2)
  
}
# Use the same filename consumed by ippstimeline.R.
write.csv(all_articles,
          file.path("scraping", paste0("br_keyword_", keyword, ".csv")),
          row.names = FALSE)
