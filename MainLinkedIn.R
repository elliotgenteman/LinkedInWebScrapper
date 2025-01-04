# 1. Load necessary libraries
# install.packages(c("rvest", "dplyr", "httr"))
library(rvest)
library(dplyr)
library(httr)

# 2. Format Search Parameter
format_search_parameter <- function(search_term) {
  gsub(" ", "%20", search_term)
}

# 3. Scrape Job Listings from LinkedIn
# Existing scrape_jobs function
scrape_jobs <- function(search_term) {
  formatted_search <- format_search_parameter(search_term)
  base_url <- "https://www.linkedin.com/jobs/search/?keywords="
  search_url <- paste0(base_url, formatted_search)
  
  user_agents <- c(
    "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/101.0.4951.41 Safari/537.36",
    "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Firefox/100.0 Safari/537.36",
    "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/102.0.5005.61 Safari/537.36"
  )
  
  random_user_agent <- sample(user_agents, 1)
  Sys.sleep(runif(1, 10, 30))  # Random delay between 10 and 30 seconds
  
  response <- tryCatch(
    GET(search_url,
        user_agent(random_user_agent),
        add_headers(Referer = "https://www.google.com")
    ),
    error = function(e) {
      message("Error fetching the URL: ", e)
      return(NULL)
    }
  )
  
  if (is.null(response) || status_code(response) != 200) {
    message("Failed to fetch the page. Status code: ", status_code(response))
    return(data.frame())
  }
  
  webpage <- read_html(response)
  
  # Updated selectors for LinkedIn
  job_titles <- webpage %>% html_nodes(".base-search-card__title") %>% html_text(trim = TRUE)
  companies <- webpage %>% html_nodes(".base-search-card__subtitle") %>% html_text(trim = TRUE)
  locations <- webpage %>% html_nodes(".job-search-card__location") %>% html_text(trim = TRUE)
  posting_dates <- webpage %>% html_nodes(".job-search-card__listdate") %>% html_text(trim = TRUE)
  job_links <- webpage %>% html_nodes(".base-card__full-link") %>% html_attr("href")
  
  # Ensure vectors are the same length
  max_length <- max(length(job_titles), length(companies), length(locations), length(posting_dates), length(job_links))
  job_titles <- c(job_titles, rep(NA, max_length - length(job_titles)))
  companies <- c(companies, rep(NA, max_length - length(companies)))
  locations <- c(locations, rep(NA, max_length - length(locations)))
  posting_dates <- c(posting_dates, rep(NA, max_length - length(posting_dates)))
  job_links <- c(job_links, rep(NA, max_length - length(job_links)))
  
  jobs <- data.frame(
    Title = job_titles,
    Company = companies,
    Location = locations,
    PostingDate = posting_dates,
    JobLink = job_links,
    stringsAsFactors = FALSE
  )
  
  return(jobs)
}


# 4. Save Results to CSV
save_to_csv <- function(data, filename = "job_listings.csv") {
  write.csv(data, filename, row.names = FALSE)
  message("Data saved to ", filename)
}

# 5. Combine Steps
scrape_and_save_jobs <- function(search_term) {
  # Scrape jobs
  jobs <- scrape_jobs(search_term)
  
  # Print the first few rows of the result
  print(head(jobs))
  
  # Save to CSV
  save_to_csv(jobs, paste0(gsub(" ", "_", search_term), "_Linkedin_Jobs.csv"))
}

# 6. Run Scraper
scrape_and_save_jobs("Data Analyst")

