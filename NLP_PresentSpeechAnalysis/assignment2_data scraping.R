# Load required libraries
library(rvest)
library(magrittr)
library(tidyverse)
library(dplyr)

# Read the url
url <- 'https://www.americanrhetoric.com/gwbushspeeches.htm'
web <- read_html(url)

# Extract the table containing speech titles and dates
titles <- web %>% html_nodes('table') %>% .[[2]] %>% html_table()
titles

# Extract all links from the page
link <- web %>% html_nodes('a') %>% html_attr("href")
link

# Convert titles to a data frame and clean it
clean <- data.frame(titles)
clean <- clean[-c(1:30),]  # Remove the first 30 rows
clean <- clean[,-c(4:34)]  # Remove columns 4 to 34

# Clean and combine the data from different parts
clean1 <- clean %>% slice(1:5) %>% select(-1)
names(clean1) <- c("time", "title")

clean2 <- clean %>% slice(6:34) %>% select(-3)
names(clean2) <- c("time", "title")

clean3 <- clean %>% slice(35:108) %>% select(-1)
names(clean3) <- c("time", "title")

clean_combined <- rbind(clean1, clean2, clean3)

# Further clean and process the links
link <- data.frame(link)
names(link) <- "link"
link1 <- link[!grepl("pdf", link$link),]
link2 <- link1[!grepl("mp3", link1$link),]
link2 <- link2[-c(1:22),]
link2 <- link2[-c(109:117),]

# Combine cleaned data with links
clean_combined <- cbind(clean_combined, link2)
clean_combined <- clean_combined[!grepl("http", clean_combined$link),]

# Correct the URLs
clean_combined$link <- paste('https://www.americanrhetoric.com/', clean_combined$link, sep="")
link4 <- clean_combined$link

# Extract the speech content from each link
content <- sapply(link4, function(url) {
  tryCatch({
    read_html(url) %>% html_node('td') %>% html_text()
  }, error = function(e) {
    NA
  })
})

content <- data.frame(content)

# Remove headers and clean the content
result <- sapply(content, function(text) {
  substring(text, 700)
})

result <- data.frame(result)

# Combine all the cleaned data into the final dataframe
BushData <- cbind(clean_combined, result)
BushData <- BushData[, -3]  # Remove the redundant link column

# Rename the columns appropriately
names(BushData) <- c("date", "title", "content")

# Add a political party column
BushData$Party <- 'R'

# Display the final data
BushData
