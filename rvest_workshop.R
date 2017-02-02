# Webscraping with rvest
# Clay Ford
# Research Data Services - UVa Library
# Spring 2017

# install.packages("rvest")
# install.packages("magrittr")
# install.packages("pbapply")
# install.packages("readr")
# install.packages("tidyr")
# install.packages("dplyr")
# install.packages("ggplot2")

library(rvest)
# Notice the xml2 package is also loaded. 

library(magrittr) # for extract()
library(pbapply) # progress bar (pb) apply

# We'll use the Double Colon operator (::) to access readr, dplyr and tidyr
# functions when needed. Loading the readr package masks a function in rvest.

# We use ggplot2 near the end. We'll load it if we get there. 


# Example 1 ---------------------------------------------------------------

# Let's learn a little about CSS with this Stack Overflow question: "Difference
# between id and class in CSS and when to use it"

URL <- "http://stackoverflow.com/questions/12889362/difference-between-id-and-class-in-css-and-when-to-use-it"

# First we parse the web page using read_html. Reminder: read_html is a function
# in the xml2 package. You won't find documentation for it in the rvest
# documentation.

page <- read_html(URL)

# Enter page in the console and hit Enter. We see this page has two parts: head 
# and body. Notice the default print method is to show just a few lines. If we 
# want to view the full web page source, it's better to do so in the web browser
# instead of RStudio.


# Now we can use the html_nodes() function to select all elements with a given 
# css selector or html tag. It is convenient to use the %>% operator to chain a
# parsed web page to the html_nodes() function.

# Use Ctrl+Shift+M (Win) or Cmd+Shift+M (Mac) to insert %>%

# All links on the page, <a>:
page %>% html_nodes(css = "a")

# Notice only the first 20 are displayed.

# To see the text of the links, we use the html_text() function
page %>% html_nodes(css = "a") %>% html_text()


# Let's say I want to scrape the vote totals for each answer. Let's use
# SelectorGadget. It tells us we want class .vote-count-post.

# to get all nodes with class ".vote-count-post"
page %>% html_nodes(".vote-count-post")

# to get all nodes with class ".vote-count-post" and pull out the text
page %>% html_nodes(".vote-count-post") %>% html_text()

# We could add a function to convert to integer
page %>% html_nodes(".vote-count-post") %>% html_text() %>% as.integer()


# Scrape the Related questions.

#  .sidebar-related selects the "Related" box. 
#  .question-hyperlink selects the questions
# Therefore ".sidebar-related .question-hyperlink" should scrape questions in the Related box

# Related side-bar questions
page %>% html_nodes(".sidebar-related .question-hyperlink") %>% html_text()

# Scrape the Related questions votes.
#  .answer-votes selects the vote totals
# This is hard to get with SelectorGadget
page %>% html_nodes(".sidebar-related .answer-votes") %>% html_text()
page %>% html_nodes(".sidebar-related .answer-votes") %>% html_text() %>% as.integer()


# Scrape the user names of people participating on this page.

# SelectorGadget says we should use class .user-details
page %>% html_nodes(".user-details") %>% html_text()

# This gives us more than we want. Notice each name is a link. Therefore we
# could select each "a" element within .user-details to get the user name
page %>% html_nodes(".user-details a") %>% html_text()

# Reputation score
page %>% html_nodes(".reputation-score") %>% html_text()




# Example 2 ---------------------------------------------------------------

# web scraping a table

# Figure skating stats
URL <- "http://tracings.net/2016gpf.html"
page <- read_html(URL)
# how many tables? 10
page %>% html_nodes("table")
# Do View Source and Use Ctrl + F to find "<table" to identify the tables

# Let's get the 3rd table: Senior Dance
page %>% html_nodes("table") %>% extract(3)

# convert to data frame (but notice the data frame is in a list)
page %>% html_nodes("table") %>% extract(3) %>% html_table()

# save data frame
dat <- page %>% html_nodes("table") %>% extract(3) %>% html_table()

# This returns a list object; dat[[1]] is the data frame
class(dat)
str(dat)
dat[[1]]

# convert to data frame simply by saving dat[[1]]
dat <- dat[[1]]

# change column names. 
names(dat) <- dat[1,]
names(dat)


# remove 1st row
dat <- dat[-1,]

# looks better but everything is character
dat
str(dat)

# Let's separate the columns that contain both score and placement using the
# tidyr function separate:
dat <- dat %>% tidyr::separate(SD, c("SP", "SP_place"), sep=" ")
dat <- dat %>% tidyr::separate(FD, c("FD", "FD_place"), sep=" ")
dat <- dat %>% tidyr::separate(Final, c("Final_place", "Final"), sep = " ")
dat

# Let's parse all columns except the first as numbers using the readr function
# parse_number:
dat[, -1] <- lapply(dat[, -1], readr::parse_number)
dat



# Example 3 ---------------------------------------------------------------

# Submitting a form

# UVa Library catalog search
# search for something and scrape the titles

uvaLibrary <- "http://search.lib.virginia.edu/catalog"

# establish a session
page <- html_session(uvaLibrary)

# see what forms are available
html_form(page)
html_form(page)[[1]]

# save the blank form
f0 <- html_form(page)[[1]]

# set the values for form f0
f1 <- set_values(form = f0, q = "Asimov", catalog_select = "catalog")

# submit the form using our session and save results
results <- page %>% submit_form(form = f1)
 
# extract book titles from results;
# It took some trial and error to get "dd.titleField h2 a"
results %>% html_nodes("dd.titleField h2 a") %>% html_text()

# save titles
titles <- results %>% html_nodes("dd.titleField h2 a") %>% html_text()

# Navigate to page 2 using follow_link() and get page 2 titles
results %>% follow_link("Next") %>% html_nodes("dd.titleField h2 a") %>% html_text()

# We could do something like this: combine with first 20 using c()
titles <- c(titles, results %>% follow_link("Next") %>% html_nodes("dd.titleField h2 a") %>% html_text())

# Now we have 40 titles. 

# Let's keep following "Next" until there is no more "Next", iteratively storing
# all results in a vector called "titles".

# It's good practice to allocate space (if possible) when iteratively storing 
# objects in R. The search results tell us the total number of results. In this
# case, 284. We can use that to create a vector of length 284.

n <- results %>% html_nodes(".current-page strong") %>% 
  html_text() %>% readr::parse_number()
titles <- vector("character", length = n)

# Now that we've allocated space for 284 titles, let's fill the vector with 
# titles. In R we have to specify the positions where we store elements. This
# means for page 1 results, we need do something like this:

# titles[1:20] <- results %>% html_nodes("dd.titleField h2 a") %>% html_text()

# We need to dynamically generate the positions. Fortunately we can scrape that
# off the web page.
results %>% html_nodes(".listItemNumber") %>% html_text() %>% readr::parse_number()

# So we can scrape the list numbers, and then use the list numbers to specify
# where in the vector to store the titles. Like so...

idx <- results %>% html_nodes(".listItemNumber") %>% html_text() %>% readr::parse_number()
titles[idx] <- results %>% html_nodes("dd.titleField a") %>% html_text()

# Now we can do a while loop that will continue to loop while there are any
# empty elements in the titles vector.

while(any(titles == "")){
  results <- results %>% follow_link("Next")
  idx <- results %>% html_nodes(".listItemNumber") %>% html_text() %>% readr::parse_number()
  titles[idx] <- results %>% html_nodes("dd.titleField a") %>% html_text()
}

# Note: hitting ESC will cancel the web scraping

# Finally we could write a function so we can try different search phrases. 

getTitles <- function(title){
  page <- html_session("http://search.lib.virginia.edu/catalog")
  f0 <- html_form(page)[[1]]
  f1 <- set_values(form = f0, q = title, catalog_select = "catalog")
  results <- page %>% submit_form(form = f1)
  n <- results %>% html_nodes("#resultLine .current-page strong") %>% 
    html_text() %>% readr::parse_number()
  # present results and opportunity to cancel
  ask <- menu(c("yes","no"), title = paste(n, "results. Continue?"))
  if(ask==2) stop("canceled")
  titles <- vector("character", length = n)
  # page 1 results
  idx <- results %>% html_nodes(".listItemNumber") %>% html_text() %>% readr::parse_number()
  titles[idx] <- results %>% html_nodes("dd.titleField h2 a") %>% html_text()
  # if any page 2 or beyond results
  while(any(titles == "")){
    results <- results %>% follow_link("Next")
    idx <- results %>% html_nodes(".listItemNumber") %>% html_text() %>% readr::parse_number()
    titles[idx] <- results %>% html_nodes("dd.titleField h2 a") %>% html_text()
  }
  titles
}

# Try it out
asimov <- getTitles("Asimov")

# May want to cancel this one:
getTitles("Faulkner")



# Example 4 ---------------------------------------------------------------

# Web scraping bestbuy.com
# refrigerators with french doors
# get Description, Model, Price, average review

# This is a longer, more involved example. 

# go to http://www.bestbuy.com/ and search for "refrigerators with french doors"

# Resulting URL is long
# http://www.bestbuy.com/site/searchpage.jsp?st=refrigerators+with+french+doors&_dyncharset=UTF-8&id=pcat17071&type=page&sc=Global&cp=1&nrp=&sp=&qp=&list=n&af=true&iht=y&usc=All+Categories&ks=960&keys=keys

URL <- "http://www.bestbuy.com/site/searchpage.jsp?st=refrigerators+with+french+doors&_dyncharset=UTF-8&id=pcat17071&type=page&sc=Global&cp=1&nrp=&sp=&qp=&list=n&af=true&iht=y&usc=All+Categories&ks=960&keys=keys"
page <- read_html(URL)

# Now let's figure out how to select the information we want to scrape

# Description
# found h4 with GadgetSelector
# took some trial and error to figure out the id #main-results;
# Description contains Brand, size, description and color; 
# we can separate them out if we want after scraping
page %>% html_nodes("#main-results h4")
page %>% html_nodes("#main-results h4") %>% html_text()
description <- page %>% html_nodes("#main-results h4") %>% html_text()


# Model number
# .model-number found with GadgetSelector
page %>% html_nodes(".model-number") %>% html_text()

# Would be nice not to scrape "Model:"
# Notice the model number has class .sku-value
page %>% html_nodes(".model-number")
page %>% html_nodes(".model-number .sku-value")
page %>% html_nodes(".model-number .sku-value") %>% html_text()
model <- page %>% html_nodes(".model-number .sku-value") %>% html_text()


# Price
# Is there always a price? No! Sometimes it says "See price in cart"

# Looking at the source code we see there is an attribute called "data-price"
# inside div.list-item that contains the price if one is displayed.
page %>% html_nodes("div.list-item") %>% html_attr("data-price")

# The advantage to this approach is that we'll always get an NA if there is no 
# price. There is always a "div.list-item". We could also scrape the box
# containing the price. In that case we get the sale price and reg price, or the
# phrase "See price in cart" if there is no price. But that results in extra
# clean up.

page %>% html_nodes("div.list-item") %>% html_attr("data-price") %>% as.numeric()
price <- page %>% html_nodes("div.list-item") %>% html_attr("data-price") %>% as.numeric()

# average review

# Again we use attributes. Inside div.list-item there are attributes
# "data-average-rating" and "data-review-count"
page %>% html_nodes("div.list-item") %>% 
  html_attr("data-average-rating") %>% as.numeric()

page %>% html_nodes("div.list-item") %>% 
  html_attr("data-review-count") %>% as.numeric()

avg.rating <- page %>% html_nodes("div.list-item") %>% 
  html_attr("data-average-rating") %>% as.numeric()
review.count <- page %>% html_nodes("div.list-item") %>% 
  html_attr("data-review-count") %>% as.numeric()


# Combine into data frame
# set stringsAsFactors = FALSE, else text converted to Factor
dat <- data.frame(description, model, price, avg.rating, review.count, stringsAsFactors = FALSE)

# Turn what we did into a function called getData

# Give this function a URL from BestBuy search results and it should return a
# data frame
getData <- function(url){
  page <- read_html(url)
  description <- page %>% html_nodes("#main-results h4") %>% html_text()
  model <- page %>% html_nodes(".model-number .sku-value") %>% html_text()
  price <- page %>% html_nodes("div.list-item") %>% html_attr("data-price") %>% as.numeric()
  avg.rating <- page %>% html_nodes("div.list-item") %>% 
    html_attr("data-average-rating") %>% as.numeric()
  review.count <- page %>% html_nodes("div.list-item") %>% 
    html_attr("data-review-count") %>% as.numeric()
  data.frame(description, model, price, avg.rating, 
             review.count, stringsAsFactors = FALSE)
}

# Try it out
getData(URL)

# Now let's apply our getData function to a vector of URLs (ie, all pages of
# results). We need to get URLs

# notice cp=1, cp=2, cp=3, in the URLs.
# They're the only part that changes as we page through results

# Therefore we can manually build a vector of URLs %>% 
allURLs <- paste0("http://www.bestbuy.com/site/searchpage.jsp?cp=",
                  1:5,
                  "&searchType=search&st=refrigerators%20with%20french%20doors&_dyncharset=UTF-8&id=pcat17071&type=page&sc=Global&nrp=&sp=&qp=&list=n&af=true&iht=y&usc=All%20Categories&ks=960&keys=keys")

# Now "lapply" our function to each URL in our vector.

# We'll use the pbapply function. The "pb" stands for progress bar and allows us
# to see the progress.
allDat <- pblapply(allURLs, getData)

# The result is a list. We can collapse into one data frame using the dplyr
# function bind_rows():
allDatDF <- dplyr::bind_rows(allDat)

# This works for the one search we performed. But what if we wanted to 
# generalize this code so we could search BestBuy for any product and get a data
# frame of results, all within R?

# We need to be able to do two things:
# 1. submit the search form
# 2. automatically assemble our list of URLs for our getData function

# Let's do #2 first.

# We'll create a function to generate a vector of URLs for whatever we search for.

# Notice there are 24 results per page and the total number of pages is not 
# always returned. Try searching "TV", for example. However the total number of 
# items is listed next to All Items. Therefore we could divide that number by 24
# and round to next largest integer if we get a decimal.

# use SelectorGadget to select the number next to All Items
# The result is ".active .count"
page %>% html_nodes(".active .count") %>% html_text()

# Use the parse_number() function from the readr package to handle numbers with
# commas (eg: 5,664) and to remove parentheses:
page %>% html_nodes(".active .count") %>% html_text() %>% readr::parse_number()

total <- page %>% html_nodes(".active .count") %>% 
  html_text() %>% readr::parse_number()


# divide by 24 and use ceiling to get number of pages, and then generate sequence
pages <- ceiling(total/24) %>% seq()
# create vector of URLs using pages instead of 1:5
allURLs <- paste0("http://www.bestbuy.com/site/searchpage.jsp?st=refrigerators+with+french+doors&_dyncharset=UTF-8&id=pcat17071&type=page&sc=Global&cp=",
                  pages,
                  "&nrp=&sp=&qp=&list=n&af=true&iht=y&usc=All+Categories&ks=960&keys=keys")

# create function
getURLS <- function(url){
  page <- read_html(url)
  total <- page %>% html_nodes(".active .count") %>% 
    html_text() %>% readr::parse_number()
  pages <- ceiling(total/24) %>% seq()
  paste0("http://www.bestbuy.com/site/searchpage.jsp?st=refrigerators+with+french+doors&_dyncharset=UTF-8&id=pcat17071&type=page&sc=Global&cp=",
         pages,
         "&nrp=&sp=&qp=&list=n&af=true&iht=y&usc=All+Categories&ks=960&keys=keys")
}

getURLS(URL)
allURLs <- getURLS(URL)

# try out with function
allDat <- pblapply(allURLs, getData)
allDatDF <- dplyr::bind_rows(allDat)


# Now let's build a function that allows us to submit a form request to bestbuy.com

# Recall we need to use html_session instead of read_html
bb <- html_session("http://www.bestbuy.com/")

# to see form fields use html_form()
bb %>% html_form()

# Notice  <input text> 'st':
# We want to set values for that form field

# save form
f0 <- html_form(bb)[[1]]
# set values using set_values()
f1 <- set_values(form = f0, st = "refrigerators with french doors")

bb %>% submit_form(f1)
page <- bb %>% submit_form(f1)

# the result is the same as before plus more; page is a parsed html file that we
# can use html_nodes on:
page %>% html_nodes(".active .count")

# but it also has the URL generated from the search
page$url

# We can use page$url with our getURLS() function

# First need to generalize getURLs function to work for any search;
# previously it worked for "refrigerators with french doors"

# Notice we can use strsplit to split the URL at "cp="
strsplit(page$url, "cp=1")
unlist(strsplit(page$url, "cp=1"))
parts <- unlist(strsplit(page$url, "cp=1"))
cp <- paste0("cp=",pages)
paste0(parts[1],cp,parts[2])

# Update function. Basically just add in what we did above
getURLS <- function(url){
  page <- read_html(url)
  total <- page %>% html_nodes(".active .count") %>% 
    html_text() %>% readr::parse_number()
  pages <- ceiling(total/24) %>% seq()
  cp <- paste0("cp=",pages)
  parts <- unlist(strsplit(url, "cp=1"))
  paste0(parts[1],cp,parts[2])
}

# Test it
getURLS(page$url)


# Now we can build a master function to search and scrape Best Buy
bbSearch <- function(x){
  bb <- html_session("http://www.bestbuy.com/")
  f0 <- html_form(bb)[[1]]
  f1 <- set_values(form = f0, st = x)
  page <- bb %>% submit_form(f1)
  # create vector of URLS 
  allURLs <- getURLS(page$url)
  # scrape site using getData
  allDat <- pblapply(allURLs, getData)
  dplyr::bind_rows(allDat)
}

# test it
fridgeData <- bbSearch("refrigerators with french doors")

# Try for "TV"
tvData <- bbSearch("TV")


# some basic analysis by brand

# pull Brand out of description
fridgeData$description

# Brand appears to always preceded a dash. 
strsplit(fridgeData$description, split = " - ") %>% sapply(function(x)x[1])
fridgeData$brand <- strsplit(fridgeData$description, split = " - ") %>% sapply(function(x)x[1])

# Should probably just add that to the function above!

# Break down of brands
table(fridgeData$brand)

# mean avg.rating by brand
library(dplyr)
fridgeData %>% 
  group_by(brand) %>% 
  summarize(score = mean(avg.rating, na.rm=TRUE),
            reviews = sum(review.count))

# Top 10 highest rated models for those with at least 100 reviews
fridgeData %>% 
  arrange(desc(avg.rating)) %>% 
  filter(review.count > 100) %>% 
  head(n=10) %>% 
  select(description, avg.rating, review.count, price) %>% 
  View()

# distribution of prices by brand
library(ggplot2)
ggplot(fridgeData, aes(x = brand, y = price)) + geom_boxplot()


