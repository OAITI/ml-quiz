## Load libraries
## Scraping
library(XML)
library(RCurl)
library(httr)
library(jsonlite)
library(stringr)

## Top Page
top_page <- htmlTreeParse(readLines("https://developers.google.com/machine-learning/glossary/"), useInternalNodes = TRUE)

## Defining data frame to hold scraped results
results <- data.frame(Header=str_trim(xpathSApply(top_page, "//h2[@class='hide-from-toc']",xmlValue)),Defination_Text=NA)

## Parsing page to select defination subsets
top_page_list <- xmlToList(xpathSApply(top_page, "//*[@id='gc-wrapper']/div[2]/article/article/div[2]")[[1]])
sel_idx <- which(sapply(top_page_list,function(x) ".attrs" %in% names(x)))
sel_idx <- c(sel_idx[sapply(top_page_list[sel_idx],function(x) !(x$.attrs %in% c("glossary","glossary-icon-container","prettyprint","sparse-dense-tables")))],length(top_page_list))
definations <- mapply(function(x,y,data) data[(x+1):(y-2)],sel_idx[-length(sel_idx)],sel_idx[-1],MoreArgs=list(data=top_page_list))


## Parsing Subset
parse_tag <- function(x)
{
	x_l <- unlist(x)
	name_spl <- sapply(gsub("\\.\\.","\\.",names(x_l)),function(x) strsplit(x,"\\."))
	f_tag <- sapply(name_spl,function(x) head(x,1))
	l_tag <- sapply(name_spl,function(x) tail(x,1))
	paste0(x_l[(!(f_tag %in% c("div","table"))) & (l_tag %in% c("text","p","strong","em","li","sub","code",""))],collapse="")
}
results$Defination_Text <- sapply(definations,parse_tag)

