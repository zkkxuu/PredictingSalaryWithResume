remotes::install_github("rstudio/chromote")
remotes::install_github("rundel/hayalbaz")
library(hayalbaz)
library(chromote)
library(rvest)
library(tidyverse)
library(stringr)
library(devtools)
scrape_data <- function(n){
  test <- puppet$new()
  # 10 items per page
  starts <- seq(0, n, by = 10)
  full_links <- tibble()
  for(start1 in starts) {
    cat(start1, "\n")
    url <- paste0("https://www.indeed.com/jobs?q=analyst&start=", start1)
    test$goto(url)
    test$wait_on_load()
    
    a <- test$get_source()
    
    links <- a %>%
      read_html() %>%
      html_nodes(".mosaic-zone a") %>%
      html_attr("href") %>%
      tibble(link = .) %>%
      filter(str_detect(link, "&vjs="))
    
    full_links <- rbind(full_links, links)
  }
  # remove duplicated values
  job_links <- tibble(link = unique(full_links$link))
  # add prefix to each link
  job_links <- job_links %>%
    mutate(link = paste0("https://www.indeed.com", link))
  # table used to store each job's information
  job_table <- data.frame()
  for (i in seq(1, nrow(job_links))){
    link <- job_links[i,] %>% pull()
    job_page <- read_html(link)
    tmp <- data.frame(
      job_title = ifelse(length(job_page %>%
                                  html_elements(".jobsearch-JobInfoHeader-title") %>%
                                  html_text2()) == 0, NA, job_page %>%
                           html_elements(".jobsearch-JobInfoHeader-title") %>%
                           html_text2()),
      job_salary = ifelse(length(job_page %>%
                                   html_elements(".jobsearch-JobMetadataHeader-item .icl-u-xs-mr--xs") %>%
                                   html_text2()) == 0, NA, job_page %>%
                            html_elements(".jobsearch-JobMetadataHeader-item .icl-u-xs-mr--xs") %>%
                            html_text2()),
      job_reviews = ifelse(length(job_page %>%
                                    html_elements(".icl-Ratings-link .icl-Ratings-count") %>%
                                    html_text2()) == 0, 0, job_page %>%
                             html_elements(".icl-Ratings-link .icl-Ratings-count") %>%
                             html_text2()),
      job_remote = ifelse(length(job_page %>%
                                   html_elements(".jobsearch-DesktopStickyContainer-companyrating~ div+ div") %>%
                                   html_text2()) == 0, "on site", job_page %>%
                            html_elements(".jobsearch-DesktopStickyContainer-companyrating~ div+ div") %>%
                            html_text2()),
      job_des = ifelse(length(job_page %>%
                                html_elements("#jobDescriptionText") %>%
                                html_text2()) == 0, NA, job_page %>%
                         html_elements("#jobDescriptionText") %>%
                         html_text2()),
      job_loc = ifelse(length(job_page %>%
                                html_elements(".jobsearch-DesktopStickyContainer-companyrating+ div") %>%
                                html_text2()) == 0, NA, job_page %>%
                         html_elements(".jobsearch-DesktopStickyContainer-companyrating+ div") %>%
                         html_text2()),
      job_type = ifelse(length(job_page %>%
                                 html_elements(" .jobsearch-JobMetadataHeader-item.icl-u-xs-mt--xs") %>%
                                 html_text2()) == 0, NA, job_page %>%
                          html_elements(" .jobsearch-JobMetadataHeader-item.icl-u-xs-mt--xs") %>%
                          html_text2())
      
    )
    job_table <- rbind(job_table, tmp)
  }
  # remove NA
  job_table <- job_table[!is.na(job_table$job_salary),]
  # remove duplicated values (based on description)
  job_table <- job_table[!duplicated(job_table$job_des), ]
  return(job_table)
}