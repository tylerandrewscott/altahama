library(rvest)


library(rvest)
nerr_sites <- html("http://nerrs.noaa.gov/reserves/management-profiles.html")

library(dplyr)

library(magrittr)

site.df <- data.frame(site = nerr_sites %>% html_nodes(' td:nth-child(1)') %>% html_text(),
                      site.page = nerr_sites %>%  html_nodes(" td:nth-child(4)") %>% html_nodes(".accent-link") %>% html_attr('href'),
                      site.code = nerr_sites %>%  html_nodes(" td:nth-child(3)") %>% html_nodes(".accent-link") %>% html_attr('href') %>% gsub('_SiteProfile.*.','',.) %>% gsub('.*_','',.))


mgmt.df <-  data.frame(mgmt.plan = nerr_sites %>%  html_nodes(" td:nth-child(2)") %>% html_nodes(".accent-link") %>% html_attr('href'),
                       site.code =  nerr_sites %>%  html_nodes(" td:nth-child(2)") %>% html_nodes(".accent-link") %>% html_attr('href') %>% gsub('_MgmtPlan.pdf','',.) %>%
                         gsub('.*_','',.))

profile.df <-  data.frame(site.profile = nerr_sites %>%  html_nodes(" td:nth-child(3)") %>% html_nodes(".accent-link") %>% html_attr('href'),
                       site.code =  nerr_sites %>%  html_nodes(" td:nth-child(3)") %>% html_nodes(".accent-link") %>% html_attr('href') %>% gsub('_SiteProfile.pdf','',.) %>%
                         gsub('.*_','',.))

site.df <- left_join(left_join(site.df,mgmt.df),profile.df)




