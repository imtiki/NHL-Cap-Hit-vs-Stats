library(RSelenium)
library(rvest)
library(dplyr)
library(stringr)
library(ggplot2)
library(plotly)

url = "https://www.capfriendly.com/browse/active/2024/caphit/all/forwards?stats-season=2024&display=expiry-year&hide=clauses,age,handed,expiry-status,salary,goalie-stats&limits=gp-10-90"
cf_page <- read_html(url)
table_node <- html_nodes(cf_page, "table") 
stats <- html_table(table_node)
write.table(stats, "nhlstats.txt", row.names = FALSE)
url2 = "https://www.capfriendly.com/browse/active/2024/caphit/all/forwards?stats-season=2024&display=expiry-year&hide=clauses,age,handed,expiry-status,salary,goalie-stats&limits=gp-10-90&pg=2"
cf2 <- read_html(url2)
tn2 <- html_nodes(cf2, "table")
s2 <- html_table(tn2)
write.table(s2, "nhlstats.txt", append = TRUE, row.names = FALSE)
url3 <- "https://www.capfriendly.com/browse/active/2024/caphit/all/forwards?stats-season=2024&display=expiry-year&hide=clauses,age,handed,expiry-status,salary,goalie-stats&limits=gp-10-90&pg=3"
cf3 <- read_html(url3)
tn3 <- html_nodes(cf3, "table")
s3 <- html_table(tn3)
write.table(s3, "nhlstats.txt", append = TRUE, row.names = FALSE)
url4 <- "https://www.capfriendly.com/browse/active/2024/caphit/all/forwards?stats-season=2024&display=expiry-year&hide=clauses,age,handed,expiry-status,salary,goalie-stats&limits=gp-10-90&pg=4"
cf4 <- read_html(url4)
tn4 <- html_nodes(cf4, "table")
s4 <- html_table(tn4)
write.table(s4, "nhlstats.txt", append = TRUE, row.names = FALSE)
url5 <- "https://www.capfriendly.com/browse/active/2024/caphit/all/forwards?stats-season=2024&display=expiry-year&hide=clauses,age,handed,expiry-status,salary,goalie-stats&limits=gp-10-90&pg=5"
cf5 <- read_html(url5)
tn5 <- html_nodes(cf5, "table")
s5 <- html_table(tn5)
write.table(s5, "nhlstats.txt", append = TRUE, row.names = FALSE)
url6 <- "https://www.capfriendly.com/browse/active/2024/caphit/all/forwards?stats-season=2024&display=expiry-year&hide=clauses,age,handed,expiry-status,salary,goalie-stats&limits=gp-10-90&pg=6"
cf6 <- read_html(url6)
tn6 <- html_nodes(cf6, "table")
s6 <- html_table(tn6)
write.table(s6, "nhlstats.txt", append = TRUE, row.names = FALSE)
url7 <- "https://www.capfriendly.com/browse/active/2024/caphit/all/forwards?stats-season=2024&display=expiry-year&hide=clauses,age,handed,expiry-status,salary,goalie-stats&limits=gp-10-90&pg=7"
cf7 <- read_html(url7)
tn7 <- html_nodes(cf7, "table")
s7 <- html_table(tn7)
write.table(s7, "nhlstats.txt", append = TRUE, row.names = FALSE)
url8 <- "https://www.capfriendly.com/browse/active/2024/caphit/all/forwards?stats-season=2024&display=expiry-year&hide=clauses,age,handed,expiry-status,salary,goalie-stats&limits=gp-10-90&pg=8"
cf8 <- read_html(url8)
tn8 <- html_nodes(cf8, "table")
s8 <- html_table(tn8)
write.table(s8, "nhlstats.txt", append = TRUE, row.names = FALSE)
url9 <- "https://www.capfriendly.com/browse/active/2024/caphit/all/forwards?stats-season=2024&display=expiry-year&hide=clauses,age,handed,expiry-status,salary,goalie-stats&limits=gp-10-90&pg=9"
cf9 <- read_html(url9)
tn9 <- html_nodes(cf9, "table")
s9 <- html_table(tn9)
write.table(s9, "nhlstats.txt", append = TRUE, row.names = FALSE)
url10 <- "https://www.capfriendly.com/browse/active/2024/caphit/all/forwards?stats-season=2024&display=expiry-year&hide=clauses,age,handed,expiry-status,salary,goalie-stats&limits=gp-10-90&pg=10"
cf10 <- read_html(url10)
tn10 <- html_nodes(cf10, "table")
s10 <- html_table(tn10)
write.table(s10, "nhlstats.txt", append = TRUE, row.names = FALSE)
url11 <- "https://www.capfriendly.com/browse/active/2024/caphit/all/forwards?stats-season=2024&display=expiry-year&hide=clauses,age,handed,expiry-status,salary,goalie-stats&limits=gp-10-90&pg=11"
cf11 <- read_html(url11)
tn11 <- html_nodes(cf11, "table")
s11 <- html_table(tn11)
write.table(s11, "nhlstats.txt", append = TRUE, row.names = FALSE)

# Cleaning Data

f <- read.table("nhlstats.txt", 
                header = TRUE, sep = " ")

f$PLAYER <- str_replace_all(f$PLAYER, "[[:digit:].]", "")
f$CAP.HIT <- str_replace_all(f$CAP.HIT, "[$,]", "")
f$PLAYER <- trimws(f$PLAYER)
f$CAP.HIT <- trimws(f$CAP.HIT)
f$CAP.HIT <- as.numeric(f$CAP.HIT)
f$G <- strtoi(f$G)
f$EXP..YEAR <- strtoi(f$EXP..YEAR)
f$GP <- strtoi(f$GP)
f$P <- strtoi(f$P)
f$A <- strtoi(f$A)
f$P.GP <- as.numeric(f$P.GP)
f <- f %>% filter(PLAYER != "PLAYER")


write.table(f, "nhlstats.txt", sep = " ", row.names = FALSE)



f$costeffectiveness <- f$P.GP / f$CAP.HIT

g <- f %>% filter(CAP.HIT > 1500000) 

g$CapHitPerMillion <- g$CAP.HIT / 1000000

p <- plot_ly(type = "scatter", mode = "markers")
p <- p %>%
  add_trace(x = g$P.GP, 
            y = g$CapHitPerMillion,
            marker = list(color = "red"),
            hovertemplate = paste(g$PLAYER, '<i>Points Per Game<i>: %{x}',
                                  '<i>Cap Hit per Million<i>: %{y}', 
                                  sep = '\n'),
            showlegend = F) 



