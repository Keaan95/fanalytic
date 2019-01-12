install.packages(c("reshape", "MASS", "psych", "Rglpk", "XML", "data.table"), dependencies=TRUE)

devtools::install_github(repo = "isaactpetersen/ffanalytics")

library("XML")

wr4_nfl <- readHTMLTable("http://fantasy.nfl.com/research/projections?offset=76&position=3&sort=projectedPts&statCategory=projectedStats&statSeason=2014&statType=seasonProjectedStats", stringsAsFactors = FALSE)$`NULL`
wr5_nfl <- readHTMLTable("http://fantasy.nfl.com/league/1567471/history/2017/schedule?gameSeason=2017&leagueId=1567471&scheduleDetail=4&scheduleType=week&standingsTab=schedule", stringsAsFactors = FALSE)$`NULL`
wr6_nfl <- readHTMLTable("http://fantasy.nfl.com/research/projections?offset=126&position=3&sort=projectedPts&statCategory=projectedStats&statSeason=2014&statType=seasonProjectedStats", stringsAsFactors = FALSE)$`NULL`
te1_nfl <- readHTMLTable("http://fantasy.nfl.com/research/projections?position=4&statCategory=projectedStats&statSeason=2014&statType=seasonProjectedStats", stringsAsFactors = FALSE)$`NULL`
te2_nfl <- readHTMLTable("http://fantasy.nfl.com/research/projections?offset=26&position=4&sort=projectedPts&statCategory=projectedStats&statSeason=2014&statType=seasonProjectedStats", stringsAsFactors = FALSE)$`NULL`


setwd("C:/Users/keaan/Documents/NFL Auction House/")

library("httr")
library("XML")

URLyear <- "http://fantasy.nfl.com/league/1567471/history/"
tempyear <- tempfile(fileext = ".html")
GET(url = URLyear, user_agent("Mozilla/5.0"), write_disk(tempyear))
docyear <- htmlParse(tempyear)

xpexpryear <- "//table[contains(@class, 'tableType-history hasGroups')]/tbody/tr[contains(@class, 'history-champ')]"
listofTableNodes <- getNodeSet(docyear, xpexpryear)
listofTableNodes

dfyear <- xmlToDataFrame(listofTableNodes, stringsAsFactors = FALSE)
dfyear <- dfyear[!duplicated(dfyear), ]
dfyear <- dfyear[!sapply(dfyear, function(x) all(x == ""))]
colnames(dfyear) <- c("Year","Champion")

dfhist <- list()
dfid <- data.frame()
dfteam <- list()
for (year in dfyear$Year){
  print(year)
  URLhist <- paste0(URLyear,year,"/standings?historyStandingsType=regular")
  temphist <- tempfile(fileext = ".html")
  GET(url = URLhist, user_agent("Mozilla/5.0"), write_disk(temphist))
  dochist <- htmlParse(temphist)
  
  xpexprhist <- "//table[contains(@class, 'tableType-team')]/tbody/tr[contains(@class, 'team-')]"
  listofTableNodes <- getNodeSet(dochist, xpexprhist)
  listofTableNodes
  
  dfhist[[year]] <- xmlToDataFrame(listofTableNodes, stringsAsFactors = FALSE)
  dfhist[[year]] <- dfhist[[year]][!duplicated(dfhist[[year]]), ]
  colnames(dfhist[[year]]) <- c("Seed","Team","Record","Pct","Streak","PointsFor","PointsAgainst")
  
  
  
  URLteambase <- paste0(URLyear,year,"/schedule?standingsTab=schedule&scheduleType=team&leagueId=1567471&scheduleDetail=")
  
  for (team in seq(1,20))tryCatch({
    print(team)
    URLteam <- paste0(URLteambase,team)
    temp <- tempfile(fileext = ".html")
    GET(url = URLteam, user_agent("Mozilla/5.0"), write_disk(temp))
    
    doc <- htmlParse(temp)
    
    xpexpr <- "//table[contains(@class, 'tableType-weeks noGroups')]/tbody/tr[contains(@class, 'weeks')]"
    
    listofTableNodes <- getNodeSet(doc, xpexpr)
    
    
    dfteam[[year]][[team]] <- xmlToDataFrame(listofTableNodes, stringsAsFactors = FALSE)
    dfteam[[year]][[team]] <- cbind(dfteam[[year]][[team]],do.call(rbind, strsplit(dfteam[[year]][[team]][,3], '-| ')))
    dfteam[[year]][[team]] <- dfteam[[year]][[team]][!sapply(dfteam[[year]][[team]], function(x) all(x == ""))]
    dfteam[[year]][[team]] <- dfteam[[year]][[team]][, -c(1,3)]
    colnames(dfteam[[year]][[team]]) <- c("Team","Home","Away","Outcome")
    
    xpexpr <- "//h2[contains(@class, 'teamName teamId')]"
    
    listofTableNodes <- getNodeSet(doc, xpexpr)
    listofTableNodes
    
    df <- xmlToDataFrame(listofTableNodes, stringsAsFactors = FALSE)
    df$team <- team
    df$year <- year
    dfid <- rbind(dfid,df)
    
    
  },error = function(e) NULL)
  
}

dfinputcurr <- read.table("dfinputcurrent.txt", header=TRUE, sep="\t")
dfinputprev <- read.table("dfinputprevious.txt", header=TRUE, sep="\t")


dfidcurr <- merge(dfid, dfinputcurr, by = "text") 
dfidprev <- merge(dfid, dfinputprev, by = "text") 

dfidfull <- rbind(dfidcurr,dfidprev)
dfidfull <- dfidfull[!dfidfull$year.y > dfidfull$year.x,]

toremove <- dfidfull[!(duplicated(dfidfull[,c(2,4)]) | duplicated(dfidfull[,c(2,4)],  fromLast = TRUE)),]
toremove <- toremove[!(toremove$year.x==dfyear$Year[1] & toremove$year.y==dfyear$Year[1]),]

dfidfull2 <- dfidfull[!(do.call(paste, dfidfull) %in% do.call(paste, toremove)),]


#Everyone's Data

with(dfid, table(year))



dfid <- dfid[with(dfid,order(dfid$team,rev(dfid$year))),]

xy <- vector()
for (team in unique(dfid$team)) {
  print(team)
  for (year in unique(dfid$year)) {
    print(year)
    x <- mean(as.numeric(as.character(dfteam[[year]][[team]][,2][1:13])))
    xy <- append(xy,x)
  }
}

RSavg <- xy[!is.na(xy)]
dfid$RSavg <- RSavg

require(reshape2)
require(ggplot2)
test <- melt(dfid,id.vars=c("RSavg","year","text"))

ggplot(test, aes(x=year,y=RSavg,group=text,colour=text)) +
  labs(color = "Teams") +
  xlab("Year") +
  ylab("Points Per Game") +
  geom_point() +
  geom_line(lty="solid") +
  theme(axis.text.x = element_text(angle=90, hjust = 1)) +
  theme_bw()
             