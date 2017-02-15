#read a yaml file and get the following information...
# Countries played, Players name.
#Runs scored by each player, winner, looser
# List of countries, matches date, location, 

#Is cricket a game of skills or chance?
# Analyze the games played between countries, and toss win
# Year, Game id, city - > isHome, winner, team 1, team 2
#Generate 2 lines for each country, have the same match id. 
# How many matches won with home condition after winning the toss.
#http://genomicsclass.github.io/book/pages/dplyr_tutorial.html

# http://cricsheet.org/downloads/

library("yaml")
library("dplyr")
directory = "odis_male"
matchid = 1
index = 0
nrmatches = 0
df = data.frame(matchid = integer(0), fileName=character(0),date = as.Date(character()),team=character(0), opponent = character(0), won=numeric(0), wonToss = numeric(0), description = character(0), venue=character(0), city=character(0) ,stringsAsFactors=FALSE) 
selectedTeams = c("Australia", "England", "India","New Zealand", "Pakistan", "South Africa",
                  "Sri Lanka","West Indies")

for(file in list.files(directory)) {
  l = yaml.load_file(paste(directory,file, sep = "/"))
  print(paste("reading file", file, ". Number is", matchid))
  tosswinner = l$info$toss$winner
  winner = l$info$outcome$winner
  isWinnerSameAsTossWinner = winner == tosswinner
  date = l$info$dates
  if(length(date)>1) {
    date = date[1]
  }
  venue = l$info$venue
  city = l$info$city
  if(is.null(city)) {
    city = NA
  }
  teams = l$info$teams
  # discard the no result match. 
  if(!is.null(winner)) {
    winnerIndex = match(winner, teams)
    if(winnerIndex == 1) {
      loser = teams[2]
    } else {
      loser = teams[1]
    }
    
    if(!is.na(match(winner, selectedTeams)) && !is.na(match(loser, selectedTeams)))
    {
      winnerDescription  = paste("won by", l$info$outcome$by)
      loserDescription = paste("lost by", l$info$outcome$by)
      
      winnerRow = c(matchid,file,date,winner, loser, 1, as.integer(isWinnerSameAsTossWinner), winnerDescription, venue, city)
      loserRow = c(matchid, file,date, loser,winner, 0, as.integer(!isWinnerSameAsTossWinner), loserDescription, venue, city)

      index = index +1
      df[index, ] = winnerRow
      index = index+1
      
      df[index,] = loserRow
      matchid = matchid+1
      
    }
    
  } else {
    nrmatches = nrmatches+1
  }
  
}
print(paste(" number of no result matches = ", nrmatches))

mapping  = read.csv2("venues.csv")
fullname = paste(df$venue,df$city, sep = ", ")
isHomeMatch = rep(0, nrow(df))
i = 1
for(name in fullname) {
  index = grep(name, mapping$x)
  matchCountry =  mapping$y[index]
  isHomeMatch[i] = matchCountry == df$team[i]
  i = i+1
  
}

df[,"isHomeMatch"] = isHomeMatch

selected_df = select(df, team, opponent, won, wonToss,isHomeMatch)
selected_df$won = as.integer(selected_df$won)
selected_df$wonToss = as.integer(selected_df$wonToss)
selected_df$isHomeMatch = as.integer(selected_df$isHomeMatch)

homewin = selected_df %>%
  filter(isHomeMatch==1) %>%
group_by(team,opponent) %>%
  summarise(won = sum(won), 
            total = sum(isHomeMatch)) %>%
  mutate(percent = (won / total)*100) %>%
  
  arrange(team,  opponent)

outsidewin = selected_df %>%
  filter(isHomeMatch==0) %>%
  group_by(team,opponent) %>%
  summarise(won = sum(won), 
            total = n()) %>%
  mutate(percent = (won / total)*100) %>%
  arrange(team,  opponent)


#split and plot it. 
splittedData = split(homewin, homewin$team)
splittedDataOUtsideWin = split(outsidewin, outsidewin$team)
par(mar = c(7, 4, 4, 2) + 0.1) 
fill = c("#5F9EA0", "#E1B378")

for(country in selectedTeams) {
  cdt = splittedData[country] 
  coutside = splittedDataOUtsideWin[country]
  title = paste(country, "vs Rest")


  dat1 = data.frame("type" = "Home", "percent" =round(cdt[[1]]$percent,2), "opponent"=cdt[[1]]$opponent, "total"=cdt[[1]]$total)
  dat2 = data.frame("type" = "Outside", "percent" =round(coutside[[1]]$percent,2), "opponent"=coutside[[1]]$opponent,
                    "total"=coutside[[1]]$total)
  dat = rbind(dat1, dat2)
  
  p4 = ggplot() + theme_bw() +
         geom_bar(aes(y = percent, x = opponent, 
                                fill = type), data = dat,
                            stat="identity")
  p4 = p4 + theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  p4 = p4 + geom_text(data=dat, aes(x = opponent, y = percent, label = paste0(percent,"%"," (",total,")")),
                       size=3)
  
  p4 = p4+ theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank())
  
  p4 = p4 + ggtitle(title) + labs(y = "Win percent") +  
    scale_y_continuous(labels = dollar_format(suffix = "%", prefix = "")) 
  p4 <- p4 + scale_fill_manual(values=fill)
  print(p4)

}

# Write the report in RMD file. 