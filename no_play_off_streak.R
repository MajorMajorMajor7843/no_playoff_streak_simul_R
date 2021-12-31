American_East = 1:5

American_Central = 6:10

American_West = 11:15

National_East = 16:20

National_Central = 21:25

National_West = 25:30


team_list = c(American_East,American_Central, American_West, National_East, National_Central,  National_West)

american = c(American_East, American_Central, American_West)
national = c(National_East, National_Central, National_West)


simulate_season = function(){
  east_am = sample(American_East, size = 1)
  central_am = sample(American_Central, size = 1)
  west_am = sample(American_West, size = 1)
  left_am = american[american != east_am & american != central_am & american != west_am]
  wildcard_am = sample(left_am, size = 2)
  
  east_na = sample(National_East, size = 1)
  central_na = sample(National_Central, size = 1)
  west_na = sample(National_West, size = 1)
  left_na = national[national != east_na & national != central_na & national != west_na]
  wildcard_na = sample(left_na, size = 2)
  
  return(c(east_am, central_am,  west_am, wildcard_am, east_na, central_na, west_na, wildcard_na))
}

simulate_100_seasons = function(){

playoff_history = matrix(data = 0, nrow = 30, ncol = 100)
for (i in 1:100){
  playoff_team = simulate_season()
  playoff_history[playoff_team,i] = 1 
  
}
max_out_of_all = c()
for (i in 1:30){
  streak = rle(playoff_history[i,])
  max_streak = max(streak$lengths[streak$values == 0])
  max_out_of_all = c(max_out_of_all, max_streak)
}
return (max(max_out_of_all))
}

max_100 = c()
for (i in 1:50000){
  print (i)
  max_100 = c(max_100, simulate_100_seasons())
}

hist(max_100)
