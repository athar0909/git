library(dplyr)
library(tidyr)
library(stringr)
library(gsheet)
library(googlesheets4)
library(naniar)
library(sqldf)
library(rvest)
library(stringr)

#LOAD DATA
url <- html("https://data.humdata.org/dataset/movement-range-maps")
link = url %>%
  html_nodes("a") %>%       
  html_attr("href") %>%     
  str_subset("\\.zip") %>% 
  .[[1]]                   
k = paste("https://data.humdata.org",link, sep= '')
temp <- tempfile()
temp2 <- tempfile()
download.file(k,temp)
unzip(zipfile = temp, exdir = temp2)
dt = list.files(temp2, pattern = "movement.*.txt",full.names = T)
idn = read.csv.sql(dt, header = T, sep = "\t", sql = "select * from file where `country` == 'IDN'")

covid_timeline = gsheet2tbl('docs.google.com/spreadsheets/d/1Ll0g-MW8YPmwCRcdpElS9EtRNx9YKvschCLEv7CtxMg/edit#gid=1776420023')
colnames(covid_timeline) <- as.character(unlist(covid_timeline[1,]))
cut = which(covid_timeline[,1] == "aktif")
covid_timeline <- covid_timeline[cut:nrow(covid_timeline),]
colnames(covid_timeline) = covid_timeline[1,]
covid_timeline = covid_timeline[-1,]
covid_timeline = replace_with_na_all(covid_timeline, ~.x == '#DIV/0!')
colnames(covid_timeline)[1] = "Tanggal"
covid_infected = gsheet2tbl('https://docs.google.com/spreadsheets/d/1Ll0g-MW8YPmwCRcdpElS9EtRNx9YKvschCLEv7CtxMg/edit#gid=0')
covid_infected = covid_infected[,1:35]
colnames(covid_infected)[1] = "Tanggal"

# MOBILITY CLEANING
Aceh1 = idn %>% filter(str_detect(polygon_id, pattern = "IDN.1\\. *"))
Bali1 = idn %>% filter(str_detect(polygon_id, pattern = "IDN.2\\. *"))
Babel1 = idn %>% filter(str_detect(polygon_id, pattern = "IDN.3\\. *"))
Banten1 = idn %>% filter(str_detect(polygon_id, pattern = "IDN.4\\. *"))
Bengkulu1 = idn %>% filter(str_detect(polygon_id, pattern = "IDN.5\\. *"))
Gorontalo1 = idn %>% filter(str_detect(polygon_id, pattern = "IDN.6\\. *"))
Jakarta1 = idn %>% filter(str_detect(polygon_id, pattern = "IDN.7\\. *"))
Jambi1 = idn %>% filter(str_detect(polygon_id, pattern = "IDN.8\\. *"))
Jabar1 = idn %>% filter(str_detect(polygon_id, pattern = "IDN.9\\. *"))
Jateng1 = idn %>% filter(str_detect(polygon_id, pattern = "IDN.10\\. *"))
Jatim1 = idn %>% filter(str_detect(polygon_id, pattern = "IDN.11\\. *"))
Kalbar1 = idn %>% filter(str_detect(polygon_id, pattern = "IDN.12\\. *"))
Kalsel1 = idn %>% filter(str_detect(polygon_id, pattern = "IDN.13\\. *"))
Kalteng1 = idn %>% filter(str_detect(polygon_id, pattern = "IDN.14\\. *"))
Kaltimtara1 = idn %>% filter(str_detect(polygon_id, pattern = "IDN.15\\. *"))
Kaltara_1 = Kaltimtara1 %>% filter(str_detect(polygon_name, pattern = "Tarakan"))
Kaltara_2 = Kaltimtara1 %>% filter(str_detect(polygon_name, pattern = "Malinau"))
Kaltara_3 = Kaltimtara1 %>% filter(str_detect(polygon_name, pattern = "Bulungan"))
Kaltara_4 = Kaltimtara1 %>% filter(str_detect(polygon_name, pattern = "Tana Tidung"))
Kaltara_5 = Kaltimtara1 %>% filter(str_detect(polygon_name, pattern = "Nunukan"))
Kaltara1 = rbind(Kaltara_1, Kaltara_2, Kaltara_3, Kaltara_4, Kaltara_5)
Kaltim1 = rbind(Kaltimtara1 %>% filter(str_detect(polygon_name, pattern = "Balikpapan")),Kaltimtara1 %>% filter(str_detect(polygon_name, pattern = "Paser")),Kaltimtara1 %>% filter(str_detect(polygon_name, pattern = "Samarinda")),Kaltimtara1 %>% filter(str_detect(polygon_name, pattern = "Berau")),Kaltimtara1 %>% filter(str_detect(polygon_name, pattern = "Bontang")),Kaltimtara1 %>% filter(str_detect(polygon_name, pattern = "Kutai")))
Kep_Riau1 = idn %>% filter(str_detect(polygon_id, pattern = "IDN.16\\. *"))
Lampung1 = idn %>% filter(str_detect(polygon_id, pattern = "IDN.17\\. *"))
Malut1 = idn %>% filter(str_detect(polygon_id, pattern = "IDN.18\\. *"))
Maluku1 = idn %>% filter(str_detect(polygon_id, pattern = "IDN.19\\. *"))
NTB1 = idn %>% filter(str_detect(polygon_id, pattern = "IDN.20\\. *"))
NTT1 = idn %>% filter(str_detect(polygon_id, pattern = "IDN.21\\. *"))
Papbar1 = idn %>% filter(str_detect(polygon_id, pattern = "IDN.22\\. *"))
Papua1 = idn %>% filter(str_detect(polygon_id, pattern = "IDN.23\\. *"))
Riau1 = idn %>% filter(str_detect(polygon_id, pattern = "IDN.24\\. *"))
Sulbar1 = idn %>% filter(str_detect(polygon_id, pattern = "IDN.25\\. *"))
Sulsel1 = idn %>% filter(str_detect(polygon_id, pattern = "IDN.26\\. *"))
Sulteng1 = idn %>% filter(str_detect(polygon_id, pattern = "IDN.27\\. *"))
Sultra1 = idn %>% filter(str_detect(polygon_id, pattern = "IDN.28\\. *"))
Sulut1 = idn %>% filter(str_detect(polygon_id, pattern = "IDN.29\\. *"))
Sumbar1 = idn %>% filter(str_detect(polygon_id, pattern = "IDN.30\\. *"))
Sumsel1 = idn %>% filter(str_detect(polygon_id, pattern = "IDN.31\\. *"))
Sumut1 = idn %>% filter(str_detect(polygon_id, pattern = "IDN.32\\. *"))
DIY1 = idn %>% filter(str_detect(polygon_id, pattern = "IDN.33\\. *"))

list_daerah = list(Aceh1, Bali1, Babel1,Banten1,Bengkulu1,Gorontalo1,Jakarta1,Jambi1,Jabar1,Jateng1,Jatim1,Kalbar1,Kalsel1,
                   Kalteng1,Kaltara1,Kaltim1,Kep_Riau1,Lampung1,Malut1,Maluku1,NTB1,NTT1,Papbar1,Papua1,Riau1,Sulbar1,
                   Sulsel1,Sulteng1,Sultra1,Sulut1,Sumbar1,Sumsel1,Sumut1,DIY1)

list_clean = list()
for(i in 1:length(list_daerah)){
  dum = data.frame(list_daerah[i])
  change = dum %>% group_by(ds) %>% summarise(median(all_day_bing_tiles_visited_relative_change))
  proportion = dum %>% group_by(ds) %>% summarise(median(all_day_ratio_single_tile_users))
  full = data.frame(proportion,change)
  full = full %>% select(-ds.1)
  full = full[order(as.Date(full$ds, format="%Y-%m-%d")),]
  for(j in 1:nrow(full)){
    full[j,2] = (1 - full[j,2])*full[j,3]
  }
  full = full %>% select(-median.all_day_bing_tiles_visited_relative_change.)
  list_clean[[i]] = full
}

list_daerah_clean = list(Aceh = 0, Bali = 0, Babel = 0,Banten = 0,Bengkulu = 0,Gorontalo = 0,Jakarta = 0,Jambi = 0,Jabar = 0,Jateng = 0,Jatim = 0,Kalbar = 0,Kalsel = 0,
                         Kalteng = 0,Kaltara = 0,Kaltim = 0,Kep_Riau = 0,Lampung = 0,Malut = 0,Maluku = 0,NTB = 0,NTT = 0,Papbar = 0,Papua = 0,Riau = 0,Sulbar = 0,
                         Sulsel = 0,Sulteng = 0,Sultra = 0,Sulut = 0,Sumbar = 0,Sumsel = 0,Sumut = 0,DIY = 0)
list_nama = c('Aceh', 'Bali', 'Babel','Banten','Bengkulu','Gorontalo','Jakarta','Jambi','Jabar','Jateng','Jatim','Kalbar','Kalsel',
              'Kalteng','Kaltara','Kaltim','Kep Riau','Lampung','Malut','Maluku','NTB','NTT','Papbar','Papua','Riau','Sulbar',
              'Sulsel','Sulteng','Sultra','Sulut','Sumbar','Sumsel','Sumut','DIY')

for(i in 1:length(list_daerah_clean)){
  list_daerah_clean[[i]] = list_clean[[i]]
  colnames(list_daerah_clean[[i]])[2] = list_nama[i]
}

df_daerah = merge(list_daerah_clean[[1]],list_daerah_clean[[2]], by = "ds")
for(i in 3:length(list_daerah_clean)){
  dum3 = data.frame(list_daerah_clean[[i]])
  df_daerah = merge(df_daerah, dum3, by = "ds")
}

df_daerah$ds = as.Date(df_daerah$ds, format="%Y-%m-%d")

mob_idx = df_daerah[,2:35]
mob_idx[,]=0
for(i in 1:nrow(mob_idx)){
  for(j in 1:ncol(mob_idx)){
    mob_idx[i,j] = (df_daerah[i,j+1] - min(df_daerah[i,2:35]))/(max(df_daerah[i,2:35])-min(df_daerah[i,2:35]))
  }
}

df_prop = df_daerah

colnames(df_prop)[1] = "Tanggal"
colnames(df_prop)[18] = "Kep Riau"

df_daerah = data.frame(df_daerah[,1],mob_idx)
colnames(df_daerah)[1] = "Tanggal"
colnames(df_daerah)[18] = "Kep Riau"

df_mob = df_daerah %>% gather('Aceh', 'Bali', 'Babel','Banten','Bengkulu','Gorontalo','Jakarta','Jambi','Jabar','Jateng','Jatim','Kalbar','Kalsel',
                              'Kalteng','Kaltara','Kaltim','Kep Riau','Lampung','Malut','Maluku','NTB','NTT','Papbar','Papua','Riau','Sulbar',
                              'Sulsel','Sulteng','Sultra','Sulut','Sumbar','Sumsel','Sumut','DIY',key='Provinsi',value='Mobility Index')

df_prop = df_prop %>% gather('Aceh', 'Bali', 'Babel','Banten','Bengkulu','Gorontalo','Jakarta','Jambi','Jabar','Jateng','Jatim','Kalbar','Kalsel',
                             'Kalteng','Kaltara','Kaltim','Kep Riau','Lampung','Malut','Maluku','NTB','NTT','Papbar','Papua','Riau','Sulbar',
                             'Sulsel','Sulteng','Sultra','Sulut','Sumbar','Sumsel','Sumut','DIY',key='Provinsi',value='Mobility Index')

#COVID CLEANING
covid_timeline$Tanggal = as.Date(covid_timeline$Tanggal, format="%d-%b-%y")
covid_infected$Tanggal = as.Date(covid_infected$Tanggal, format="%d-%b-%y")

df_cov = covid_timeline %>% gather('Aceh', 'Bali', 'Babel','Banten','Bengkulu','Gorontalo','Jakarta','Jambi','Jabar','Jateng','Jatim','Kalbar','Kalsel',
                                   'Kalteng','Kaltara','Kaltim','Kep Riau','Lampung','Malut','Maluku','NTB','NTT','Papbar','Papua','Riau','Sulbar',
                                   'Sulsel','Sulteng','Sultra','Sulut','Sumbar','Sumsel','Sumut','DIY',key='Provinsi',value='COVID Index')
df_inf = covid_infected %>% gather('Aceh', 'Bali', 'Babel','Banten','Bengkulu','Gorontalo','Jakarta','Jambi','Jabar','Jateng','Jatim','Kalbar','Kalsel',
                                   'Kalteng','Kaltara','Kaltim','Kep Riau','Lampung','Malut','Maluku','NTB','NTT','Papbar','Papua','Riau','Sulbar',
                                   'Sulsel','Sulteng','Sultra','Sulut','Sumbar','Sumsel','Sumut','DIY',key='Provinsi',value='Infected Population')

#MERGING DATASETS
df_merged = merge(df_mob, df_cov, all = T)
df_merged = merge(df_merged,df_inf, all =T)

df_merged_prop = merge(df_prop, df_cov, all =T)
df_merged_prop = merge(df_merged_prop, df_inf, all =T)

#UPDATING
nrows_mob = nrow(df_prop)
last_date = df_prop[nrows_mob,1]
cut_row = which(df_merged_prop$Tanggal == last_date)
cut_row = cut_row[length(cut_row)]
df_update1 = df_merged_prop[1:cut_row,]
df_update2 = df_merged[1:cut_row,]

sheet_write(df_update1, 'https://docs.google.com/spreadsheets/d/15HrZEZGTjsH_xF8aKBTyPcqKeVpZcZB7eJ7s_-uK38I/edit', sheet = "df_update1")

sheet_write(df_update2, 'https://docs.google.com/spreadsheets/d/15HrZEZGTjsH_xF8aKBTyPcqKeVpZcZB7eJ7s_-uK38I/edit', sheet = "df_update2")
