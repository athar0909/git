library(dplyr)
library(tidyr)
library(stringr)
library(gsheet)
library(googlesheets4)
library(naniar)
library(sqldf)
library(rvest)
library(stringr)
library(tibble)

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
list_clean2 = list()
for(i in 1:length(list_daerah)){
  dum = data.frame(list_daerah[i])
  change1 = data.frame(dum[,1], dum[,5:6])
  still1 = data.frame(dum[,1], dum[,5], dum[,7])
  colnames(change1)[1] = "ds"
  colnames(change1)[2] = "prov"
  colnames(change1)[3] = "change"
  colnames(still1)[1] = "ds"
  colnames(still1)[2] = "prov"
  colnames(still1)[3] = "still"
  change1$date = as.Date(change1$ds, format="%Y-%m-%d")
  still1$date = as.Date(still1$ds, format="%Y-%m-%d")
  change = pivot_wider(change1, id_cols = ds, names_from = prov, values_from = change)
  still = pivot_wider(still1, id_cols = ds, names_from = prov, values_from = still)
  list_clean[[i]] = change
  list_clean2[[i]] = still
}

penduduk = gsheet2tbl('https://docs.google.com/spreadsheets/d/180aeRoUNaSIFl2MKAu0G9V0j0jNJ7DVIt3glRNywUP4/edit#gid=1042545656')
list_daerah2 = list()
list_nama = c('Aceh', 'Bali', 'Babel','Banten','Bengkulu','Gorontalo','Jakarta','Jambi','Jabar','Jateng','Jatim','Kalbar','Kalsel',
              'Kalteng','Kaltara','Kaltim','Kepri','Lampung','Malut','Maluku','NTB','NTT','Papbar','Papua','Riau','Sulbar',
              'Sulsel','Sulteng','Sultra','Sulut','Sumbar','Sumsel','Sumut','DIY')

for(i in 1:length(list_nama)){
  list_daerah2[[i]] = penduduk %>% filter(str_detect(Provinsi, pattern = list_nama[i]))
}
list_daerah_clean = list()

for(k in 1:length(list_daerah2)){
  df = list_daerah2[[k]]
  cek = pivot_wider(df, id_cols = Provinsi, names_from = Kota, values_from = Persentase)
  colnames(cek)[1] = "ds"
  c_kop = list_clean[[k]]
  s_kop = list_clean2[[k]]
  c_cut_kop = ncol(c_kop)
  c_kop1 = bind_rows(c_kop, cek)
  s_kop1 = bind_rows(s_kop,cek)
  c_z = data.frame(c_kop1[,1:c_cut_kop])
  s_z = data.frame(s_kop1[,1:c_cut_kop])
  tot = 0
  for(i in 1:(nrow(c_z)-1)){
    checking1 = any(is.na(c_z[i,2:ncol(c_z)]))
    if(checking1 == FALSE){
      tot[i] =  sum(c_z[nrow(c_z),2:ncol(c_z)])
    } else{
      checking2 = data.frame(which(is.na(c_z[i,1:ncol(c_z)]),arr.ind = T))
      minus = 0
      for(j in 1:nrow(checking2)){
        minus = minus + c_z[nrow(c_z),checking2[j,2]]
      }
      tot[i] = sum(c_z[nrow(c_z),2:ncol(c_z)]) - minus
    }
  }
  
  for(i in 2:ncol(c_z)){
    c_z[1:(nrow(c_z)-1), i]  =  c_z[1:(nrow(c_z)-1), i]*c_z[nrow(c_z),i]
    s_z[1:(nrow(s_z)-1), i]  =  s_z[1:(nrow(s_z)-1), i]*s_z[nrow(s_z),i]
  }
  c_wav = c_z[-nrow(c_z),]
  s_wav = s_z[-nrow(s_z),]
  for(i in 1:nrow(c_wav)){
    c_wav[i,2] = sum(c_wav[i,2:ncol(c_wav)],na.rm = TRUE)/tot[i]
    s_wav[i,2] = sum(s_wav[i,2:ncol(s_wav)],na.rm = TRUE)/tot[i]
  }
  colnames(c_wav)[2] = "change"
  colnames(s_wav)[2] = "still"
  c_wave = c_wav %>% select(c(ds,colnames(c_wav)[2]))
  s_wave = s_wav %>% select(c(ds,colnames(s_wav)[2]))
  full = data.frame(s_wave,c_wave)
  full = full %>% select(-ds.1)
  for(j in 1:nrow(full)){
    full[j,2] = (1 - full[j,2])*full[j,3]
  }
  full = full %>% select(c(colnames(full)[1],colnames(full)[2]))
  colnames(full)[2] = list_nama[k]
  list_daerah_clean[[k]] = full
}

df_daerah = merge(list_daerah_clean[[1]],list_daerah_clean[[2]], by = "ds")
for(i in 3:length(list_daerah_clean)){
  dum3 = data.frame(list_daerah_clean[[i]])
  df_daerah = merge(df_daerah, dum3, by = "ds", all = T)
}
df_daerah$ds = as.Date(df_daerah$ds, format="%Y-%m-%d")
colnames(df_daerah)[1] = "Tanggal"
colnames(df_daerah)[18] = "Kep Riau"

df_mob = df_daerah %>% gather('Aceh', 'Bali', 'Babel','Banten','Bengkulu','Gorontalo','Jakarta','Jambi','Jabar','Jateng','Jatim','Kalbar','Kalsel',
                              'Kalteng','Kaltara','Kaltim','Kep Riau','Lampung','Malut','Maluku','NTB','NTT','Papbar','Papua','Riau','Sulbar',
                              'Sulsel','Sulteng','Sultra','Sulut','Sumbar','Sumsel','Sumut','DIY',key='Provinsi',value='Mobility Index')

#COVID-19 INDEX
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

#UPDATING
nrows_mob = nrow(df_mob)
last_date = df_mob[nrows_mob,1]
cut_row = which(df_merged$Tanggal == last_date)
cut_row = cut_row[length(cut_row)]
df_update = df_merged[1:cut_row,]
base_date = which(df_merged$Tanggal == "2020-06-04")
cut_base = base_date[1]
last_base = nrow(df_merged)
df_base = df_merged[cut_base:last_base,]
df_base_mob = df_base[,1:3]
df_true_mob = pivot_wider(df_base_mob, id_cols = Tanggal, names_from = Provinsi, values_from = `Mobility Index`)
df_base_cov = df_base %>% select(c(-`Mobility Index`,-`Infected Population`))
df_true_cov = pivot_wider(df_base_cov, id_cols = Tanggal, names_from = Provinsi, values_from = `COVID Index`)
df_true_cov[,2:35] <- mutate_all(df_true_cov[,2:35], function(x) as.numeric(as.character(x)))
base_data_mob = df_true_mob[1,]
base_data_cov = df_true_cov[1,]

for(i in 1:nrow(df_true_mob)){
  df_true_mob[i,2:35] = df_true_mob[i,2:35] - base_data_mob[1,2:35]
}

for(i in 1:nrow(df_true_cov)){
  df_true_cov[i,2:35] = base_data_cov[1,2:35] - df_true_cov[i,2:35]
}

df_mob_true = df_true_mob %>% gather('Aceh', 'Bali', 'Babel','Banten','Bengkulu','Gorontalo','Jakarta','Jambi','Jabar','Jateng','Jatim','Kalbar','Kalsel',
                                     'Kalteng','Kaltara','Kaltim','Kep Riau','Lampung','Malut','Maluku','NTB','NTT','Papbar','Papua','Riau','Sulbar',
                                     'Sulsel','Sulteng','Sultra','Sulut','Sumbar','Sumsel','Sumut','DIY',key='Provinsi',value='Mobility Index')
  
df_cov_true = df_true_cov %>% gather('Aceh', 'Bali', 'Babel','Banten','Bengkulu','Gorontalo','Jakarta','Jambi','Jabar','Jateng','Jatim','Kalbar','Kalsel',
                                     'Kalteng','Kaltara','Kaltim','Kep Riau','Lampung','Malut','Maluku','NTB','NTT','Papbar','Papua','Riau','Sulbar',
                                     'Sulsel','Sulteng','Sultra','Sulut','Sumbar','Sumsel','Sumut','DIY',key='Provinsi',value='COVID Index')

df_true = merge(df_mob_true, df_cov_true, all = T)
df_inf_true = df_base %>% select(c(-`Mobility Index`,-`COVID Index`))
df_true = merge(df_true, df_inf_true, all =T)
df_true[is.na(df_true)] <- 0

update_last_cov = df_merged[nrow(df_merged),1]
cut_latest_cov = which(df_merged$Tanggal == update_last_cov)
cut_latest_cov = cut_latest_cov[1]
df_latest_cov = df_merged[cut_latest_cov:last_base,]
df_latest_cov = df_latest_cov %>% select(c(-`Mobility Index`,-`Infected Population`))

update_last_mob = which(df_merged$Tanggal == last_date)
update_low_mob = update_last_mob[1]
update_high_mob = update_last_mob[length(update_last_mob)]
df_latest_mob = df_merged[update_low_mob:update_high_mob,]
df_latest_mob = df_latest_mob[,1:3]

agg_pend = gsheet2tbl('https://docs.google.com/spreadsheets/d/180aeRoUNaSIFl2MKAu0G9V0j0jNJ7DVIt3glRNywUP4/edit#gid=1070410237')
agg_pend = agg_pend %>% select(-`Jumlah Penduduk`)
agg_cek = pivot_wider(agg_pend, names_from = Provinsi, values_from = Persentase)
agg_cek = agg_cek %>% add_column(new_col = NA, .before = "Aceh")
colnames(agg_cek)[1] = "Tanggal"
df_agg = bind_rows(df_daerah, agg_cek)
for(i in 2:ncol(df_agg)){
  df_agg[1:(nrow(df_agg)-1), i]  =  df_agg[1:(nrow(df_agg)-1), i]*df_agg[nrow(df_agg),i]
}
df_agg = df_agg[-nrow(df_agg),]
for(j in 1:nrow(df_agg)){
  df_agg[j,2] = sum(df_agg[j,2:ncol(df_agg)])
}
df_agg = df_agg[,1:2]
colnames(df_agg)[2] = "Aggregate"

sheet_write(df_true, 'https://docs.google.com/spreadsheets/d/15HrZEZGTjsH_xF8aKBTyPcqKeVpZcZB7eJ7s_-uK38I/edit', sheet = "df_update_new")

sheet_write(df_latest_cov, 'https://docs.google.com/spreadsheets/d/15HrZEZGTjsH_xF8aKBTyPcqKeVpZcZB7eJ7s_-uK38I/edit',sheet = "raw cov")

sheet_write(df_latest_mob, 'https://docs.google.com/spreadsheets/d/15HrZEZGTjsH_xF8aKBTyPcqKeVpZcZB7eJ7s_-uK38I/edit',sheet = "raw mob")

sheet_write(df_agg, 'https://docs.google.com/spreadsheets/d/15HrZEZGTjsH_xF8aKBTyPcqKeVpZcZB7eJ7s_-uK38I/edit',sheet = "Agregat Mobility")

