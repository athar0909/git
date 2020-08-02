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

#MOBILITY
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

#Data Penduduk
agg_penduduk = gsheet2tbl('https://docs.google.com/spreadsheets/d/180aeRoUNaSIFl2MKAu0G9V0j0jNJ7DVIt3glRNywUP4/edit#gid=1070410237')

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

cut_mob_bub = which(df_daerah$Tanggal == "2020-06-04")
df_mob_bubble = df_daerah[cut_mob_bub:nrow(df_daerah),]
base_mob_bub = df_mob_bubble[1,]
for(i in 1:nrow(df_mob_bubble)){
  df_mob_bubble[i,2:35] = df_mob_bubble[i,2:35] - base_mob_bub[1,2:35]
}


mob_last = df_daerah[nrow(df_daerah),]

agg_pend = agg_penduduk %>% select(-`Jumlah Penduduk`)
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

df_mob = df_mob_bubble %>% gather('Aceh', 'Bali', 'Babel','Banten','Bengkulu','Gorontalo','Jakarta','Jambi','Jabar','Jateng','Jatim','Kalbar','Kalsel',
                                  'Kalteng','Kaltara','Kaltim','Kep Riau','Lampung','Malut','Maluku','NTB','NTT','Papbar','Papua','Riau','Sulbar',
                                  'Sulsel','Sulteng','Sultra','Sulut','Sumbar','Sumsel','Sumut','DIY',key='Provinsi',value='Mobility Index')
  
df_latest_mob = mob_last %>% gather('Aceh', 'Bali', 'Babel','Banten','Bengkulu','Gorontalo','Jakarta','Jambi','Jabar','Jateng','Jatim','Kalbar','Kalsel',
                                    'Kalteng','Kaltara','Kaltim','Kep Riau','Lampung','Malut','Maluku','NTB','NTT','Papbar','Papua','Riau','Sulbar',
                                    'Sulsel','Sulteng','Sultra','Sulut','Sumbar','Sumsel','Sumut','DIY',key='Provinsi',value='Mobility Index')


#COVID INDEX
covid_infected = gsheet2tbl('https://docs.google.com/spreadsheets/d/1Ll0g-MW8YPmwCRcdpElS9EtRNx9YKvschCLEv7CtxMg/edit#gid=0')
covid_infected = covid_infected[,1:35]
colnames(covid_infected)[1] = "Tanggal"

tanggal = gsheet2tbl("https://docs.google.com/spreadsheets/d/1Ll0g-MW8YPmwCRcdpElS9EtRNx9YKvschCLEv7CtxMg/edit#gid=0")
tanggal = tanggal[,1]
colnames(tanggal) = "Tanggal"

aktif = gsheet2tbl("https://docs.google.com/spreadsheets/d/1Ll0g-MW8YPmwCRcdpElS9EtRNx9YKvschCLEv7CtxMg/edit#gid=1233029347")
header = colnames(aktif)[2:35]
aktif = aktif[37:ncol(aktif)]
aktif = aktif[-c(1,2,3),]
colnames(aktif) = header
aktif = aktif %>% add_column(new_col = NA, .before = "Aceh")
aktif[,1] = tanggal[,1]
colnames(aktif)[1] = "Tanggal"
aktif$Tanggal = as.Date(aktif$Tanggal, format="%d-%b-%y")

gr = gsheet2tbl("https://docs.google.com/spreadsheets/d/1Ll0g-MW8YPmwCRcdpElS9EtRNx9YKvschCLEv7CtxMg/edit#gid=1238289")
gr = gr[-1,1:35]
colnames(gr)[2:ncol(gr)] = header
gr$Tanggal = as.Date(gr$Tanggal, format="%d-%b-%y")
gr[,2:35] <- mutate_all(gr[,2:35], function(x) as.numeric(as.character(x)))

covid_death = gsheet2tbl('https://docs.google.com/spreadsheets/d/1Ll0g-MW8YPmwCRcdpElS9EtRNx9YKvschCLEv7CtxMg/edit#gid=1116188425')
covid_death = covid_death[,1:35]
colnames(covid_death)[1] = "Tanggal"
covid_death[is.na(covid_death)] <- 0
cov_death = covid_death
covid_death[,2:35] = 0
for(i in 2:nrow(cov_death)){
  covid_death[i,2:35] = cov_death[i,2:35] - (cov_death[i-1,2:35])
}

indeks_death = data.frame()
for(i in 1:(nrow(covid_death)-7)){
  indeks_death[i,1] = covid_death[i+7,1]
  for(j in 2:ncol(covid_death)){
    indeks_death[i,j] = sum(covid_death[(i+1):(i+7),j])/7
  }
  
}
colnames(indeks_death) = colnames(covid_death)

agg_pend = agg_penduduk %>% select(-`Persentase`)
agg_cek = pivot_wider(agg_pend, names_from = Provinsi, values_from = `Jumlah Penduduk`)

calcdeath = bind_rows(indeks_death, agg_cek)
for(i in 2:ncol(calcdeath)){
  calcdeath[1:(nrow(calcdeath)-1), i]  =  calcdeath[1:(nrow(calcdeath)-1), i]/calcdeath[nrow(calcdeath),i]
}
death = calcdeath[-nrow(calcdeath),]
death$Tanggal = as.Date(death$Tanggal, format="%d-%b-%y")

##INDEXING

base_aktif = data.frame()
base_aktif[1,1] = NA
baseline_date = c("2020-04-01","2020-04-30", "2020-05-01")
ak_low = which(aktif$Tanggal == baseline_date[1])
ak_hi = which(aktif$Tanggal == baseline_date[2])

for(i in 2:ncol(aktif)){
  base_aktif[1,i] = mean(unname(unlist(aktif[ak_low:ak_hi,i])))
}
colnames(base_aktif) = colnames(aktif)

start_ak = which(aktif$Tanggal == baseline_date[3])
idx_aktif = aktif[start_ak:nrow(aktif),]
idx_aktif = as.data.frame(bind_rows(idx_aktif, base_aktif))

for(i in 2:ncol(idx_aktif)){
  idx_aktif[1:(nrow(idx_aktif)-1), i]  =  log1p((idx_aktif[1:(nrow(idx_aktif)-1), i])/idx_aktif[nrow(idx_aktif),i])
}
idx_aktif = idx_aktif[-nrow(idx_aktif),]

base_growth = data.frame()
base_growth[1,1] = NA
gr_low = which(gr$Tanggal == baseline_date[1])
gr_hi = which(gr$Tanggal == baseline_date[2])

for(i in 2:ncol(gr)){
  base_growth[1,i] = mean(unname(unlist(gr[gr_low:gr_hi,i])))
}
colnames(base_growth) = colnames(gr)

start_gr = which(gr$Tanggal == baseline_date[3])
idx_gr = gr[start_gr:nrow(gr),]
idx_gr = as.data.frame(bind_rows(idx_gr, base_growth))

for(i in 2:ncol(idx_gr)){
  idx_gr[1:(nrow(idx_gr)-1), i]  =  (idx_gr[1:(nrow(idx_gr)-1), i] - idx_gr[nrow(idx_gr),i])
}
idx_gr = idx_gr[-nrow(idx_gr),]

base_death = data.frame()
base_death[1,1] = NA
dt_low = which(death$Tanggal == baseline_date[1])
dt_hi = which(death$Tanggal == baseline_date[2])

for(i in 2:ncol(death)){
  base_death[1,i] = mean(unname(unlist(death[dt_low:dt_hi,i])))
}
colnames(base_death) = colnames(death)

for(i in 2:ncol(base_death)){
  if(base_death[1,i] == 0){
    base_death[1,i] = min(death[death[,i] > 0, i])
  }
}

start_dt = which(death$Tanggal == baseline_date[3])
idx_dt = death[start_dt:nrow(death),]

idx_dt = as.data.frame(bind_rows(idx_dt, base_death))

for(i in 2:ncol(idx_dt)){
  idx_dt[1:(nrow(idx_dt)-1), i]  =  log1p(idx_dt[1:(nrow(idx_dt)-1), i]/idx_dt[nrow(idx_dt),i])
}
idx_dt = idx_dt[-nrow(idx_dt),]
idx_dt[is.na(idx_dt)] <- 0

idx_cov = data.frame()
for(i in 1:nrow(idx_aktif)){
  idx_cov[i,1] = idx_aktif[i,1]
  idx_cov[i,2:35] = (idx_aktif[i,2:35] + idx_gr[i,2:35] + idx_dt[i,2:35])/3
}
colnames(idx_cov)[1] = "Tanggal"
idx_cov = idx_cov %>% add_column(new_col = NA, .after = "Tanggal")
colnames(idx_cov)[2] = "National Weighted Average"
agg_pend = agg_penduduk %>% select(-`Jumlah Penduduk`)
agg_cek = pivot_wider(agg_pend, names_from = Provinsi, values_from = Persentase)
idx_cov_new = bind_rows(idx_cov, agg_cek)
for(i in 1:(nrow(idx_cov_new)-1)){
  idx_cov_new[i,2] = sum(idx_cov_new[i,3:ncol(idx_cov_new)]*idx_cov_new[nrow(idx_cov_new),3:ncol(idx_cov_new)])
}
idx_cov_new = idx_cov_new[-nrow(idx_cov_new),]
idx_cov_fixed = idx_cov_new[,-2]
df_cov_agg = idx_cov_new %>% select(c("Tanggal","National Weighted Average"))

cut_bub = which(idx_cov_fixed$Tanggal == "2020-06-04")
idx_cov_bubble = idx_cov_fixed[cut_bub:nrow(idx_cov_fixed),]

base_cov = idx_cov_bubble[1,]
for(i in 1:nrow(idx_cov_bubble)){
  idx_cov_bubble[i,2:35] = base_cov[1,2:35] - idx_cov_bubble[i,2:35]
}

idx_cov_last = idx_cov_fixed[nrow(idx_cov_fixed),]

covid_infected$Tanggal = as.Date(covid_infected$Tanggal, format="%d-%b-%y")
cut_infected = which(covid_infected$Tanggal == "2020-06-04")
cov_inf = covid_infected[cut_infected:nrow(covid_infected),]
df_inf = cov_inf %>% gather('Aceh', 'Bali', 'Babel','Banten','Bengkulu','Gorontalo','Jakarta','Jambi','Jabar','Jateng','Jatim','Kalbar','Kalsel',
                            'Kalteng','Kaltara','Kaltim','Kep Riau','Lampung','Malut','Maluku','NTB','NTT','Papbar','Papua','Riau','Sulbar',
                            'Sulsel','Sulteng','Sultra','Sulut','Sumbar','Sumsel','Sumut','DIY',key='Provinsi',value='Active Cases')

df_cov = idx_cov_bubble %>% gather('Aceh', 'Bali', 'Babel','Banten','Bengkulu','Gorontalo','Jakarta','Jambi','Jabar','Jateng','Jatim','Kalbar','Kalsel',
                                 'Kalteng','Kaltara','Kaltim','Kep Riau','Lampung','Malut','Maluku','NTB','NTT','Papbar','Papua','Riau','Sulbar',
                                 'Sulsel','Sulteng','Sultra','Sulut','Sumbar','Sumsel','Sumut','DIY',key='Provinsi',value='COVID Index')

df_latest_cov = idx_cov_last %>% gather('Aceh', 'Bali', 'Babel','Banten','Bengkulu','Gorontalo','Jakarta','Jambi','Jabar','Jateng','Jatim','Kalbar','Kalsel',
                                        'Kalteng','Kaltara','Kaltim','Kep Riau','Lampung','Malut','Maluku','NTB','NTT','Papbar','Papua','Riau','Sulbar',
                                        'Sulsel','Sulteng','Sultra','Sulut','Sumbar','Sumsel','Sumut','DIY',key='Provinsi',value='COVID Index')

#MERGING
df_merged = merge(df_mob, df_cov, all = T)
df_true = merge(df_merged,df_inf, all =T)
df_true[is.na(df_true)] <- 0

list_sheets = list(df_true, df_latest_cov, df_latest_mob, df_agg, df_cov_agg, idx_cov_fixed, df_daerah)
sheet_names = c("df_update_new", "last cov", "last mob", "Agregat Mobility", "Agregat COVID", "raw cov","raw mob")

Sys.sleep(120)

for(i in 1:length(list_sheets)){
  sheet_write(list_sheets[[i]], 'https://docs.google.com/spreadsheets/d/15HrZEZGTjsH_xF8aKBTyPcqKeVpZcZB7eJ7s_-uK38I/edit', sheet = sheet_names[i])
  Sys.sleep(15)
}


