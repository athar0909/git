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
library(reshape2)
library(lubridate)
library(zoo)

link_pasar = c("https://docs.google.com/spreadsheets/d/15HrZEZGTjsH_xF8aKBTyPcqKeVpZcZB7eJ7s_-uK38I/edit#gid=479614768",
               "https://docs.google.com/spreadsheets/d/15HrZEZGTjsH_xF8aKBTyPcqKeVpZcZB7eJ7s_-uK38I/edit#gid=1348875238",
               "https://docs.google.com/spreadsheets/d/15HrZEZGTjsH_xF8aKBTyPcqKeVpZcZB7eJ7s_-uK38I/edit#gid=1835954238")

link_pihps = c("https://hargapangan.id/tabel-harga/pasar-tradisional/daerah",
               "https://hargapangan.id/tabel-harga/pasar-modern/daerah",
               "https://hargapangan.id/tabel-harga/pedagang-besar/daerah")

trad = c("Psr_Trad", "IHK Psr_Trad","Inf.MoM_Trad","MoM_Trad last","Inf.YoY_Trad","YoY_Trad last")
mod = c("Psr_mod", "IHK Psr_mod","Inf.MoM_mod","MoM_mod last","Inf.YoY_mod","YoY_mod last")
bsr = c("Pdg_Besar", "IHK Pdg_Besar","Inf.MoM_Bsr","MoM_Bsr last","Inf.YoY_Bsr","YoY_Bsr last")
nama_sheet = list(trad,mod,bsr)
sheet_list = list()

for(k in 1:length(link_pasar)){
  ihk = gsheet2tbl(link_pasar[k])
  ihk = data.frame(ihk)
  url <- read_html(link_pihps[k])
  my_tables <- html_nodes(url,"table")[[1]]
  sembako <- html_table(my_tables)
  sembako = sembako[,-1]
  dates = colnames(sembako)
  sbk = as.data.frame(t(sembako))
  rownames(sbk) = NULL
  colnames(sbk) = sembako[,1]
  sbk = cbind(dates,sbk)
  sbk = sbk[-1,]
  
  for(i in 2:ncol(sbk)){
    sbk[,i] = as.numeric(as.character(sbk[,i]))*1000
  }
  
  sbk$dates = as.Date(sbk$dates, format="%d/%m/%Y")
  sbk = sbk %>% select(dates, Beras, `Daging Ayam`, `Daging Sapi`, `Telur Ayam`, `Bawang Merah`, `Bawang Putih`,
                       `Cabai Merah`, `Cabai Rawit`,`Minyak Goreng`,`Gula Pasir`)
  colnames(ihk) = colnames(sbk)
  last_date = ihk[nrow(ihk),1]
  last_date_sbk = which(sbk$dates == last_date)
  
  if(last_date == sbk[nrow(sbk),1]){
    combine_ihk = ihk
  } else{
    combine_ihk = bind_rows(ihk,sbk[(last_date_sbk+1):nrow(sbk),])
  }
  
  df1.zoo<-zoo(combine_ihk[,-1],combine_ihk[,1]) #set date to Index
  df2 <- merge(df1.zoo,zoo(,seq(start(df1.zoo),end(df1.zoo),by="day")), all=TRUE)
  df2 = data.frame(df2)
  d <- cbind(rownames(df2), data.frame(df2, row.names=NULL))
  colnames(d)[1] = "Tanggal"
  
  
  for(i in 2:nrow(d)){
    if(any(is.na(d[i,2:ncol(d)])) == FALSE){
      d[i,2:ncol(d)] = d[i,2:ncol(d)]
    } else {
      d[i,2:ncol(d)] = d[(i-1),2:ncol(d)]
    }
  }
  
  combine_ihk = d
  colnames(combine_ihk) = colnames(sbk)
  combine_ihk$dates = as.Date(combine_ihk$dates, format="%Y-%m-%d")
  colnames(combine_ihk)[1] = "Tanggal"
  update = combine_ihk
  
  bobot = data.frame(13.3042,6.07,2.41,2.79,1.36,0.88,1.45,0.72,3.4,1.1)
  colnames(bobot) = colnames(combine_ihk)[-1]
  
  calcbine = bind_rows(combine_ihk,bobot)
  
  tot = 0
  for(i in 1:(nrow(calcbine)-1)){
    checking1 = any(is.na(calcbine[i,2:ncol(calcbine)]))
    if(checking1 == FALSE){
      tot[i] =  sum(calcbine[nrow(calcbine),2:ncol(calcbine)])
    } else{
      checking2 = data.frame(which(is.na(calcbine[i,1:ncol(calcbine)]),arr.ind = T))
      minus = 0
      for(j in 1:nrow(checking2)){
        minus = minus + c_z[nrow(calcbine),checking2[j,2]]
      }
      tot[i] = sum(calcbine[nrow(calcbine),2:ncol(calcbine)]) - minus
    }
  }
  
  for(i in 2:ncol(calcbine)){
    calcbine[1:(nrow(calcbine)-1), i]  =  calcbine[1:(nrow(calcbine)-1), i]*calcbine[nrow(calcbine),i]
  }
  
  for(i in 1:nrow(calcbine)-1){
    calcbine[i,2:ncol(calcbine)]  =  calcbine[i,2:ncol(calcbine)]/tot[i]
  }
  
  calcbine = calcbine[-nrow(calcbine),]
  combine_ihk = combine_ihk %>% add_column(new_col = NA, .after = "Gula Pasir")
  colnames(combine_ihk)[ncol(combine_ihk)] = "Total Harga Setelah Bobot"  
  
  for(i in 1:nrow(combine_ihk)){
    combine_ihk$`Total Harga Setelah Bobot`[i] = sum(calcbine[i,2:(ncol(calcbine)-1)])
  }
  
  cpi = data.frame()
  date_cpi = which(combine_ihk$Tanggal == "2020-01-01")
  
  for(i in 1:(nrow(combine_ihk)-date_cpi)){
    cpi[i,1] = combine_ihk[(date_cpi+i),1]
    cpi[i,2] = 100*(combine_ihk[(date_cpi+i),ncol(combine_ihk)]/combine_ihk[(date_cpi+1),ncol(combine_ihk)])
  }
  colnames(cpi)[1] = "Tanggal"
  colnames(cpi)[2] = "CPI"
  cpi_update = cpi
  
  inf_m = data.frame()
  for(i in 1:(nrow(combine_ihk)-30)){
    inf_m[i,1] = combine_ihk[(i+30),1]
    inf_m[i,2:ncol(combine_ihk)] = (combine_ihk[i+30,2:ncol(combine_ihk)] - combine_ihk[i,2:ncol(combine_ihk)])/combine_ihk[i,2:ncol(combine_ihk)]
  }
  colnames(inf_m)[1] = "Tanggal"
  
  base_date_m = which(inf_m$Tanggal == "2020-01-01")
  inf_m_true = inf_m[base_date_m:nrow(inf_m),]
  inf_m_last = inf_m_true[nrow(inf_m_true),1:(ncol(inf_m_true)-1)]
  com_MoM = colnames(inf_m_last)
  com_MoM = com_MoM[2:length(com_MoM)]
  inf_m_last = inf_m_last %>% gather(all_of(com_MoM),key='Commodity',value='Inflation Rate')
  inf_m_last_update = inf_m_last
  inf_m_update = inf_m_true
  
  inf_y = data.frame()
  for(i in 1:(nrow(combine_ihk)-365)){
    inf_y[i,1] = combine_ihk[(i+365),1]
    inf_y[i,2:ncol(combine_ihk)] = (combine_ihk[i+365,2:ncol(combine_ihk)] - combine_ihk[i,2:ncol(combine_ihk)])/combine_ihk[i,2:ncol(combine_ihk)]
  }
  colnames(inf_y)[1] = "Tanggal"
  
  base_date_y = which(inf_y$Tanggal == "2020-01-01")
  inf_y_true = inf_y[base_date_y:nrow(inf_y),]
  inf_y_last = inf_y_true[nrow(inf_y_true),1:(ncol(inf_y_true)-1)]
  com_YoY = colnames(inf_y_last)
  com_YoY = com_YoY[2:length(com_YoY)]
  inf_y_last = inf_y_last %>% gather(all_of(com_YoY),key='Commodity',value='Inflation Rate')
  inf_y_last_update = inf_y_last
  inf_y_update= inf_y_true
  
  sheet_list[[k]] = list(update,cpi_update,inf_m_update,inf_m_last_update,inf_y_update,inf_y_last_update)
  name_sheet = nama_sheet[[k]]
  
  for(i in 1:length(sheet_list[[k]])){
    Sys.sleep(10)
    sheet_write(sheet_list[[k]][[i]],"https://docs.google.com/spreadsheets/d/15HrZEZGTjsH_xF8aKBTyPcqKeVpZcZB7eJ7s_-uK38I/edit", sheet = name_sheet[i] )
  }
  Sys.sleep(10)
}

inf_m_avg = data.frame()
trad_mom = data.frame(sheet_list[[1]][3])
mod_mom = data.frame(sheet_list[[2]][3])
bsr_mom = data.frame(sheet_list[[3]][3])

for(i in 1:nrow(trad_mom)){
  inf_m_avg[i,1] = trad_mom[i,1]
  inf_m_avg[i,2] = (trad_mom[i,12] + mod_mom[i,12] + bsr_mom[i,12])/3
}
colnames(inf_m_avg)[1] = "Tanggal"
colnames(inf_m_avg)[2] = "Average Inflation"

inf_y_avg = data.frame()
trad_yoy = data.frame(sheet_list[[1]][5])
mod_yoy = data.frame(sheet_list[[2]][5])
bsr_yoy = data.frame(sheet_list[[3]][5])

for(i in 1:nrow(trad_yoy)){
  inf_y_avg[i,1] = trad_yoy[i,1]
  inf_y_avg[i,2] = (trad_yoy[i,12] + mod_yoy[i,12] + bsr_yoy[i,12])/3
}
colnames(inf_y_avg)[1] = "Tanggal"
colnames(inf_y_avg)[2] = "Average Inflation YoY"

list_avg = list(inf_m_avg,inf_y_avg)
nama_avg = c("Inf.MoM_Avg", "Inf.YoY_Avg")

Sys.sleep(5)

for(i in 1:length(list_avg)){
  sheet_write(list_avg[[i]],"https://docs.google.com/spreadsheets/d/15HrZEZGTjsH_xF8aKBTyPcqKeVpZcZB7eJ7s_-uK38I/edit", sheet = nama_avg[i])
}



  