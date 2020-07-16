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
library(gdata)
library(tm)
library(zoo)

link_gsheet = c("https://docs.google.com/spreadsheets/d/15HrZEZGTjsH_xF8aKBTyPcqKeVpZcZB7eJ7s_-uK38I/edit#gid=212868428",
                "https://docs.google.com/spreadsheets/d/15HrZEZGTjsH_xF8aKBTyPcqKeVpZcZB7eJ7s_-uK38I/edit#gid=1508349049",
                "https://docs.google.com/spreadsheets/d/15HrZEZGTjsH_xF8aKBTyPcqKeVpZcZB7eJ7s_-uK38I/edit#gid=1298779528",
                "https://docs.google.com/spreadsheets/d/15HrZEZGTjsH_xF8aKBTyPcqKeVpZcZB7eJ7s_-uK38I/edit#gid=1938163881",
                "https://docs.google.com/spreadsheets/d/15HrZEZGTjsH_xF8aKBTyPcqKeVpZcZB7eJ7s_-uK38I/edit#gid=1513818155",
                "https://docs.google.com/spreadsheets/d/15HrZEZGTjsH_xF8aKBTyPcqKeVpZcZB7eJ7s_-uK38I/edit#gid=1552795188",
                "https://docs.google.com/spreadsheets/d/15HrZEZGTjsH_xF8aKBTyPcqKeVpZcZB7eJ7s_-uK38I/edit#gid=1970822658",
                "https://docs.google.com/spreadsheets/d/15HrZEZGTjsH_xF8aKBTyPcqKeVpZcZB7eJ7s_-uK38I/edit#gid=1695416944",
                "https://docs.google.com/spreadsheets/d/15HrZEZGTjsH_xF8aKBTyPcqKeVpZcZB7eJ7s_-uK38I/edit#gid=1047038282",
                "https://docs.google.com/spreadsheets/d/15HrZEZGTjsH_xF8aKBTyPcqKeVpZcZB7eJ7s_-uK38I/edit#gid=873514690")

url <- read_html("https://hargapangan.id")
date_loc = xml_attrs(xml_child(xml_child(xml_child(xml_child(xml_child(
           xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(
           xml_child(xml_child(xml_child(xml_child(url, 2), 1), 2), 1),
           1), 1), 1), 1), 1), 2), 2), 4), 2), 1), 2))
latest_date = as.character(date_loc)
latest_date = as.Date(latest_date[5], format = "%d-%m-%Y")
categories = as.character(c(1:10))
list_data = list(Beras=0,Ayam=0,Sapi=0,Telor=0,Bmerah=0,Bputih=0,Cmerah=0,Crawit=0,Minyak=0,Gula=0)
nama_sheet = c("Beras", "Ayam", "Sapi","Telor","Bawang merah","Bawang putih","Cabe merah","Cabe rawit","Minyak","Gula")

for(i in 1:length(categories)){
  com = gsheet2tbl(link_gsheet[i])
  temp = tempfile()
  link = paste('https://hargapangan.id/tabel-harga/pasar-tradisional/komoditas/default/?province_id=0',
               '&commodity_id=cat-',categories[i],'&date=',latest_date,'&format=xls&is_external=1',sep = '')
  download.file(link,temp)
  df = read.xls(temp)
  penting = as.character(df[,1])
  cut = which(df[,2] == "Provinsi")
  df_food = df[cut:nrow(df),]
  df_food = data.frame(t(df_food))
  rownames(df_food) = NULL
  colnames(df_food) = penting[4:length(penting)]
  df_food = df_food[-1,]
  df_food = df_food %>% select(No.,II,III,IV,V,VI,VII,VIII,IX,X,XI,XII,XIII,XIV,XV,XVI,XVII,XVIII,XIX,XX,XXI,XXII,
                               XXIII,XXIV,XXV,XXVI,XXVII,XXVIII,XXIX,XXX,XXXI,XXXII,XXXIII,XXXIV,XXXV)
  df_food = data.frame(df_food)
  for(j in 1:ncol(df_food)){
    df_food[,j] = as.character(df_food[,j])
  }
  colnames(df_food) = df_food[1,]
  df_food = df_food[-1,]
  df_try = df_food
  for(j in 2:ncol(df_try)){
    df_try[,j] = removeWords(df_try[,j], c("Rp ", ","))
  }
  for(j in 2:ncol(df_try)){
    df_try[,j] = as.numeric(df_try[,j])
  }
  
  for(k in 2:ncol(df_try)){
    for(j in 2:nrow(df_try)){
      if(is.na(df_try[j,k]) == TRUE){
        df_try[j,k] = df_try[j-1,k]
      }
    }
  }
  df_try[,1] = as.Date(df_try[,1], format = "%d/%m/%Y")
  colnames(df_try)[1] = "Tanggal"
  
  last_date = as.character(data.frame(com[nrow(com),1])[1,1])
  last_date_update = which(df_try$Tanggal == last_date)
  
  if(last_date == df_try[nrow(df_try),1]){
    combine = com
  } else{
    combine = bind_rows(com,df_try[(last_date_update+1):nrow(df_try),])
  }
  list_data[[i]] = combine
}

Sys.sleep(60)

for(i in 1:length(list_data)){
  sheet_write(list_data[[i]],"https://docs.google.com/spreadsheets/d/15HrZEZGTjsH_xF8aKBTyPcqKeVpZcZB7eJ7s_-uK38I/edit", sheet = nama_sheet[i] )
  Sys.sleep(15)
}

