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

url <- read_html("https://hargapangan.id")
date_loc = xml_attrs(xml_child(xml_child(xml_child(xml_child(xml_child(
           xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(
           xml_child(xml_child(xml_child(xml_child(url, 2), 1), 2), 1),
           1), 1), 1), 1), 1), 2), 2), 4), 2), 1), 2))
latest_date = as.character(date_loc)
latest_date = as.Date(latest_date[5], format = "%d-%m-%Y")
categories = as.character(c(1:10))
list_data = list(Beras=0,Ayam=0,Sapi=0,Telor=0,Bmerah=0,Bputih=0,Cmerah=0,Crawit=0,Minyak=0,Gula=0)

for(i in 1:length(categories)){
  temp = tempfile()
  link = paste('https://hargapangan.id/tabel-harga/pedagang-besar/komoditas/default/?province_id=0',
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
  df_try[,1] = as.Date(df_try[,1], format = "%d/%m/%Y")
  list_data[[i]] = df_try
}


