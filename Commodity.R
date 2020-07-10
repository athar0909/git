library(dplyr)
library(tidyr)
library(stringr)
library(gsheet)
library(googlesheets4)
library(naniar)
library(sqldf)
library(rvest)
library(stringr)

#Load Data
Inflasi_MoM = gsheet2tbl('https://docs.google.com/spreadsheets/d/1A24dHKcpmf0LlBM_8WBn64QpK3NUoUXKQF2P0BcQxHY/edit#gid=1584659367')
i_MoM = Inflasi_MoM
colnames(i_MoM)[1] = "Tanggal"
i_MoM$Tanggal = as.Date(i_MoM$Tanggal, format="%d-%b-%y")
Inflasi_YoY = gsheet2tbl('https://docs.google.com/spreadsheets/d/1A24dHKcpmf0LlBM_8WBn64QpK3NUoUXKQF2P0BcQxHY/edit#gid=891423050')
i_YoY = Inflasi_YoY
colnames(i_YoY)[1] = "Tanggal"
i_YoY$Tanggal = as.Date(i_YoY$Tanggal, format="%d-%b-%y")

com_MoM = colnames(i_MoM)
com_MoM = com_MoM[2:length(com_MoM)]
com_YoY = colnames(i_YoY)
com_YoY = com_YoY[2:length(com_YoY)]

i_MoM_true = i_MoM %>% select(-`Indeks Harga Sembako`)
i_YoY_true = i_YoY %>% select(-`Indeks Harga Sembako`)
com_MoM = colnames(i_MoM_true)
com_MoM = com_MoM[2:length(com_MoM)]
com_YoY = colnames(i_YoY_true)
com_YoY = com_YoY[2:length(com_YoY)]
i_MoM_true = i_MoM_true[nrow(i_MoM_true),]
i_YoY_true = i_YoY_true[nrow(i_YoY_true),]
i_MoM_true = i_MoM_true %>% gather(all_of(com_MoM),key='Commodity',value='Inflation Rate')
i_YoY_true = i_YoY_true %>% gather(all_of(com_YoY),key='Commodity',value='Inflation Rate')


list_sheets = list(i_MoM, i_YoY, i_MoM_true, i_YoY_true)
sheet_names = c("Inflasi MoM", "Inflasi YoY", "Modified MoM", "Modified YoY")


for(i in 1:length(list_sheets)){
  sheet_write(list_sheets[[i]],'https://docs.google.com/spreadsheets/d/15HrZEZGTjsH_xF8aKBTyPcqKeVpZcZB7eJ7s_-uK38I/edit', sheet = sheet_names[i])
}
