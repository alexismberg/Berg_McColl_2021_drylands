################################
#.libPaths("/n/holystore01/LABS/mccoll_lab/Lab/aberg///R/x86_64-redhat-linux-gnu-library/3.2")
library(ncdf, lib="/n/holystore01/LABS/mccoll_lab/Lab/aberg///R/x86_64-redhat-linux-gnu-library/3.2")
library(colorRamps, lib="/n/holystore01/LABS/mccoll_lab/Lab/aberg///R/x86_64-redhat-linux-gnu-library/3.2")
library(maps)#, lib="/n/holystore01/LABS/mccoll_lab/Lab/aberg///R/x86_64-redhat-linux-gnu-library/3.2")
library(ncdf4)#, lib="/n/holystore01/LABS/mccoll_lab/Lab/aberg///R/x86_64-redhat-linux-gnu-library/3.2")
library(fields)#, lib="/n/holystore01/LABS/mccoll_lab/Lab/aberg///R/x86_64-redhat-linux-gnu-library/3.2")
library(akima)#, lib="/n/holystore01/LABS/mccoll_lab/Lab/aberg///R/x86_64-redhat-linux-gnu-library/3.2")
#library(LSD)#, lib="/n/holystore01/LABS/mccoll_lab/Lab/aberg///R/x86_64-redhat-linux-gnu-library/3.2")
library(RColorBrewer, lib.loc="/n/holystore01/LABS/mccoll_lab/Lab/aberg///R/x86_64-redhat-linux-gnu-library/3.2")

## lpj-guess, gfdl_esm2m
load("save_RData/trans_gfdl_esm2m_caraib_historical_histsoc_co2_year.RData")
load("save_RData/soilmoist_gfdl_esm2m_caraib_historical_histsoc_co2_year.RData")
corr_sm_tran_gfdl_esm2m_caraib_historical_histsoc_co2 <- array(NA, dim=dim(soilmoist_gfdl_esm2m_caraib_historical_histsoc_co2_year)[1:2])
for (i in 1:720){print(i)
for (j in 1:360){
if (is.na(trans_gfdl_esm2m_caraib_historical_histsoc_co2_year[i,j,1])==F) {
corr_sm_tran_gfdl_esm2m_caraib_historical_histsoc_co2[i,j] <- cor(trans_gfdl_esm2m_caraib_historical_histsoc_co2_year[i,j,111:140],
soilmoist_gfdl_esm2m_caraib_historical_histsoc_co2_year[i,j,111:140], use="complete.obs") }}}
rm(trans_gfdl_esm2m_caraib_historical_histsoc_co2_year); rm(soilmoist_gfdl_esm2m_caraib_historical_histsoc_co2_year)
load("save_RData/trans_gfdl_esm2m_caraib_rcp60_2005soc_co2_year.RData")
load("save_RData/soilmoist_gfdl_esm2m_caraib_rcp60_2005soc_co2_year.RData")
corr_sm_tran_gfdl_esm2m_caraib_rcp60_2005soc_co2 <- array(NA, dim=dim(soilmoist_gfdl_esm2m_caraib_rcp60_2005soc_co2_year)[1:2])
for (i in 1:720){print(i)
for (j in 1:360){
if (is.na(trans_gfdl_esm2m_caraib_rcp60_2005soc_co2_year[i,j,1])==F) {
corr_sm_tran_gfdl_esm2m_caraib_rcp60_2005soc_co2[i,j] <- cor(trans_gfdl_esm2m_caraib_rcp60_2005soc_co2_year[i,j,65:94],
soilmoist_gfdl_esm2m_caraib_rcp60_2005soc_co2_year[i,j,65:94], use="complete.obs") }}}
rm(trans_gfdl_esm2m_caraib_rcp60_2005soc_co2_year); rm(soilmoist_gfdl_esm2m_caraib_rcp60_2005soc_co2_year)
load("save_RData/trans_gfdl_esm2m_caraib_rcp60_2005soc_2005co2_year.RData")
load("save_RData/soilmoist_gfdl_esm2m_caraib_rcp60_2005soc_2005co2_year.RData")
corr_sm_tran_gfdl_esm2m_caraib_rcp60_2005soc_2005co2 <- array(NA, dim=dim(soilmoist_gfdl_esm2m_caraib_rcp60_2005soc_2005co2_year)[1:2])
for (i in 1:720){print(i)
for (j in 1:360){
if (is.na(trans_gfdl_esm2m_caraib_rcp60_2005soc_2005co2_year[i,j,1])==F) {
corr_sm_tran_gfdl_esm2m_caraib_rcp60_2005soc_2005co2[i,j] <- cor(trans_gfdl_esm2m_caraib_rcp60_2005soc_2005co2_year[i,j,65:94],
soilmoist_gfdl_esm2m_caraib_rcp60_2005soc_2005co2_year[i,j,65:94], use="complete.obs") }}}
rm(trans_gfdl_esm2m_caraib_rcp60_2005soc_2005co2_year); rm(soilmoist_gfdl_esm2m_caraib_rcp60_2005soc_2005co2_year)

## lpj-guess, hadgem2_es
load("save_RData/trans_hadgem2_es_caraib_historical_histsoc_co2_year.RData")
load("save_RData/soilmoist_hadgem2_es_caraib_historical_histsoc_co2_year.RData")
corr_sm_tran_hadgem2_es_caraib_historical_histsoc_co2 <- array(NA, dim=dim(soilmoist_hadgem2_es_caraib_historical_histsoc_co2_year)[1:2])
for (i in 1:720){print(i)
for (j in 1:360){
if (is.na(trans_hadgem2_es_caraib_historical_histsoc_co2_year[i,j,1])==F) {
corr_sm_tran_hadgem2_es_caraib_historical_histsoc_co2[i,j] <- cor(trans_hadgem2_es_caraib_historical_histsoc_co2_year[i,j,111:140],
soilmoist_hadgem2_es_caraib_historical_histsoc_co2_year[i,j,111:140], use="complete.obs") }}}
rm(trans_hadgem2_es_caraib_historical_histsoc_co2_year); rm(soilmoist_hadgem2_es_caraib_historical_histsoc_co2_year)
## Corr, future with CO2:
load("save_RData/trans_hadgem2_es_caraib_rcp60_2005soc_co2_year.RData")
load("save_RData/soilmoist_hadgem2_es_caraib_rcp60_2005soc_co2_year.RData")
corr_sm_tran_hadgem2_es_caraib_rcp60_2005soc_co2 <- array(NA, dim=dim(soilmoist_hadgem2_es_caraib_rcp60_2005soc_co2_year)[1:2])
for (i in 1:720){print(i)
for (j in 1:360){
if (is.na(trans_hadgem2_es_caraib_rcp60_2005soc_co2_year[i,j,1])==F) {
corr_sm_tran_hadgem2_es_caraib_rcp60_2005soc_co2[i,j] <- cor(trans_hadgem2_es_caraib_rcp60_2005soc_co2_year[i,j,65:94],
soilmoist_hadgem2_es_caraib_rcp60_2005soc_co2_year[i,j,65:94], use="complete.obs") }}}
rm(trans_hadgem2_es_caraib_rcp60_2005soc_co2_year); rm(soilmoist_hadgem2_es_caraib_rcp60_2005soc_co2_year)
load("save_RData/trans_hadgem2_es_caraib_rcp60_2005soc_2005co2_year.RData")
load("save_RData/soilmoist_hadgem2_es_caraib_rcp60_2005soc_2005co2_year.RData")
corr_sm_tran_hadgem2_es_caraib_rcp60_2005soc_2005co2 <- array(NA, dim=dim(soilmoist_hadgem2_es_caraib_rcp60_2005soc_2005co2_year)[1:2])
for (i in 1:720){print(i)
for (j in 1:360){
if (is.na(trans_hadgem2_es_caraib_rcp60_2005soc_2005co2_year[i,j,1])==F) {
corr_sm_tran_hadgem2_es_caraib_rcp60_2005soc_2005co2[i,j] <- cor(trans_hadgem2_es_caraib_rcp60_2005soc_2005co2_year[i,j,65:94],
soilmoist_hadgem2_es_caraib_rcp60_2005soc_2005co2_year[i,j,65:94], use="complete.obs") }}}
rm(trans_hadgem2_es_caraib_rcp60_2005soc_2005co2_year); rm(soilmoist_hadgem2_es_caraib_rcp60_2005soc_2005co2_year)

## lpj-guess, ipsl_cm5a_lr
load("save_RData/trans_ipsl_cm5a_lr_caraib_historical_histsoc_co2_year.RData")
load("save_RData/soilmoist_ipsl_cm5a_lr_caraib_historical_histsoc_co2_year.RData")
corr_sm_tran_ipsl_cm5a_lr_caraib_historical_histsoc_co2 <- array(NA, dim=dim(soilmoist_ipsl_cm5a_lr_caraib_historical_histsoc_co2_year)[1:2])
for (i in 1:720){print(i)
for (j in 1:360){
if (is.na(trans_ipsl_cm5a_lr_caraib_historical_histsoc_co2_year[i,j,1])==F) {
corr_sm_tran_ipsl_cm5a_lr_caraib_historical_histsoc_co2[i,j] <- cor(trans_ipsl_cm5a_lr_caraib_historical_histsoc_co2_year[i,j,111:140],
soilmoist_ipsl_cm5a_lr_caraib_historical_histsoc_co2_year[i,j,111:140], use="complete.obs") }}}
rm(trans_ipsl_cm5a_lr_caraib_historical_histsoc_co2_year); rm(soilmoist_ipsl_cm5a_lr_caraib_historical_histsoc_co2_year)
load("save_RData/trans_ipsl_cm5a_lr_caraib_rcp60_2005soc_co2_year.RData")
load("save_RData/soilmoist_ipsl_cm5a_lr_caraib_rcp60_2005soc_co2_year.RData")
corr_sm_tran_ipsl_cm5a_lr_caraib_rcp60_2005soc_co2 <- array(NA, dim=dim(soilmoist_ipsl_cm5a_lr_caraib_rcp60_2005soc_co2_year)[1:2])
for (i in 1:720){print(i)
for (j in 1:360){
if (is.na(trans_ipsl_cm5a_lr_caraib_rcp60_2005soc_co2_year[i,j,1])==F) {
corr_sm_tran_ipsl_cm5a_lr_caraib_rcp60_2005soc_co2[i,j] <- cor(trans_ipsl_cm5a_lr_caraib_rcp60_2005soc_co2_year[i,j,65:94],
soilmoist_ipsl_cm5a_lr_caraib_rcp60_2005soc_co2_year[i,j,65:94], use="complete.obs") }}}
rm(trans_ipsl_cm5a_lr_caraib_rcp60_2005soc_co2_year); rm(soilmoist_ipsl_cm5a_lr_caraib_rcp60_2005soc_co2_year)
load("save_RData/trans_ipsl_cm5a_lr_caraib_rcp60_2005soc_2005co2_year.RData")
load("save_RData/soilmoist_ipsl_cm5a_lr_caraib_rcp60_2005soc_2005co2_year.RData")
corr_sm_tran_ipsl_cm5a_lr_caraib_rcp60_2005soc_2005co2 <- array(NA, dim=dim(soilmoist_ipsl_cm5a_lr_caraib_rcp60_2005soc_2005co2_year)[1:2])
for (i in 1:720){print(i)
for (j in 1:360){
if (is.na(trans_ipsl_cm5a_lr_caraib_rcp60_2005soc_2005co2_year[i,j,1])==F) {
corr_sm_tran_ipsl_cm5a_lr_caraib_rcp60_2005soc_2005co2[i,j] <- cor(trans_ipsl_cm5a_lr_caraib_rcp60_2005soc_2005co2_year[i,j,65:94],
soilmoist_ipsl_cm5a_lr_caraib_rcp60_2005soc_2005co2_year[i,j,65:94], use="complete.obs") }}}
rm(trans_ipsl_cm5a_lr_caraib_rcp60_2005soc_2005co2_year); rm(soilmoist_ipsl_cm5a_lr_caraib_rcp60_2005soc_2005co2_year)

## lpj-guess, miroc5
load("save_RData/trans_miroc5_caraib_historical_histsoc_co2_year.RData")
load("save_RData/soilmoist_miroc5_caraib_historical_histsoc_co2_year.RData")
corr_sm_tran_miroc5_caraib_historical_histsoc_co2 <- array(NA, dim=dim(soilmoist_miroc5_caraib_historical_histsoc_co2_year)[1:2])
for (i in 1:720){print(i)
for (j in 1:360){
if (is.na(trans_miroc5_caraib_historical_histsoc_co2_year[i,j,1])==F) {
corr_sm_tran_miroc5_caraib_historical_histsoc_co2[i,j] <- cor(trans_miroc5_caraib_historical_histsoc_co2_year[i,j,111:140],
soilmoist_miroc5_caraib_historical_histsoc_co2_year[i,j,111:140], use="complete.obs") }}}
rm(trans_miroc5_caraib_historical_histsoc_co2_year); rm(soilmoist_miroc5_caraib_historical_histsoc_co2_year)
load("save_RData/trans_miroc5_caraib_rcp60_2005soc_co2_year.RData")
load("save_RData/soilmoist_miroc5_caraib_rcp60_2005soc_co2_year.RData")
corr_sm_tran_miroc5_caraib_rcp60_2005soc_co2 <- array(NA, dim=dim(soilmoist_miroc5_caraib_rcp60_2005soc_co2_year)[1:2])
for (i in 1:720){print(i)
for (j in 1:360){
if (is.na(trans_miroc5_caraib_rcp60_2005soc_co2_year[i,j,1])==F) {
corr_sm_tran_miroc5_caraib_rcp60_2005soc_co2[i,j] <- cor(trans_miroc5_caraib_rcp60_2005soc_co2_year[i,j,65:94],
soilmoist_miroc5_caraib_rcp60_2005soc_co2_year[i,j,65:94], use="complete.obs") }}}
rm(trans_miroc5_caraib_rcp60_2005soc_co2_year); rm(soilmoist_miroc5_caraib_rcp60_2005soc_co2_year)
load("save_RData/trans_miroc5_caraib_rcp60_2005soc_2005co2_year.RData")
load("save_RData/soilmoist_miroc5_caraib_rcp60_2005soc_2005co2_year.RData")
corr_sm_tran_miroc5_caraib_rcp60_2005soc_2005co2 <- array(NA, dim=dim(soilmoist_miroc5_caraib_rcp60_2005soc_2005co2_year)[1:2])
for (i in 1:720){print(i)
for (j in 1:360){
if (is.na(trans_miroc5_caraib_rcp60_2005soc_2005co2_year[i,j,1])==F) {
corr_sm_tran_miroc5_caraib_rcp60_2005soc_2005co2[i,j] <- cor(trans_miroc5_caraib_rcp60_2005soc_2005co2_year[i,j,65:94],
soilmoist_miroc5_caraib_rcp60_2005soc_2005co2_year[i,j,65:94], use="complete.obs") }}}
rm(trans_miroc5_caraib_rcp60_2005soc_2005co2_year); rm(soilmoist_miroc5_caraib_rcp60_2005soc_2005co2_year)


