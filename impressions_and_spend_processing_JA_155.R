rawcsv<-read.csv("C:\\Users\\SyedFar\\Desktop\\HVL_JA_155\\hvl_JA_2020.csv")
raw<- rawcsv
raw$WK_START_DATE<-as.POSIXct(as.character(raw$WK_START_DATE),format = "%m/%d/%Y")


theDataType <- sapply(raw, class)
sum(theDataType == "numeric")
sum(theDataType == "logical")
logCols <- names(theDataType[theDataType == "logical"])
logCols

sapply(raw[,logCols], function(x) sum(is.na(x)))
raw[logCols] <- sapply(raw[logCols],as.numeric)

theDataType <- sapply(raw, class)
sum(theDataType == "numeric")
sum(theDataType == "logical")
logCols <- names(theDataType[theDataType == "logical"])
logCols

#####variables to be used#####
x= read.xlsx("C:\\Users\\SyedFar\\Desktop\\HVL_JA_155\\HVL Model Dataset - RMD Refresh - JA 2020 V2_IMPRESSION.xlsx",sheet = "variables")

raw<- raw[,x$variable]

####read the mapping file that has variables to be used####

raw0 <- raw



#####Transpose the dataset#### 
raw1 <-gather(raw0,"variable", "value",-WK_START_DATE,-MODEL,-DMA_NAME)

####filter only variables we need####



####filter MODEL not equal to Brand Mazda####

raw2 =raw1%>% filter(MODEL != "Brand Mazda")

####read the mapping file that has channel mapping###
mappingrawdata1<-readxl::read_xlsx("C:\\Users\\SyedFar\\Desktop\\HVL_JA_155\\HVL Model Dataset - RMD Refresh - JA 2020 V2_IMPRESSION.xlsx",sheet = "mapping1")
raw3 = merge(raw2, mappingrawdata1, by = "variable", all.x = TRUE)

####read the mapping file that has weight #####
mappingrawdata2<-readxl::read_xlsx("C:\\Users\\SyedFar\\Desktop\\HVL_JA_155\\HVL Model Dataset - RMD Refresh - JA 2020 V2_IMPRESSION.xlsx",sheet = "mapping2")
raw4 = merge(raw3, mappingrawdata2, by = "DMA_NAME", all.x = TRUE)

####read the mapping file that has rules to operate on rows####
mappingrawdata3<-readxl::read_xlsx("C:\\Users\\SyedFar\\Desktop\\HVL_JA_155\\HVL Model Dataset - RMD Refresh - JA 2020 V2_IMPRESSION.xlsx",sheet = "mapping")
raw5 = merge(raw4, mappingrawdata3, by = "variable", all.x = TRUE)

####operation####
raw6 <- raw5%>%
  group_by(WK_START_DATE,variable,MODEL,rule,channel,DMA_NAME)%>%
  mutate(value1 = if(rule == "add"){value*1
  }else if(rule == "weight") {value * percent
  }else if (rule == "divide") {(value)/8
  } else if(rule == "divide_and_weight") {(value* percent)/8})


raw6$value1[is.na(raw6$value1)] <-0

####Add month variable to the dataset#####



####summarize data by carline,by month, by varible####
raw7 <- raw6%>%
  group_by(DMA_NAME,channel,WK_START_DATE,variable)%>%
  summarise(sum(value1))
write.csv(raw7,"impressions155JA_by_market.csv")



colnames(raw7)[which(names(raw7) == "sum(value1)")] <- "value1"

merge1<-data.table(raw7)
nm1 <- grep("^value1", colnames(merge1), value=TRUE)
nm2 <- paste("lag", nm1, sep=".")
raw9<-merge1[, (nm2) :=  shift(.SD,n=10), by=.(merge1$variable,merge1$DMA_NAME), .SDcols=nm1]

write.csv(raw9,"impressions155JA_by_MARKET_NEW_lagged.csv")

###Adstocking####


theData<-  read.csv("C:/Users/SyedFar/Desktop/MMM Tool/Rong iterations/espndata_linear_social.csv", header=TRUE, sep=",")
adstock<-function(x, r) {
  tempX<-rep(NA,length(x))
  x[is.na(x)]<-0
  for(i in 1:length(x)){
    if (i==1) tempX[i] <- x[i]*r else tempX[i] <- x[i]*r + (1-r) * tempX[i-1]
  }
  return (tempX)
}


cnames <- names(theData)
cnames <- cnames[-1]

mAdstock1<-function(df, var, r=c(.1,.2,.3,.4,.5,.6,.7,.8,.9)) {
  for (v in var) {
    for (i in r){
      df[,paste0(v,i*100)] <- adstock(df[,v], i)
    }
  }
  return(df)
}

adtocked_df <- mAdstock1(theData, cnames)

write.csv(adtocked_df,"AdstockedforBrandopinion154_OND.csv")
write.csv(adtocked_df,"BO_Adstocked.csv")
write.csv(adtocked_df,"espn_combined_variable_adstocked.csv")
write.csv(adtocked_df,"espn_linear_Social_variable_adstocked.csv")



####Spend processing######

raw<-read.xlsx("C:/Users/SyedFar/Desktop/HVL JFM2020/HVL Model Dataset - RMD Refresh - JFM 2020 V1Spend.xlsx",sheet="HVL Model Dataset - RMD Refresh")
raw$WK_START_DATE <-convertToDate(raw$WK_START_DATE)
theDataType <- sapply(raw, class)
sum(theDataType == "numeric")
sum(theDataType == "logical")
logCols <- names(theDataType[theDataType == "logical"])
logCols

sapply(raw[,logCols], function(x) sum(is.na(x)))
raw[logCols] <- sapply(raw[logCols],as.numeric)

theDataType <- sapply(raw, class)
sum(theDataType == "numeric")
sum(theDataType == "logical")
logCols <- names(theDataType[theDataType == "logical"])
logCols

#####variables to be used#####
x =c(
     
)

####read the mapping file that has variables to be used####
y<-names(raw) %in% x
raw0 <- raw[y]



#####Transpose the dataset#### 
raw1 <-gather(raw0,"variable", "value",-WK_START_DATE,-MODEL,-DMA_NAME)

####filter only variables we need####



####filter MODEL not equal to Brand Mazda####

raw2 =raw1%>% filter(MODEL != "Brand Mazda")

####read the mapping file that has channel mapping###
mappingrawdata1<-readxl::read_xlsx("C:/Users/SyedFar/Desktop/HVL JFM2020/HVL Model Dataset - RMD Refresh - JFM 2020 V1Spend.xlsx",sheet = "mapping1")
raw3 = merge(raw2, mappingrawdata1, by = "variable", all.x = TRUE)

####read the mapping file that has weight #####
mappingrawdata2<-readxl::read_xlsx("C:/Users/SyedFar/Desktop/HVL JFM2020/HVL Model Dataset - RMD Refresh - JFM 2020 V1Spend.xlsx",sheet = "mapping2")
raw4 = merge(raw3, mappingrawdata2, by = "DMA_NAME", all.x = TRUE)

####read the mapping file that has rules to operate on rows####
mappingrawdata3<-readxl::read_xlsx("C:/Users/SyedFar/Desktop/HVL JFM2020/HVL Model Dataset - RMD Refresh - JFM 2020 V1Spend.xlsx",sheet = "mapping")
raw5 = merge(raw4, mappingrawdata3, by = "variable", all.x = TRUE)

####operation####
raw6 <- raw5%>%
  group_by(WK_START_DATE,variable,MODEL,rule,channel,DMA_NAME)%>%
  mutate(value1 = if(rule == "add"){value*1
  }else if(rule == "weight") {value * percent
  }else if (rule == "divide") {(value)/8
  } else if(rule == "divide_and_weight") {(value* percent)/8})



raw6$value1[is.na(raw6$value1)] <-0

raw7 <- raw6%>%
  group_by(MODEL,channel,WK_START_DATE,variable)%>%
  summarise(sum(value1))

raw8 <-read.xlsx("C:/Users/SyedFar/Desktop/raw11.xlsx",sheet = "raw11")



merge1<-data.table(raw8)
nm1 <- grep("^value1", colnames(merge1), value=TRUE)
nm2 <- paste("lag", nm1, sep=".")
raw9<-merge1[, (nm2) :=  shift(.SD,n=10), by=.(merge1$variable,merge1$MODEL), .SDcols=nm1]

write.csv(raw9,"raw12.csv")


####Add month variable to the dataset#####
plaform <- c("facebook","instagram","sem","Snapchat","Twitter","yt", "Dma tv")


raw9<- raw8%>%
  filter(channel %in% plaform)

raw10<- raw9%>%
  filter(WK_START_DATE >="2018-03-31")

raw10$month <- floor_date(raw10$WK_START_DATE,"month")

write.csv(raw10,"spendbymarketandcarlineforselectdates.csv")


raw8$month <- floor_date(raw8$WK_START_DATE,"month")

channel_filters <- c("cinema","Audio","image","national tv","OOH","Print","radio","video","Dma tv")
Market_filter <- c("San Diego","Chicago","Dallas-Ft. Worth","Los Angeles","Miami-Ft. Lauderdale","New York","Houston","San Antonio","Orlando-Daytona Bch-Melbrn","Austin","Boston (Manchester)","Philadelphia","Seattle-Tacoma","Washington, DC (Hagrstwn)")
#Subset data based on channel#
raw9<-raw8%>%
  filter(!channel %in% channel_filters )%>%
  filter(!DMA_NAME%in% Market_filter)%>%
  select(-value,-percent,-rule)

raw10<-aggregate(raw9$value1,raw9$lag.value1,by= list(raw9$variable,raw9$DMA_NAME,raw9$MODEL,raw9$channel,raw9$month),FUN= sum )
####summarize data by carline,by month, by varible####
raw9 <- raw8%>%
  group_by(DMA_NAME,MODEL,channel,month,variable,lag.value1)%>%
  summarise(sum(value1))
write.csv(raw9,"Spend154JFM_V1_by_marketandcarline_V6.csv")
