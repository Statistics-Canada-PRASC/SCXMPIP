
source("functions.R")

raw_data_input<-function(
  positions,
  multiple_files=FALSE,
  header=TRUE,
  base_year,
  quaterly_monthly,
  imports_code,
  exports_code) {

  month<-positions[1]
  year<-positions[2]
  trade_type<-positions[3]
  hs<-positions[4]
  partner_code<-positions[5]
  value<-positions[6]
  weight<-positions[7]
  unit_of_measure<-positions[8]
  quantity<-positions[9]

  if(is.na(unit_of_measure)){
    num_columns=8
  }else{
    num_columns=9
  }

  # Raw Data retrieval

  if(multiple_files){

    # Mulptiple files is currently only available to windows OS

    if(.Platform$OS.type == "windows"){

    raw_data<-data.frame()
    folder<-choose.dir()

    # Using a for loop to retrieve each file in the folder provided

    for(temp_file in list.files(folder,full.names = TRUE)){

      raw_data<-rbind(raw_data,retrieve_data(temp_file,header,num_columns))

    }

    }else{

      stop("You can only choose mutliple files when you're running this on a windows OS")
    }

  }else{

    file<-file.choose()

    raw_data<-retrieve_data(file,header,num_columns)

  }

  # Checking if the unit of measure column is included

  if(is.na(unit_of_measure)){
  raw_data<-raw_data[,c(month,year,trade_type,hs,partner_code,value,weight,quantity)]
  }else{
  raw_data<-raw_data[,c(month,year,trade_type,hs,partner_code,value,weight,unit_of_measure,quantity)]
  }

  if(is.na(unit_of_measure)){
    colnames(raw_data)<-c("MONTH","YEAR","TRADE_TYPE","HS","PARTNER_CODE","VALUE","WEIGHT","QUANTITY")
    }else{
    colnames(raw_data)<-c("MONTH","YEAR","TRADE_TYPE","HS","PARTNER_CODE","VALUE","WEIGHT","UNIT_OF_MEASURE","QUANTITY")
    }

  

  # Validation of raw data

  # Taking the first 6 digits of the HS code

  raw_data$HS<-substring(raw_data$HS,1,6)

  # HS code validation based on the file attached ("Copy of UN Comtrade Commodity Classfications-3128.csv")

  hs_codes<-read.csv("validation documents/UN Comtrade HS6 list.csv",fill = TRUE,header = TRUE)

  codes<-hs_codes[,c("Code")]

  # All invalid items are written to the "Invalid.csv" file
 
  invalid<-raw_data[raw_data$HS %notin% codes,]

  write.csv(invalid,"reports/Invalid.csv",row.names=FALSE)

  raw_data<-raw_data[raw_data$HS %in% codes,]

  raw_data$OBSERVATIONS<-ifelse(raw_data$YEAR==base_year,1,0)

  # Aggregating the data based on the HS6 code, the trade type, the partner country and the year

  aggregate_data<-aggregate(
    cbind(VALUE=as.numeric(VALUE),QUANTITY=as.numeric(QUANTITY),WEIGHT=as.numeric(WEIGHT),OBSERVATIONS)
    ~HS+TRADE_TYPE+PARTNER_CODE+YEAR,
    data=raw_data,
    sum)

  # Removing any aggregate with less than 30 observations

  aggregate_data<-aggregate_data[aggregate_data$OBSERVATIONS>=30,]
  
  # Quantity and Value checker

  valid_quantity<-ifelse(
    is.numeric(raw_data$QUANTITY) & raw_data$QUANTITY>0, 
    raw_data$QUANTITY,
    NA
    )

  # Calculating the unit value

  raw_data$UNIT_VALUE<-ifelse(
    !is.na(valid_quantity),
    as.numeric(raw_data$VALUE)/as.numeric(raw_data$WEIGHT),
    as.numeric(raw_data$VALUE)/as.numeric(raw_data$QUANTITY)
  )

  # Using a log transformation on the unit value

  raw_data$LOG_UNIT_VALUE<-log(raw_data$UNIT_VALUE)

  Outlier_detector <- function(x){

    #Looking at the aggregate data and choosing the corresponding commodities

    table<-raw_data[which(
        x["HS"] == raw_data$HS & 
        x["TRADE_TYPE"] == raw_data$TRADE_TYPE & 
        x["PARTNER_CODE"] == raw_data$PARTNER_CODE &
        x["YEAR"]==base_year
        ),]

    # Finding the first quartile, the median and the third quartile

    quant<-as.data.frame(quantile(table$LOG_UNIT_VALUE, probs = seq(0, 1, 0.25), na.rm = TRUE,))

    q1<-quant[2,]
    median<-quant[3,]
    q3<-quant[4,]

    # Calculating the RIQ, IQR and the loqer/upper threshold

    RIQ<-(q3-q1)/median

    IQR<-q3-q1

    lower_threshold<-q1-1.5*IQR
    upper_threshold<-q3+1.5*IQR

    table$UPPER=upper_threshold
    table$LOWER=lower_threshold

    total_value=sum(as.numeric(table$VALUE))
    
    outlier_value=sum(as.numeric(table[which(table$LOG_UNIT_VALUE <=lower_threshold | table$LOG_UNIT_VALUE>=upper_threshold),]$VALUE))

    # Removing any outliers where the outlier value represents more than 10% of the total value or the RIQ is above 2

    Remove_aggregate=ifelse(
      (outlier_value/total_value)>0.1 |
      RIQ>2,
      FALSE,
      TRUE)

    return(Remove_aggregate)
  }

  # Applying the function above to the aggregate data

  aggregate_data<-aggregate_data[which(apply(aggregate_data, 1, Outlier_detector)),]

  # Filtering the raw_data based on the aggregate data

  Filter_raw_data <- function(x) {
      if(dim(aggregate_data[which(
        aggregate_data$HS == x["HS"] & 
        aggregate_data$TRADE_TYPE == x["TRADE_TYPE"] & 
        aggregate_data$PARTNER_CODE == x["PARTNER_CODE"]
        ) , ])[1]==0){
        return(FALSE)
      }else{
        return(TRUE)
      }
    }

  # Applying the function above to the raw data

  raw_data<-raw_data[which(apply(raw_data,1,Filter_raw_data)),]

  # Writing the raw data to "Valid_raw_data.csv"

  write.csv(raw_data[ , !(names(raw_data) %in% c("OBSERVATIONS"))],"Valid_raw_data.csv",row.names=FALSE)

  # Calling the write report function in functions.R

  write_report(raw_data,imports_code,exports_code,quaterly_monthly)

}

  # Add data function to append any data to the valid_raw_data.csv file (outlier detection is run again with the new added data)

add_data<-function(positions,header){

  month<-positions[1]
  year<-positions[2]
  trade_type<-positions[3]
  hs<-positions[4]
  partner_code<-positions[5]
  value<-positions[6]
  weight<-positions[7]
  unit_of_measure<-positions[8]
  quantity<-positions[9]

  # Checking if the UOM column is included

  if(is.na(unit_of_measure)){
    num_columns=8
  }else{
    num_columns=9
  }

  # Fetching the HS6 codes and the valid raw data file

  hs_codes<-read.csv("validation documents/UN Comtrade HS6 list.csv",fill = TRUE,header = TRUE)

  old_data<-read.csv("Valid_raw_data.csv",fill = TRUE,header = TRUE)

  codes<-hs_codes[,c("Code")]

  file<-file.choose()

  new_data<-retrieve_data(file,header,num_columns)

  # Ordering and naming the columns based on whether UOM is included

  if(is.na(unit_of_measure)){
   new_data<-new_data[,c(month,year,trade_type,hs,partner_code,value,weight,quantity)]
  }else{
    new_data<-new_data[,c(month,year,trade_type,hs,partner_code,value,weight,unit_of_measure,quantity)]
  }

  if(is.na(unit_of_measure)){
  colnames(new_data)<-c("MONTH","YEAR","TRADE_TYPE","HS","PARTNER_CODE","VALUE","WEIGHT","QUANTITY")
  }else{
  colnames(new_data)<-c("MONTH","YEAR","TRADE_TYPE","HS","PARTNER_CODE","VALUE","WEIGHT","UNIT_OF_MEASURE","QUANTITY")
  }

  # Removing any invalid HS6 codes

  new_data$HS<-substring(new_data$HS,1,6)

  new_data<-new_data[new_data$HS %in% codes,]

  # Removing any invalid quantity

  valid_quantity<-ifelse(
    is.numeric(new_data$QUANTITY) & new_data$QUANTITY>0, 
    new_data$QUANTITY,
    NA
    )

  # Calculating the log unit value

  new_data$UNIT_VALUE<-ifelse(
    !is.na(valid_quantity),
    as.numeric(new_data$VALUE)/as.numeric(new_data$WEIGHT),
    as.numeric(new_data$VALUE)/as.numeric(new_data$QUANTITY)
  )

  new_data$LOG_UNIT_VALUE<-log(new_data$UNIT_VALUE)

  # Combining the old data with the new formatted data

  complete_data<-rbind(old_data,new_data)

  complete_data$OBSERVATIONS<-ifelse(complete_data$YEAR==base_year,1,0)

  # Removing any aggregate with less than 30 observations

  aggregate_data<-aggregate(
    cbind(VALUE=as.numeric(VALUE),QUANTITY=as.numeric(QUANTITY),WEIGHT=as.numeric(WEIGHT),OBSERVATIONS)
    ~HS+TRADE_TYPE+PARTNER_CODE+YEAR,
    data=complete_data[ , !(names(complete_data) %in% c("UNIT_OF_MEASURE","UNIT_VALUE","LOG_UNIT_VALUE"))],
    sum)

  aggregate_data<-aggregate_data[aggregate_data$OBSERVATIONS>=30,]

  Outlier_detector <- function(x){

    #Looking at the aggregate data and choosing the corresponding commodities

    table<-complete_data[which(
        x["HS"] == complete_data$HS & 
        x["TRADE_TYPE"] == complete_data$TRADE_TYPE & 
        x["PARTNER_CODE"] == complete_data$PARTNER_CODE &
        x["YEAR"]==base_year
        ),]

    # Finding the first quartile, the median and the third quartile

    quant<-as.data.frame(quantile(table$LOG_UNIT_VALUE, probs = seq(0, 1, 0.25), na.rm = TRUE,))

    q1<-quant[2,]
    median<-quant[3,]
    q3<-quant[4,]

    # Calculating the RIQ, IQR and the loqer/upper threshold

    RIQ<-(q3-q1)/median

    IQR<-q3-q1

    lower_threshold<-q1-1.5*IQR
    upper_threshold<-q3+1.5*IQR

    table$UPPER=upper_threshold
    table$LOWER=lower_threshold


    total_value=sum(as.numeric(table$VALUE))
    
    outlier_value=sum(as.numeric(table[which(table$LOG_UNIT_VALUE <=lower_threshold | table$LOG_UNIT_VALUE>=upper_threshold),]$VALUE))
  
    # Removing any outliers where the outlier value represents more than 10% of the total value or the RIQ is above 2

    Remove_aggregate=ifelse(
      (outlier_value/total_value)>0.1 |
      RIQ>2,
      FALSE,
      TRUE)
    return(Remove_aggregate)
  }

  # Applying the function above to the aggregate data

  aggregate_data<-aggregate_data[which(apply(aggregate_data, 1, Outlier_detector)),]

  # Filtering the raw_data based on the aggregate data

  Filter_raw_data <- function(x) {
      if(dim(aggregate_data[which(
        aggregate_data$HS == trimws(x["HS"]) & 
        aggregate_data$TRADE_TYPE == x["TRADE_TYPE"] & 
        aggregate_data$PARTNER_CODE == x["PARTNER_CODE"]
        ) , ])[1]==0){
        return(FALSE)
      }else{
        return(TRUE)
      }
    }

  # Applying the function above to the raw data

  complete_data<-complete_data[which(apply(complete_data,1,Filter_raw_data)),]

  # Writing the complete data to valid_raw_data.csv

  write.table(complete_data[ , !(names(complete_data) %in% c("OBSERVATIONS"))], "Valid_raw_data.csv", sep = ",", row.names = FALSE)

}