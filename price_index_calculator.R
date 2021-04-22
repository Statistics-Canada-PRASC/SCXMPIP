laspeyres_index<-function(base_year){
    
    # Laspeyres price index calculated as the share-weighted arithmetic average of price relatives

    data<-read.csv("Valid_raw_data.csv",fill = TRUE,header = TRUE)

    latest_entry<-head(data[order(-data$YEAR,-data$MONTH),],1)
    total=0;

    # Splitting up the data into months of the year

    for(year in as.numeric(base_year:latest_entry$YEAR)){
        last_month=ifelse(year==latest_entry$YEAR,latest_entry$MONTH,12)
        for(month in as.numeric(1:last_month)){
            total=total+1;
            data[,paste(year,month,sep="_")]<-ifelse(data$YEAR==year & data$MONTH==month,as.numeric(data$LOG_UNIT_VALUE),NA)
        }
    }

    # Custom carryforward Function

    na.locf <- function(x) {
    v <- !is.na(x)
    c(NA, x[v])[cumsum(v)+1]
    }

    # Aggregating the data and taking the median of each period for each HS6 code

    aggregate_data<-aggregate(
    .~HS+TRADE_TYPE+PARTNER_CODE,
    data=data[ , !(names(data) %in% c("MONTH","YEAR","VALUE","WEIGHT","QUANTITY","UNIT_OF_MEASURE","UNIT_VALUE","LOG_UNIT_VALUE"))],
    FUN=box_plot_detection, 
    na.action=NULL)
    median_data<-cbind(aggregate_data[,1:3],t(data.frame(apply(aggregate_data[,4:ncol(aggregate_data)],1,na.locf))))

    # Calculating the price relatives
    ratio_median_data<-median_data

    for(i in as.numeric(5:(ncol(median_data)))){
            ratio_median_data[,i]<-median_data[,i]/median_data[,(i-1)]
    }

    # Setting up the first period of the index

    ratio_median_data[,4]<-NA

    colnames(ratio_median_data)<-colnames(aggregate_data)

    # Calucating the base year weight

    Weight_Calculator <- function(x){

    table<-data[which(
        trimws(x["HS"])== data$HS & 
        data$YEAR==base_year &
        x["TRADE_TYPE"]==data$TRADE_TYPE &
        x["PARTNER_CODE"]==data$PARTNER_CODE
        ),]
    return(sum(table$VALUE))
    }

    # Applying the base year weight function

    ratio_median_data<-cbind(as.data.frame(apply(ratio_median_data,1,Weight_Calculator)),ratio_median_data)
    ratio_median_data[,1]<-ratio_median_data[,1]/sum(ratio_median_data[,1])
    colnames(ratio_median_data)[1]<-"BASE_YEAR_WEIGHT"
    # Multiplying the log unit values by the base year weight

    ratio_median_data[,6:ncol(ratio_median_data)]<-ratio_median_data[,6:ncol(ratio_median_data)]*ratio_median_data[,1]
    # Adding the Direct Index
    empty<-data.frame(matrix(ncol = 4, nrow = 1))
    empty[,4]<-"Direct Index"
    colnames(empty)<-c("BASE_YEAR_WEIGHT","HS","TRADE_TYPE","PARTNER_CODE")

    result<-rbind(
        ratio_median_data,
        cbind(empty,t(colSums(ratio_median_data[,5:ncol(ratio_median_data)])*100)))

    # Setting up the base year

    result[nrow(result),5]<-100
    
    write.csv(
        result,
        "Laspeyres_Index_Result.csv",
        row.names=FALSE,
    )

}

paasche_index<-function(base_year){
    
    # Paasche price index calculated as the share-weighted harmonic average of price relatives

    data<-read.csv("Valid_raw_data.csv",fill = TRUE,header = TRUE)

    latest_entry<-head(data[order(-data$YEAR,-data$MONTH),],1)
    total=0;

    # Splitting up the data into months of the year
    value_proportion<-data

    for(year in as.numeric(base_year:latest_entry$YEAR)){
        last_month=ifelse(year==latest_entry$YEAR,latest_entry$MONTH,12)
        for(month in as.numeric(1:last_month)){
            total=total+1;
            data[,paste(year,month,sep="_")]<-ifelse(data$YEAR==year & data$MONTH==month,as.numeric(data$LOG_UNIT_VALUE),NA)
        }
    }
    
    for(year in as.numeric(base_year:latest_entry$YEAR)){
        last_month=ifelse(year==latest_entry$YEAR,latest_entry$MONTH,12)
        for(month in as.numeric(1:last_month)){
            total=total+1;
            value_proportion[,paste(year,month,sep="_")]<-ifelse(data$YEAR==year & data$MONTH==month,as.numeric(data$VALUE),NA)
        }
    }
    
    # Custom carryforward Function

    na.locf <- function(x) {
    v <- !is.na(x)
    c(NA, x[v])[cumsum(v)+1]
    }

    # Aggregating the data and taking the median of each period for each HS6 code

    aggregate_data<-aggregate(
    .~HS+TRADE_TYPE+PARTNER_CODE,
    data=data[ , !(names(data) %in% c("MONTH","YEAR","VALUE","WEIGHT","QUANTITY","UNIT_OF_MEASURE","UNIT_VALUE","LOG_UNIT_VALUE"))],
    FUN=box_plot_detection, 
    na.action=NULL)

    value_proportion<-aggregate(
    .~HS+TRADE_TYPE+PARTNER_CODE,
    data=value_proportion[ , !(names(value_proportion) %in% c("MONTH","YEAR","VALUE","WEIGHT","QUANTITY","UNIT_OF_MEASURE","UNIT_VALUE","LOG_UNIT_VALUE"))],
    FUN=sum, 
    na.rm=TRUE,
    na.action=NULL)

    value_proportion<-sweep(value_proportion[,4:ncol(value_proportion)],2,colSums(value_proportion[,4:ncol(value_proportion)]),'/')
    value_proportion[value_proportion==0]<-NA
    median_data<-cbind(aggregate_data[,1:3],t(data.frame(apply(aggregate_data[,4:ncol(aggregate_data)],1,na.locf))))

    # Calculating the price relatives
    ratio_median_data<-median_data

    for(i in as.numeric(5:(ncol(median_data)))){
        ratio_median_data[,i]<-median_data[,i]/median_data[,(i-1)]
    }

    # Setting up the first period of the index

    ratio_median_data[,4]<-NA

    colnames(ratio_median_data)<-colnames(aggregate_data)
    ratio_median_data[,4:ncol(ratio_median_data)]<-value_proportion/ratio_median_data[,4:ncol(ratio_median_data)]
    ratio_median_data[is.na(ratio_median_data)]<-0

    # Adding the direct index
    empty<-data.frame(matrix(ncol = 3, nrow = 1))
    empty[,3]<-"Direct Index"
    colnames(empty)<-c("HS","TRADE_TYPE","PARTNER_CODE")

    result<-rbind(
        ratio_median_data,
        cbind(empty,t(100/colSums(ratio_median_data[,4:ncol(ratio_median_data)]))))

    # Setting up the base year
    result[,4]<-NA
    result[nrow(result),4]<-100
    
    write.csv(
        result,
        "Paasche_Index_Result.csv",
        row.names=FALSE,
    )

}
