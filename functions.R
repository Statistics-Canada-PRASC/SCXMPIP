# Creating the "not in" function

`%notin%` <- Negate(`%in%`)

# Retreve data function retrieves data and transforms it to our desired structure.

 retrieve_data <- function(file,header,num_columns) {

    # Checking the extension type of the file, currently only accepting .csv and .txt file

    if(grepl("\\.csv$",file)){

      df_file<-read.csv(
        file,
        fill = TRUE,
        header = header,
        sep = ",",
        colClasses=c(rep("character",num_columns)),
        fileEncoding="UTF-8-BOM",)
        row.names(df_file)<-NULL

    }else if(grepl("\\.txt$",file)){

      df_file<-read.delim(
        file,
        fill = TRUE,
        header = header,
        sep = "" ,
        colClasses=c(rep("character",num_columns)),
        )
        row.names(df_file)<-NULL

    }else{

    # The function is stopped if the file isn't .csv or .txt

      stop("Please only use .csv and .txt files")

    }

    return(df_file)

  }

  #  Report writing function. Creates three different reports in /reports. 

  write_report<-function(data,imports_code,exports_code,quaterly_monthly){

    # Importing the different HS descriptions from /validation documents.

    HS4_descriptions<-read.table("validation documents/HS4 descriptions.txt",sep = "\t",quote = "",fill = TRUE,header = FALSE,colClasses=c(rep("character",2)))
    HS2_descriptions<-read.table("validation documents/HS2 descriptions.txt",sep = "\t",quote = "",fill = TRUE,header = FALSE,colClasses=c(rep("character",2)))

    # Creating the monthly/quarterly columns for the reports

    for(year in sort(unique(data$YEAR),decreasing=FALSE)){
      if(quaterly_monthly==TRUE){
        
        # Creating the value grid layout for quarters

        for(quarter in seq(1,4)){
          if(quarter==1){
            months=c(1,2,3)
          }else if(quarter==2){
            months=c(4,5,6)
          }else if(quarter==3){
            months=c(7,8,9)
          }else{
            months=c(10,11,12)
          }
        data[,paste(year,quarter,sep="_q")]<-ifelse(data$YEAR==year & data$MONTH %in% months,as.numeric(data$VALUE),0)
      }
      }else{

      # Creating the value grid layout for months

      for(month in seq(1,12)){
        data[,paste(year,month,sep="_")]<-ifelse(data$YEAR==year & data$MONTH==month,as.numeric(data$VALUE),0)
      }
      }
    }

    # Function to input HS4 description

    function_HS4 <- function(x) {
      if(dim(HS4_descriptions[which(HS4_descriptions$V1 == x["HS"]), ])[1]==0){
        return("")
      }else{
        return(HS4_descriptions[which(HS4_descriptions$V1 == x["HS"]), ]$V2)
      }
    }

    # Function to input HS2 description

    function_HS2 <- function(x) {
      if(dim(HS2_descriptions[which(HS2_descriptions$V1 == x["HS"]), ])[1]==0){
        return("")
      }else{
        return(HS2_descriptions[which(HS2_descriptions$V1 == x["HS"]), ]$V2)
      }
    }

    # For loop where 1 corresponds to the HS4 report and the value by country report and 2 corresponds to the HS2 report

    for(n in seq(1,2)){

      # Adding descriptions to the reports based on the functions above

      if(n==1){
      data$HS<-substring(data$HS,1,4)
      data$DESCRIPTION<-apply(data,1,function_HS4)
      }else{
      data$HS<-substring(data$HS,1,2)
      data$DESCRIPTION<-apply(data,1,function_HS2)
      }

    # Removing any unecessary Columns

    data<-data[,!names(data) %in% c("MONTH","YEAR","VALUE","WEIGHT","UNIT_OF_MEASURE","QUANTITY","OBSERVATIONS","UNIT_VALUE","LOG_UNIT_VALUE")]

    # Aggregating data based on HS, Trade Type and Partner Code.
    
    data<-aggregate(
    .~HS+TRADE_TYPE+PARTNER_CODE+DESCRIPTION,
    data=data,
    sum)

    # Extracting all the imports from the data

    imports<-data[data$TRADE_TYPE %in% imports_code,]

    if(n==1){

    # Removing unecessary columns
    
    country_imports_value<-imports[ , !(names(imports) %in% c("HS","TRADE_TYPE","DESCRIPTION"))]

    # Agreggating by Partner Code

    country_imports_by_value<-aggregate(
    .~PARTNER_CODE,
    data=country_imports_value,
    sum)

    # Calculating the sum of the rows for each partner code

    Total_imports_by_country<-data.frame(COUNTRY_CODE=country_imports_by_value$PARTNER_CODE,VALUE=rowSums(country_imports_by_value[,!(names(country_imports_by_value) %in% c("PARTNER_CODE"))]))
    
    # Ordering each partner code by value

    Total_imports_by_country<-Total_imports_by_country[order(-Total_imports_by_country$VALUE),]

    # Naming import column and adding a buffer line

    Total_imports_by_country$TRADE_TYPE<-"Imports"
    Total_imports_by_country[nrow(Total_imports_by_country)+1,] <- ""

    }

    # Naming the columns and adding a buffer line for the reports

    imports[nrow(imports)+1,] <- ""
    imports[nrow(imports)+1,] <-names(imports)

    # Extracting all the exports from the data

    exports<-data[data$TRADE_TYPE %in% exports_code,]

    if(n==1){

    # Removing unecessary columns

    country_exports_value<-exports[ , !(names(exports) %in% c("HS","TRADE_TYPE","DESCRIPTION"))]

    # Agreggating by Partner Code

    country_exports_by_value<-aggregate(
    .~PARTNER_CODE,
    data=country_exports_value,
    sum)

    # Calculaating the sum of the rows for each partner code

    Total_exports_by_country<-data.frame(COUNTRY_CODE=country_exports_by_value$PARTNER_CODE,VALUE=rowSums(country_exports_by_value[,!(names(country_exports_by_value) %in% c("PARTNER_CODE"))]))
    
    # Ordering each partner code by value

    Total_exports_by_country<-Total_exports_by_country[order(-Total_exports_by_country$VALUE),]

    # Naming the Exports column and combining both import value by country and export value by country into one dataframe.

    Total_exports_by_country$TRADE_TYPE<-"Exports"
    final_country_by_value<-rbind(Total_imports_by_country,Total_exports_by_country)

    }

    # Combining the imports and exports into one dataframe for the reports

    final<-rbind(imports,exports)

    # Writing all three reports

   if(n==1){
       write.csv(
          final,
          row.names=FALSE,
          "reports/HS4 report.csv",
          fileEncoding = 'UTF-8'
        )
      write.csv(
          final_country_by_value,
          row.names=FALSE,
          "reports/Country by Value report.csv",
          fileEncoding = 'UTF-8'
        )
      }else{
       write.csv(
          final,
          row.names=FALSE,
          "reports/HS2 report.csv",
          fileEncoding = 'UTF-8'
        )
      }
    }
  }

  # Box plot detection function

  box_plot_detection<-function(x){

    # Calculating the first quartile, the median and the third quartile for the given x vector

    quant<-as.data.frame(quantile(x, probs = seq(0, 1, 0.25), na.rm = TRUE,na.action=NULL))

    q1<-quant[2,]
    median<-quant[3,]
    q3<-quant[4,]

    # Calculating the interquartile range and the lower/upper threshold

    IQR<-q3-q1

    lower_threshold<-q1-1.5*IQR
    upper_threshold<-q3+1.5*IQR

    # Returning all data which fits between the thresholds

    return(median(x[which(x<=upper_threshold & x>=lower_threshold)],na.rm=TRUE,na.action=NULL))
  } 
