source("functions.R")

basket_creator<-function(positions,header,hs_sections,base_year){

  month<-positions[1]
  year<-positions[2]
  trade_type<-positions[3]
  hs<-positions[4]
  partner_code<-positions[5]
  value<-positions[6]
  weight<-positions[7]
  unit_of_measure<-positions[8]
  quantity<-positions[9]

  basket<-data.frame()

  file<-file.choose()

  raw_data<-retrieve_data(file,header)

  raw_data<-raw_data[,c(month,year,trade_type,hs,partner_code,value,weight,unit_of_measure,quantity)]

  colnames(raw_data)<-c("MONTH","YEAR","TRADE_TYPE","HS","PARTNER_CODE","VALUE","WEIGHT","UNIT_OF_MEASURE","QUANTITY")

  # Defining the different sections
  
  sections<-c("01-05","06-14","15",'16-24','25-27',
  '28-38','39-40','41-43','44-46','47-49',
  '50-63','64-67','68-70','71','72-83','84-85',
  '86-89','90-92','93','94-96','97','98-99')

  # Using a for loop for each HS section selected

  for(section_number in hs_sections){

  # Checking if the HS code belongs to the right section

  split<-strsplit(sections[section_number],"\\-")[[1]]

    if(length(split)==1){

      selected<-raw_data[as.numeric(substr(raw_data$HS,1,2)) == as.numeric(split) &
      raw_data$YEAR == base_year,]

    }else{

      selected<-raw_data[as.numeric(substr(raw_data$HS,1,2)) <= as.numeric(split[2]) & 
      as.numeric(substr(raw_data$HS,1,2)) >= as.numeric(split[1]) &
      raw_data$YEAR == base_year,]

    }

    # Aggregating the data based on HS code, trade type and the partner code.

    if(nrow(na.omit(selected))!=0){

    selected<-aggregate(cbind(VALUE=as.numeric(VALUE),QUANTITY=as.numeric(QUANTITY))~HS+TRADE_TYPE+PARTNER_CODE,data=selected,sum)

    total<-sum(selected$VALUE)

    # Adding the columns section_sum, proportion, section number and row number

    selected$SECTION_SUM<-total
    selected$PROPORTION<-selected$VALUE/total
    selected$SECTION<-section_number

    selected<-selected[order(selected$VALUE,decreasing = TRUE),]
    rownames(selected)<-NULL
    selected$row_num <- seq.int(nrow(selected))

    # Only taking the items valuing more than 5% of the total basket value

    selected<-selected[selected$PROPORTION >=0.05,]

    # Adding the selected hs chapter to the basket

    basket<-rbind(selected,basket)

    }

  }

  # Writing the price basket to the "Price_basket.csv" file

  write.csv(basket,"Price_basket.csv",row.names=FALSE)

}