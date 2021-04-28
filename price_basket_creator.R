source("functions.R")

# Section		Chapters	Name
# I	        01-05	    Live animals; animal products
# II        06-14	    Vegetable products
# III	      15	      Animal or vegetable fats or oils and their cleavage products; prepared edible fats; animal or vegetable waxes
# IV	      16-24	    Prepared foodstuffs; beverages; spirits and vinegar, tobacco and manufactured tobacco substitutes
# V	        25-27	    Mineral products
# VI        28-38	    Products of the chemical or allied industries
# VII       39-40	    Plastics and articles thereof; rubber and articles thereof
# VIII      41-43	    Raw hides and skins, leather, furskins, and articles thereof; saddlery and harness; travel goods, handbags, and similar containers; articles of animal gut (except silk-worm gut)
# IX	      44-46	    Wood and articles of wood; wood charcoal; cork and articles of cork; manufactures of straw, of esparto, or of other plaiting materials; basketware and wickerwork
# X	        47-49	    Pulp of wood or of other fibrous cellulosic material; recovered (waste and scrap) paper of paperboard; paper and paperboard and articles thereof
# XI	      50-63	    Textiles and textile products
# XII	      64-67	    Footwear, headgear, umbrellas, sun umbrellas, walking-sticks, seat-sticks, whips, riding-crops and parts thereof; prepared feathers and articles made thereof; article flowers; articles of human hair
# XIII	    68-70	    Articles of stone, plaster, cement, asbestos, mica or similar materials; ceramic products; glass and glassware
# XIV	      71	      Natural or cultured pearls, precious or semi-precious stones, precious metals, metals clad with precious metal and articles thereof; imitation jewellery; coin
# XV	      72-83	    Base metals and articles of base metal
# XVI	      84-85	    Machinery and mechanical appliances; electrical equipment; parts thereof; sound recorders and reproducers, television image and sound recorders and reproducers, and parts and accessories of such articles
# XVII	    86-89	    Vehicles, aircraft, vessels, and associated transport equipment
# XVIII	    90-92	    Optical, photographic, cinematographic, measuring, checking, precision, medical or surgical instruments and apparatus; clocks and watches; musical instruments; parts and accessories thereof
# XIX	      93	      Arms and ammunition; parts and accessories thereof
# XX	      94-96	    Miscellaneous manufactured articles
# XXI	      97	      Works of art, collectors' pieces and antiques
# XXII	    98-99	    Miscellaneous provisions; non-merchandise trade

# Optional basket creation function, select the HS sections based on table above

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

  # Checking if the unit of measure is included in the data

   if(is.na(unit_of_measure)){
    num_columns=8
  }else{
    num_columns=9
  }

  # Retrieveing the data

  basket<-data.frame()

  file<-file.choose()

  raw_data<-retrieve_data(file,header.,num_columns)

  # Reordering the columns

  if(is.na(unit_of_measure)){
   raw_data<-raw_data[,c(month,year,trade_type,hs,partner_code,value,weight,quantity)]
  }else{
   raw_data<-raw_data[,c(month,year,trade_type,hs,partner_code,value,weight,unit_of_measure,quantity)]
  }

  # Naming the columns

  if(is.na(unit_of_measure)){
  colnames(raw_data)<-c("MONTH","YEAR","TRADE_TYPE","HS","PARTNER_CODE","VALUE","WEIGHT","QUANTITY")
  }else{
  colnames(raw_data)<-c("MONTH","YEAR","TRADE_TYPE","HS","PARTNER_CODE","VALUE","WEIGHT","UNIT_OF_MEASURE","QUANTITY")
  }

  # Defining the different sections
  
  sections<-c("01-05","06-14","15",'16-24','25-27',
  '28-38','39-40','41-43','44-46','47-49',
  '50-63','64-67','68-70','71','72-83','84-85',
  '86-89','90-92','93','94-96','97','98-99')

  # Using a for loop for each HS section selected

  for(section_number in hs_sections){

  # Checking if an HS section has more than 2 digits (i.e. 71 vs 72-83)

  split<-strsplit(sections[section_number],"\\-")[[1]]

    if(length(split)==1){
      
      # Finding the data corresponding to the HS section

      selected<-raw_data[as.numeric(substr(raw_data$HS,1,2)) == as.numeric(split) &
      raw_data$YEAR == base_year,]

    }else{

      # Finding the data corresponding to the HS section

      selected<-raw_data[as.numeric(substr(raw_data$HS,1,2)) <= as.numeric(split[2]) & 
      as.numeric(substr(raw_data$HS,1,2)) >= as.numeric(split[1]) &
      raw_data$YEAR == base_year,]

    }

    # Aggregating the data based on HS code, trade type and the partner code.

    if(nrow(na.omit(selected))!=0){

    selected<-aggregate(cbind(VALUE=as.numeric(VALUE),QUANTITY=as.numeric(QUANTITY))~HS+TRADE_TYPE+PARTNER_CODE,data=selected,sum)

    # Calculating the total

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