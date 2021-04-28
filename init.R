rm(list = ls(pattern = ""))

source("raw_data_input.R")
source("price_basket_creator.R")
source("price_index_calculator.R")

# Change the position of the column name accordingly

month<-5
year<-1
trade_type<-2
hs<-8
partner_code<-3
value<-7
weight<-6
unit_of_measure<-NA
quantity<-4

# Base year of the price index

base_year<-2015

# Import/Export code

imports<-c("X")
exports<-c("R","M")

# If you want to use the program with multiple files set the variable below to TRUE

multiple_files=FALSE

# Monthly or Quarterly reports, TRUE for quaterly and FALSE for monthly

quaterly_monthly=TRUE

# If your file/files already have column names then set the variable below to FALSE

header=TRUE

# To view the defintion of these functions, look at "functions.R"

# raw_data_input(
#   positions=c(month,year,trade_type,hs,partner_code,value,weight,unit_of_measure,quantity),
#   multiple_files=FALSE,
#   header,
#   base_year,
#   quaterly_monthly,
#   imports,
#   exports
#   )

# add_data(
#   positions=c(month,year,trade_type,hs,partner_code,value,weight,unit_of_measure,quantity),
#   header=TRUE
# )

# basket_creator(
#   positions=c(month,year,trade_type,hs,partner_code,value,weight,unit_of_measure,quantity),
#   header,
#   hs_sections=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22),
#   base_year
# )

# laspeyres_index(base_year)

# paasche_index(base_year)



