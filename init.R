rm(list = ls(pattern = ""))

source("raw_data_input.R")
source("price_basket_creator.R")
source("price_index_calculator.R")

# Change the position of the column name accordingly

month<-1
year<-2
trade_type<-3
hs<-4
partner_code<-5
value<-6
weight<-7
unit_of_measue<-8
quantity<-9

# Weight reference period (aka base year) of the price index

base_year<-2015

# Import/Export code

imports<-c("M")
exports<-c("X","R")

# If you want to use the program with multiple files set the variable below to TRUE

multiple_files=FALSE

# Monthly or Quarterly reports, TRUE for quaterly and FALSE for monthly

quaterly_monthly=TRUE

# If your file(s) already has(ve) column names then set the variable below to FALSE

header=TRUE

# To view the defintion of these functions, look at "functions.R"

# raw_data_input(
#   positions=c(month,year,trade_type,hs,partner_code,value,weight,unit_of_measue,quantity),
#   multiple_files=FALSE,
#   header,
#   base_year,
#   quaterly_monthly,
#   imports,
#   exports
#   )

# add_data(
#   positions=c(month,year,trade_type,hs,partner_code,value,weight,unit_of_measue,quantity),
#   header=TRUE
# )

# basket_creator(
#   positions=c(month,year,trade_type,hs,partner_code,value,weight,unit_of_measue,quantity),
#   header,
#   hs_sections=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22),
#   base_year
# )

# laspeyres_index(base_year)

# paasche_index(base_year)

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



