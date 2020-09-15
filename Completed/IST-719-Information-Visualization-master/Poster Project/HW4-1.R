# Author: Pan
# Purpose: IST719 HW4
#https://www.kaggle.com/joniarroba/noshowappointments
#http://www.theanalysisfactor.com/r-11-bar-charts/

#load the data
rd <-file.choose()
noshow <- read.csv(rd, header= TRUE, stringsAsFactors = FALSE)

str(noshow)



