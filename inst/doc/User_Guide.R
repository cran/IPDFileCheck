## ---- include = FALSE----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup---------------------------------------------------------------
library(IPDFileCheck)

## ------------------------------------------------------------------------
 set.seed(17)
 rctdata <- data.frame(age=abs(rnorm(10, 60, 20)),
                           sex=c("M", "F","M", "F","M", "F","M", "F","F","F"),
                           yob=sample(seq(1930,2000), 10, replace=T),
                           dob=c("07/12/1969","16/02/1962","03/09/1978","17/02/1969",                                      "25/11/1960","17/04/1970","18/03/1997","30/01/1988",
                                               "03/02/1990","25/09/1978"),
                           arm=c("Control", "Intervention","Control", "Intervention","Control", "Intervention","Control", "Intervention","Control", "Intervention"),stringsAsFactors = FALSE)
 
rctdata_error <- data.frame(age=runif(10, -60, 120),
                           sex=c("M", "F","M", "F","M", "F","M", "F","F","F"),
                           yob=sample(seq(1930,2000), 10, replace=T),
                           dob=c("1997 May 28","1987-June-18",NA,"2015/July/09","1997 May 28","1987-June-18",NA,"2015/July/09","1997 May 28","1987-June-18"),
                           arm=c("Control", "Intervention","Control", "Intervention","Control", "Intervention","Control", "Intervention","Control", "Intervention"),stringsAsFactors = FALSE)

## ------------------------------------------------------------------------
  thisdir=getwd()
  testFileExistenceReadability(thisdir)
  nodir=paste(thisdir,"test",sep="/")
  testFileExistenceReadability(nodir)

## ------------------------------------------------------------------------
checkColumnExists("sex",rctdata)
checkColumnExists("gender",rctdata)

## ------------------------------------------------------------------------
getColumnNoForNames(rctdata,"sex")
getColumnNoForNames(rctdata,"gender")

## ------------------------------------------------------------------------
testColumnContents(rctdata,"sex",c("M","F"),NA)
testColumnContents(rctdata,"sex",c("M","F"))
testColumnContents(rctdata,"sex",c("Male","Female"))

## ------------------------------------------------------------------------
testDataColumnNames(c("age","sex","dob","yob","arm"),rctdata)
testDataColumnNames(c("arm","age","yob","dob","age"),rctdata)
testDataColumnNames(c("arm","gender","yob","dob","age"),rctdata)

## ------------------------------------------------------------------------
testAge(rctdata,"age",NA)
testAge(rctdata_error,"age",NA)

## ------------------------------------------------------------------------
testGender(rctdata,c("M","F"),"sex",NA)
testGender(rctdata,c("Male","Female"),"sex",NA)

## ------------------------------------------------------------------------
testDataNumeric("age",rctdata,NA,0,100)
testDataNumeric("yob",rctdata,NA,0,100)

## ------------------------------------------------------------------------
testDataNumericNorange("age",rctdata,NA)
testDataNumericNorange("yob",rctdata,NA)
testDataNumericNorange("arm",rctdata,NA)

## ------------------------------------------------------------------------
testDataString(rctdata,"arm",NA)
testDataString(rctdata,"yob",NA)

## ------------------------------------------------------------------------
testDataStringRestriction(rctdata,"arm",NA,c("Intervention","Control"))
testDataStringRestriction(rctdata,"sex",NA,c("M","F"))
testDataStringRestriction(rctdata,"sex",NA,c("Male","Female"))


## ------------------------------------------------------------------------
getColumnNoForPatternInColumnname("ob",colnames(rctdata))
getColumnNoForPatternInColumnname("gender",rctdata)


## ------------------------------------------------------------------------
descriptiveStatisticsDataColumn(rctdata, "age")
descriptiveStatisticsDataColumn(rctdata, "sex")

## ------------------------------------------------------------------------
presentMeanSdRemoveNAText(rctdata, "age")
presentMeanSdRemoveNAText(rctdata, "sex")

## ------------------------------------------------------------------------
returnSubgroupOmitNA(rctdata, "sex","F")
returnSubgroupOmitNA(rctdata, "arm","control")

## ------------------------------------------------------------------------
representCategoricalDataText(rctdata, "sex",NA)
representCategoricalDataText(rctdata, "arm",NA)

## ------------------------------------------------------------------------
calculateAgeFromDob(rctdata,"dob","%d/%m/%y",NA)
calculateAgeFromDob(rctdata_error,  "dob",0,NA)

## ------------------------------------------------------------------------
calculateAgeFromBirthYear(rctdata,"yob",NA)
