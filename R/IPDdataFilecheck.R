#############################################################################################################################
#' Function to check the package is installed, if not install
#' @param pkg name of package(s)
#' @return 0, if packages cant be installed and loaded, else -1
#' @examples checkLoadPackages("dplyr")
#' @export
checkLoadPackages<-function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  list_packages<-available.packages(contriburl = contrib.url(repos = "http://cran.us.r-project.org", type=getOption("pkgType")))
  if(length(new.pkg)>=1){
      if(new.pkg%in%list_packages){
        suppressWarnings(install.packages(new.pkg, lib=.libPaths(), repos = "http://cran.us.r-project.org",
                         contriburl = contrib.url(repos = "http://cran.us.r-project.org", type=getOption("pkgType")),
                         method="auto", available = NULL, destdir = NULL,
                         dependencies = TRUE, type = getOption("pkgType"),
                         configure.args = getOption("configure.args"),
                         configure.vars = getOption("configure.vars"),
                         clean = FALSE, Ncpus = getOption("Ncpus", 1L),
                         verbose = getOption("verbose"),
                         libs_only = FALSE, INSTALL_opts, quiet = FALSE,
                         keep_outputs = FALSE))
      }else{
        stop("Invalid package ")
      }
  }
  valid=pkg[(pkg %in% list_packages)]
  sapply(valid, require, character.only = TRUE)
  return(0)
}

###########################################################################################################
#' Function to throw error on invalid directory or file or if the file is not readable
#' @param filename name of a file or dir
#' @return 0, if success -1, if failure
#' @examples testFileExistenceReadability(system.file("extdata", "blank.txt", package = "IPDFileCheck"))
#' @export
testFileExistenceReadability<-function(filename){
  if (file.exists(filename)){
    if (file.access(filename, 0)!=0){
      stop("Error reading file")
      ##return(-2)
    }
    return(0)
  }else{
    stop("Invalid directory or file")
    ##return(-1)
  }
}
###########################################################################################################
#' Function to test column names of a data being different from what specified
#' @param column.names column names of the data frame
#' @param data a data frame
#' @return 0, if success -1, if failure
#' @examples testDataColumnNames(c("name","age"),data.frame("Age" = c(21,15), "Name" = c("John","Dora")))
#' @export
testDataColumnNames<-function(column.names,data){
  upper.given.colnames=sort(toupper(column.names))
  upper.data.colnames=sort(toupper(colnames(data)))
  if (sum(upper.given.colnames==upper.data.colnames)==length(column.names)){
    return(0)
  }else{
    stop("One or other column may have different names")
    ##return(-1)
  }
}
###########################################################################################################
#' Function to check the format of 'age' in data
#' @param data a data frame
#' @param agecolumn column name that corresponds to age or date pf birth
#' @param nrcode non response code corresponding to age column
#' @return 0, if success -1 if failure
#' @examples testAge(data.frame("Age"= c(21,15), "Name"= c("John","Dora")),"age",999)
#' @export
testAge=function(data,agecolumn="age",nrcode=NA){
  column.no=getColumnNoForNames(data,agecolumn)
  if (column.no<0){
    stop("Column name age does not exist")
    ##return(-1)
  }else{
    entry <-data[[column.no]]
    blanks=c(which(entry==""),which(is.na(entry)))
    if (length(blanks)!=0){
      entry[blanks]=nrcode
    }
    if (is.na(nrcode)){
      newentry <- as.numeric(entry[ which(!is.na(entry))])
    }else{
      newentry <- as.numeric(entry[ which(entry!=nrcode)])
    }
    if (any(newentry>150 ) || any(newentry<0)){
      stop("Invalid entry in age column")
      ##return(-2)
    }else{
      return(0)
    }
  }
}


###########################################################################################################
#' Function to return the column number for column name
#' @param data a data frame
#' @param column.name column names of the data frame
#' @return column number, if success -1, if failure
#' @examples getColumnNoForNames(data.frame("Age"= c(21,15), "Name"= c("John","Dora")),"Name")
#' @export
getColumnNoForNames=function(data,column.name){
  data.column.names = toupper(colnames(data))
  if (any(data.column.names==toupper(column.name))){
    column.no=which(data.column.names==toupper(column.name))
    return(column.no)
  }else{
    stop("Column name does not exist")
    ##return(-1)
  }
}
###########################################################################################################
#' Function to check the format of 'gender' column in data
#' @param data a data frame
#' @param gendercolumn column name for gender
#' @param gendercode how gender is coded
#' @param nrcode non response code corresponding to gender column
#' @return 0, if success -1 if failure
#' @examples testGender(data.frame("sex"= c("m","f"), "Name"= c("John","Dora")),c("f", "m"),"sex",999)
#' @export
testGender=function(data,gendercode,gendercolumn="gender",nrcode=NA){
  gendercode<-toupper(gendercode)
  column.no=getColumnNoForNames(data,gendercolumn)
  if (column.no<0){
     stop("Column name for ender does not exist")
     #return(-1)
  }
  if (column.no>0){
    entry <- data[column.no]
    if(is.na(nrcode)){
      newentry<-entry[!is.na(entry)]
      gendercode<-gendercode[!is.na(gendercode)]
    }else{
      newentry<-entry[entry!=nrcode,]
      gendercode<-gendercode[which(gendercode!=nrcode)]
    }
    facs=levels(factor(toupper(newentry)))
    if (all(facs%in%gendercode)){
        return(0)
    }else{
      stop("Invalid entry in gender column")
      #return(-2)
    }
  }
}
###########################################################################################################
#' Function to check the format of column contents
#' @param data a data frame
#' @param column column name for gender
#' @param code how column values are  coded
#' @param nrcode non response code corresponding to gender column
#' @return 0, if success -1 if failure
#' @examples testColumnContents(data.frame("sex"= c("m","f"), "Name"= c("John","Dora")),"sex",c("m","f"),999)
#' @export
testColumnContents=function(data,column,code,nrcode=NA){
  column.no=getColumnNoForNames(data,column)
  if (column.no<0){
    stop("Column name does not exist")
    #return(-1)
  }
  if (column.no>0){
    entry <- data[column.no]
    if(is.na(nrcode)){
      newentry<-entry[!is.na(entry)]
      code<-code[!is.na(code)]
    }else{
      newentry<-entry[entry!=nrcode,]
      code<-code[which(code!=nrcode)]
    }
    facs=levels(factor(newentry))
    if (all(facs%in%code)){
      return(0)
    }else{
      stop("Invalid entry in column")
      #return(-2)
    }
  }
}
###########################################################################################################
#' Function to check the format of a numeric column
#' @param column.name the column name
#' @param data data frame
#' @param nrcode non response code corresponding to the column
#' @param minval minimum value allowed
#' @param maxval maximum value allowed
#' @return 0, if success -1, if failure
#' @examples testDataNumeric("age",data.frame("Age" = c(21,15), "Name" = c("John","Dora")),-99,0,100)
#' @export
testDataNumeric=function(column.name,data,nrcode=NA, minval, maxval){
  column.no=getColumnNoForNames(data,column.name)
  if (column.no<0){
    stop("Column name does not exist")
    ##return(-1)
  }else{
    entry <-(data[[column.no]])
    if (is.na(nrcode)){
      new.entry <- (entry[ which(!is.na(entry))])
    }else{
      new.entry <- (entry[ which(entry!=nrcode)])
    }
    if (is.numeric(new.entry)){
      if (any(new.entry<minval) || any(new.entry>maxval)){
        stop("Invalid ranges in column")
        #return(-2)
      }else{
        return(0)
      }
    }else{
      stop("Non numeric values in column")
      #return(-3)
    }
  }
}
###########################################################################################################
#' Function to check the format of a numeric column when the values are not bounded
#' @param column.name the column name
#' @param data data frame
#' @param nrcode non response code corresponding to the column
#' @return 0, if success -1, if failure
#' @examples testDataNumericNorange("marks",data.frame("marks" = c(210,99), "Name" = c("John","Dora")),-99)
#' @export
testDataNumericNorange=function(column.name,data,nrcode=NA){
  column.no=getColumnNoForNames(data,column.name)
  if (column.no<0){
    stop("Column name does not exist")
    #return(-1)
  }else{
    entry <-unlist(data.frame(data[[column.no]],stringsAsFactors = FALSE))
    if (is.na(nrcode)){
      no.nrcode.entries=entry[which(!is.na(entry))]
    }
    else{
      no.nrcode.entries=entry[which(entry!=nrcode)]
    }
    if (is.numeric(no.nrcode.entries)){
      return(0)
    }else{
      stop("Some values-other than NR code is not numeric")
      #return(-2)
    }
  }
}
###########################################################################################################
#' Function to check the format of a string column when the string values are given
#' @param data data frame
#' @param column.name the column name
#' @param nrcode non response code corresponding to the column
#' @param allowed.strings allowed strings or characters to represent meaningful entry
#' @return 0, if success -1, if failure
#' @examples testDataStringRestriction(data.frame("Age" = c(21,15), "sex" = c("m","f")),"sex",-999,c("f","m"))
#' @export
testDataStringRestriction=function(data,column.name,nrcode=NA,allowed.strings){
  res<-testDataString(data,column.name,nrcode)
  if (res==0){
    column.no=getColumnNoForNames(data,column.name)
    if (column.no<0){
      #return(-1)
      stop("column name does not exist")
    }else{
      if (length(allowed.strings)>=1){
        entry <-toupper(data[[column.no]])
        #entry <- dplyr::mutate_all(data[column.no], dplyr::funs(toupper))
        if (!is.na(nrcode)){
          new.entry=entry[entry!=nrcode]
        }else{
          new.entry=entry[!is.na(entry)]
        }
        if (any(is.na(new.entry)==TRUE) || sum(toupper(allowed.strings)%in%unique(new.entry))
            <length(unique(new.entry))){
          stop("Invalid entry in column")
          #return(-2)
        }else{
          return(0)
        }
      }else{
        stop("Please provide the restriction on allowed strings, else use testDataString(..)")
        #return(-3)
      }
    }
  }else{
    if(res==-1){
      stop("Column name does not exist")
      #return(-4)
    }
    if(res==-2){
      stop("atleast one non string entry in column")
      #return(-5)
    }

  }
}
###########################################################################################################
#' Function to check the format of a string column
#' @param data data frame
#' @param column.name the column name
#' @param nrcode non response code corresponding to the column
#' @return 0, if success -1, if failure
#' @examples testDataString(data.frame("Age" = c(21,15), "Name" = c("John","Dora")),"name",-999)
#' @export
testDataString=function(data,column.name,nrcode=NA){
  column.no=getColumnNoForNames(data,column.name)
  if (column.no<0){
    stop("Column name does not exist")
    #return(-1)
  }else{
    temp=data[column.no]
    temp=unlist(temp[!is.na(temp)])
    if (!is.na(nrcode)){
      new.entry=temp[temp!=nrcode]
    }else{
      new.entry=temp[!is.na(temp)]
    }
    new.entry=suppressWarnings(as.numeric(as.character(new.entry)))
    if (any(!is.na(new.entry))){
      stop("Numeric entry in column")
      #return(-2)
    }else{
      return(0)
    }
  }
}
###########################################################################################################
#' Function to return the column number if a given pattern is contained in the column names of a data
#' @param pattern a string that needs to be checked
#' @param column.names column names actually have
#' @return column number, if success -1, if failure
#' @examples getColumnNoForPatternInColumnname("age","female_age")
#' @export
getColumnNoForPatternInColumnname=function(pattern,column.names){
  if(checkColumnNoForPatternInColumnname(pattern,column.names)==TRUE){
    test=grep(toupper(pattern),toupper(column.names))
    return(test)
  }else{
    stop("The pattern does not form any part of columnnames")
  }
}
###########################################################################################################
#' Function to return the column number if a given pattern is contained in the column names of a data
#' @param pattern a string that needs to be checked
#' @param column.names column names actually have
#' @return TRUE , if success FALSE, if failure
#' @examples checkColumnNoForPatternInColumnname("age","female_age")
#' @export
checkColumnNoForPatternInColumnname=function(pattern,column.names){
  if (is.na(pattern) || pattern==""){
    stop("Error pattern NA or empty")
  }else{
    if (is.numeric(pattern)){
      test=grep(toString(pattern),toupper(column.names))
    }else{
      test=grep(toupper(pattern),toupper(column.names))
    }
    if (length(test)==0){
      return(FALSE)
    }else{
      return(TRUE)
    }
  }
}
###########################################################################################################
#' Function to return descriptive statistics, sum, no of observations, mean, mode. median, range, standard deviation and standard error
#' @param data data frame
#' @param column.name the column name
#' @param nrcode non response code corresponding to the column
#' @return the descriptive statistics for success , -1 for failure
#' @examples descriptiveStatisticsDataColumn(data.frame("age" = c(21,15), "Name" = c("John","Dora")),"age",NA)
#' @import stats
#' @export
descriptiveStatisticsDataColumn=function(data,column.name, nrcode=NA){
  col.names=colnames(data)
  if (column.name%in%col.names){
    if (testDataNumericNorange(column.name,data,nrcode)!=0){
      stop("Non numeric columns, cant estimate the descriptive statistics")
      #return(-1)
    }else{
      this.column=data[column.name]
      if (is.na(nrcode)){
        this.column=this.column[!is.na(data[column.name])]
      }else{
        this.column=this.column[data[column.name]!=nrcode & !is.na(data[column.name])]
      }
      this.sum=sum(this.column)
      this.av=mean(this.column)
      this.med=median(this.column)
      this.mode=getModeForVector(this.column)
      this.range.low=min(this.column)
      this.range.high=max(this.column)
      this.sd=sd(this.column)
      this.se<- this.sd/sqrt(length(this.column))
      this.lq<- quantile(this.column,c(0.25))
      this.uq<- quantile(this.column,c(0.75))
      this.CI.low<- quantile(this.column,c(0.025))
      this.CI.high<- quantile(this.column,c(0.975))
      results=matrix(c(this.sum,this.av,this.sd,this.med,this.mode,this.se,this.range.low,this.range.high,
                       length(this.column),this.lq,this.uq,this.CI.low,this.CI.high), byrow=TRUE,nrow=1)
      colnames(results)<-c("Sum","Mean","SD","Median", "Mode","SE","Minimum","Maximum","Count","LQ","UQ","95%CI.low","95%CI.high")
      rownames(results)<-column.name
      return(results)
    }
  }else{
    stop("Error - no column or column name different")
    #return(-2)
  }
}
###########################################################################################################
#' Function to return mode
#' @param v a vector
#' @return mode
#' @examples getModeForVector(c(1,1,2,3))
#' @export
getModeForVector <- function(v) {
  if (is.numeric(v)){
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
  }else{
    stop("Non numeric data")
    #return(-1)
  }
}
###########################################################################################################
#' Function to check the given column exists
#' @param column.name a column name
#' @param data data frame
#' @return 0 if success -1 if failure
#' @examples checkColumnExists("age",data.frame("Age" = c(21,15), "Name" = c("John","Dora")))
#' @export
checkColumnExists<-function(column.name,data){
  if(any(toupper(colnames(data))==toupper(column.name))){
    return(0)
  }else{
    stop("Data does not contain the column with the specfied column name")
    #return(-1)
  }
}

###########################################################################################################

#' Function to present the mean and sd of a data set in the form Mean (SD)
#' @param data data frame
#' @param column.name the column name
#' @param nrcode non response code corresponding to the column
#' @return the mean(sd), -1 for failure
#' @examples presentMeanSdRemoveNAText(data.frame("age" = c(21,15), "Name" = c("John","Dora")),"age",NA)
#' @export
presentMeanSdRemoveNAText=function(data,column.name,nrcode=NA){
  desc=descriptiveStatisticsDataColumn(data,column.name,nrcode)
  if(length(desc)==1 & desc[1]==-1){
    stop("Error or no data to analyse ")
    #return(-1)
  }else{
    desc=data.frame(desc)
    this.mean=as.numeric(desc$Mean)
    this.sd=as.numeric(desc$SD)
    ans=paste(round(this.mean,2)," (",round(this.sd,2),")",sep="")
    #ans=paste(desc$Mean," (",desc$SD,")",sep="")
    return(ans)
  }


}
###########################################################################################################
#' Function to return a subgroup when certain variable equals the given value while omitting those with NA
#' @param data data frame
#' @param variable that corresponds to a column
#' @param value a value that can be taken by the variable
#' @examples returnSubgroupOmitNA(data.frame("age" = c(21,15), "Name" = c("John","Dora")),"age",10)
#' @return subgroup a data frame if success -1 if failure
#' @export
returnSubgroupOmitNA=function(data,variable,value){
  if(checkColumnExists(variable,data)==0){
      column.no=getColumnNoForNames(data,variable)
      subgroup = data[which(data[column.no]==value & !is.na(data[column.no])),]
      return(subgroup)
  }else{
    stop("No column exists")
    #return(-1)
  }
}
###########################################################################################################
#' Function to find the effect size Cohen's d
#' @param x, a vector
#' @param y, another vector
#' @return cohens d estimated with 95% CI or -1 if failure
#' @examples cohensD(c(1,2,3,4),c(3,4,5,6))
#' @export
cohensD<- function(x, y) {
  xx<-suppressWarnings(as.numeric(x))
  yy<-suppressWarnings(as.numeric(y))
  xnotna=sum(!is.na(xx))
  ynotna=sum(!is.na(yy))
  if(xnotna==length(x) && xnotna==length(y)){
    lx <- length(x)- 1
    ly <- length(y)- 1
    md  <- abs(mean(x) - mean(y))        ## mean difference (numerator)
    csd <- lx * var(x) + ly * var(y)
    csd <- csd/(lx + ly)
    csd <- sqrt(csd)                     ## common sd computation
    cd  <- md/csd                        ## cohen's d
    var_d = 1/(lx+1) +1/(ly+1) + (cd^2)/(2*(lx+ly+2))
    ans = c(cd,cd-1.96*sqrt(var_d),cd+1.96*sqrt(var_d) )
    return(ans)
  }else{
    stop("Vector contains atleast one NA or string")
    #return(-1)
  }
}
###########################################################################################################
#' Function to estimate standard error of the mean
#' @param x, a vector
#' @return SE the standard error of the mean
#' @examples getSEM(c(1,2,3,4))
#' @export
## estimate standard error of th mean and return
getSEM <- function(x){
  xx<-suppressWarnings(as.numeric(x))
  if(sum(is.na(xx))>0){
    stop("Vector contains non numeric data")
    #return(-1)
  }else{
    ans<-sd(x)/sqrt(length(x))
    return(ans)
  }
}
###########################################################################################################

#' Function to find the number and percentages of categories
#' @param data, a data frame
#' @param variable the column name
#' @param nrcode non response code
#' @return number and percentages or -1 if failure
#' @examples representCategoricalData(data.frame(mark=c(0,11,78,160),gender=c("f","m","f","m")),"gender",NA)
#' @export
representCategoricalData<-function(data,variable,nrcode=NA) {
  coding<-unique(toupper(factor(data[[variable]])))
  if(is.na(nrcode)){
    coding<-coding[!is.na(coding)]
  }else{
    coding<-coding[coding!=nrcode]
  }
  num.categories=length(coding)
  if(checkColumnExists(variable,data)==0){
    ans = rep(0,2*num.categories)
    for(i in 1:num.categories){
      if(coding[i]=="NA"){
        num=nrow(data[which(is.na(data[variable])),])
      }else{
        uppervals=toupper(factor(data[[variable]]))
        if(is.na(nrcode)){
          num=nrow(data[which(uppervals==coding[i] & !is.na(uppervals)),])
        }else{
          num=nrow(data[which(uppervals==coding[i] & uppervals!=nrcode),])
        }
      }
      perc=100*num/nrow(data)
      ans[2*i]=round(perc,2)
      ans[2*i-1]=round(num,2)
    }
    return(ans)
  }else{
    stop("No column exists")
    #return(-1)
  }
}
###########################################################################################################
#' Function to represent categorical data in the form - numbers (percentage)
#' @param data data frame
#' @param variable column name
#' @param nrcode non response code
#' @return the numbers (percentage) , -1 for failure
#' @examples representCategoricalDataText(data.frame(mark=c(0,11,78,160),gender=c("f","m","f","m")),"gender",NA)
#' @export
representCategoricalDataText=function(data, variable, nrcode){
    intresult=representCategoricalData(data, variable, nrcode)
    ans = c(0)
    i=1
    while(i<length(intresult)){
      num<-intresult[i]
      perc<-intresult[i+1]
      temp=c(paste(round(num,2)," (",round(perc,2),")",sep=""))
      ans=cbind(ans,temp)
      i=i+2
    }
    ans=ans[-1]
    return(ans)
}
###########################################################################################################
#' Function to calculate age from date of birth
#' @param data a data frame
#' @param columnname name of column corresponding to date of birth
#' @param dateformat format of date e.g. dmy default is FALSE
#' @param nrcode non response code corresponding to date of birth
#' @return data if success -1 if failure
#' @examples
#' library(IPDFileCheck)
#' calculateAgeFromDob(data.frame("dob"=c("1987-05-28","1987-06-18","1987-07-09"),
#' "num"=c(1,2,3),stringsAsFactors = FALSE),"dob")
#' @importFrom eeptools age_calc
#' @export
calculateAgeFromDob<-function(data,columnname,dateformat=FALSE,nrcode=NA){
  column.no=getColumnNoForNames(data,columnname)
  if (column.no<0){
    stop("Column name for date of birth does not exist")
    #return(-1)
  }else{
    data <-as.data.frame(data,string.as.factors=FALSE)
    entry<-data[[column.no]]
    blanks=c(which(entry==""),which(is.na(entry)))
    if (length(blanks)!=0){
      entry[blanks]=nrcode
    }
    calculated.ages=rep(0,length(entry))
    this.year <- lubridate::year(as.Date(Sys.Date(), format='%d/%m/%y'))
    if (is.na(nrcode)){
      index<-which(!is.na(entry))
    }else{
      index <- which(entry!=nrcode)
    }
    if (dateformat==FALSE){
      mod.entry<-convertStdDateFormat(entry,index,monthfirst=NULL)
    }else{
      if( dateformat=="%y/%m/%d" || dateformat=="%y-%m-%d"){
        mod.entry=entry
      }
      if( dateformat=="%y/%d/%m" || dateformat=="%y-%d-%m"){
        monthfirst=FALSE
        mod.entry<-convertStdDateFormat(entry,index,monthfirst)
      }
      if(dateformat=="%m/%d/%y" || dateformat=="%m-%d-%y"){
        monthfirst=TRUE
        mod.entry<-convertStdDateFormat(entry,index,monthfirst)
      }
      if(dateformat=="%d/%m/%y" || dateformat=="%d-%m-%y"){
        monthfirst=FALSE
        mod.entry<-convertStdDateFormat(entry,index,monthfirst)
      }
    }

    result <- eeptools::age_calc(as.Date(mod.entry[index]), units='years')
    # tryCatch( { result <- eeptools::age_calc(as.Date(mod.entry[index]), units='years')}
    #           , error = function(e) {an.error.occured =TRUE})

    calculated.ages[index]<-result
    calculated.ages[blanks]<-NA
    nonNAages<-calculated.ages[!is.na(calculated.ages)]
    # }else{
    #   stop("Date format is not right- use numeric values for dates separated by - or /")
    #   #return(-2)
    # }
    if (any(nonNAages>150 ) || any(nonNAages<0)){
      stop("Age can not be negative OR greater than 150")
      #return(-3)
    }else{
      data["calc.age.dob"]<-calculated.ages
      return(data)
    }
  }
}
###########################################################################################################
#' Helper function to keep date formats in year/month/date
#' @param entry a data frame or a  vector
#' @param index those correspond to valid date (omitting non response code or no entry)
#' @param monthfirst if month is given before date, NULL by default
#' @return entry corrected entries
#' @examples convertStdDateFormat(c("01/01/2000","02/02/2002"),c(1,2),NULL)
#' @export
convertStdDateFormat<-function(entry, index,monthfirst=NULL){
  ## conetnes of the date string
  contents<-unlist(strsplit(entry[1], ""))
  ## Get the location of non numeric separator?
  first<-which(!grepl('^[0-9]',contents))[1]
  ch<-contents[first]
  one=two=three=c(0)
  for (i in 1:length(index)){
    one<-c(one,unlist(strsplit(entry[index[i]], ch))[1])
    two<-c(two,unlist(strsplit(entry[index[i]], ch))[2])
    three<-c(three,unlist(strsplit(entry[index[i]], ch))[3])
  }
  one=(one[-1])
  two=(two[-1])
  three=(three[-1])
  test1=sum(is.na(suppressWarnings(as.numeric(one))))==length(one)
  test2=sum(is.na(suppressWarnings(as.numeric(two))))==length(two)
  test3=sum(is.na(suppressWarnings(as.numeric(three))))==length(three)
  check=test1||test2||test3
  if(check==FALSE){
    one=as.numeric(one)
    two=as.numeric(two)
    three=as.numeric(three)
    if(min(one)>=1000 ){
      if(is.null(monthfirst)){
        if(max(two)<=12 || max(three)>12){
          for (i in 1:length(index))
            entry[index[i]]=paste(one[i],"/",two[i],"/",three[i],sep="")

        }else{
          for (i in 1:length(index))
            entry[index[i]]=paste(one[i],"/",three[i],"/",two[i],sep="")
        }
      }else{
        if(monthfirst==FALSE){
          for (i in 1:length(index))
            entry[index[i]]=paste(one[i],"/",three[i],"/",two[i],sep="")
        }else{
          for (i in 1:length(index))
            entry[index[i]]=paste(one[i],"/",two[i],"/",three[i],sep="")

        }
      }
    }else{
      if(min(three)>=1000){
        if(is.null(monthfirst)){
          if(max(two)<=12|| max(one)>12){
            for (i in 1:length(index)){
              entry[index[i]]=paste(three[i],"/",two[i],"/",one[i],sep="")
            }
          }else{
            for (i in 1:length(index)){
              entry[index[i]]=paste(three[i],"/",one[i],"/",two[i],sep="")
            }
          }
        }else{
          if(monthfirst==FALSE){
            for (i in 1:length(index)){
              entry[index[i]]=paste(three[i],"/",two[i],"/",one[i],sep="")
            }
          }else{
            for (i in 1:length(index)){
              entry[index[i]]=paste(three[i],"/",one[i],"/",two[i],sep="")
            }
          }

        }

      }else{
        stop("Error-no year shown in date")
        #return(-1)
      }
    }
  }else{
    stop("Date not in numeric formats")
    #return(-1)
  }
  return(entry)
}
###########################################################################################################
#' Function to calculate age from year of birth
#' @param data a data frame
#' @param columnname name of column corresponding to year of birth
#' @param nrcode non response code corresponding to date of birth
#' @return data, if success -1 if failure
#' @examples calculateAgeFromBirthYear(data.frame("yob" = c(1951,1980), "Name" = c("John","Dora")),"yob",NA)
#' @export
calculateAgeFromBirthYear<-function(data,columnname,nrcode=NA){
  column.no=getColumnNoForNames(data,columnname)
  if (column.no<0){
    stop("Column name for year of birth does not exist")
    #return(-1)
  }else{
    entry<-data[[column.no]]
    blanks=c(which(entry==""),which(is.na(entry)))
    if (length(blanks)!=0){
      entry[blanks]=nrcode
    }
    calculated.ages=rep(0,length(entry))

    this.year <- lubridate::year(as.Date(Sys.Date(), format='%d/%m/%y'))
    if (is.na(nrcode)){
      index<-which(!is.na(entry))
      calculated.ages[index]<-this.year-as.numeric(as.character(entry[index]))
      calculated.ages[blanks]<-NA

    }else{
      index <- which(entry!=nrcode)
      calculated.ages[index]<-this.year-as.numeric(as.character(entry[index]))
      calculated.ages[blanks]<-NA

    }
    nonNAages<-calculated.ages[!is.na(calculated.ages)]
    if (any(nonNAages>150 ) || any(nonNAages<0)){
      stop("Age can not be negative OR greater than 150")
      #return(-2)
    }else{
      data["calc.age.yob"]<-calculated.ages
      return(data)
    }
  }
}
###########################################################################################################
#' Function to return the unique contents of the column given the column name
#' @param data a data frame
#' @param colname name of column corresponding to year of birth
#' @return the contents of the column, if success -1 if failure
#' @examples getConentdInCols(data.frame("yob" = c(1951,1980), "Name" = c("John","Dora")),"yob")
#' @export
getConentdInCols<-function(data, colname){
  #check to see if the columnname exists
  if(checkColumnExists(colname,data)==0){
    data<-as.data.frame(data,stringAsFactors=FALSE)
    codes=unique(data[[colname]])
    if(sum(is.na(suppressWarnings(as.numeric(codes))))<length(codes)){
      return(as.numeric(codes))
    }else{
      return(codes)
    }
  }else{
    stop("No column exists with the given name")
    #return(-1)
  }
}
