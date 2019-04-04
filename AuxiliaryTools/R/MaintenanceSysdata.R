#Date: 20170111
#' MaintenananceSysdata
#' @name MaintenananceSysdata
#' @description Functions to maintain sysdata.rda file of specified package
#' @author Johannes Anwander
#' @section Maintenance Functions:
#' \tabular{lll}{
#' \strong{Function Name}                   \tab  \strong{Short Description} \cr
#' \code{\link{exportEmptyErrFile}}         \tab Creates and exports new empty csv file with column names according to fields to be maintained for error codes \cr
#' \code{\link{exportErrSysdata}}           \tab Creates and exports 'sysdata.rda' file containing the error codes and error messages as well as other objects. \cr
#' \code{\link{exportPackageSysdata}}       \tab Creates 'sysdata.rda' file containing any objects for package \cr
#' \code{\link{importErrDataFrame}}         \tab Imports errorcodes and corresponding attributes out of specified csv file \cr
#' \code{\link{maintainCSVDataWithFix}}     \tab Maintains any csv file with editor supplied by function fix \cr
#' }
NULL

#Date: 20170111
#File description of MaintenanceSysdata.R:
# - FIRST SECTION of MaintenanceSysdata.R contains maintenance functions
# - SECOND SECTION of MaintenanceSysdata.R contains helper functions


#FIRST SECTION START - maintenance functions

#CONSTANTS
#Default csv seperator
CSV_SEPERATOR=";"
#default errorcode file name
CSV_ERRFILE="errorcodes.csv"

#  Version 1.0
#  Date: 20161219
#' @name exportErrSysdata
#' @family MaintenananceSysdata
#' @title exportErrSysdata
#' @description exportErrSysdata creates 'sysdata.rda' file containing the error codes and error messages within the referenced package in the 'R' folder.
#' @aliases exportErrSysdata
#' @export exportErrSysdata
#' @author Johannes Anwander
#' @param csvErrFile represents the csv file containing the error codes and error messages
#' @param packagePath represents path to package 'sysdata.rda' gets written to.
#' @param boolExtdata is boolean variable, when         'TRUE':  param csvErrFile is path of csv file for importing error codes and messages
#'                                         when default 'FALSE': param csvErrFile is name under which file is available in extdata directory of package
#' @section Exported Functions:
#' Functions to be exported on default when calling function exportErrSysdata.
#' \tabular{lll}{
#' \strong{Function Name}                   \tab  \strong{Short Description} \cr
#' \code{\link{getSysdataErrorEntry}}       \tab Returns entry for specified error code in errorcodes data.frame (list element of SYSDATA_OBJECTS) within sysdata  \cr
#' \code{\link{getSysdataObject}}           \tab Returns list object of SYSDATA_OBJECTS with specified object name. \cr
#' }
#' @examples
#' #Creating 'sysdata.rda' file with error codes and messages
#' for package 'CoopGame' (analogue for 'CommunicationGames')
#'
#' #USECASE 1 (recommended usage) - Working directory is set
#' #to folder of package 'CoopGame' (under development) and error codes
#' # and messages are stored in a CSV file within the same package in the folder 'inst/extdata/'
#'
#' setwd("CoopGame");
#'
#' #CoopGame  <-Working Directory
#' #├── inst
#' #│   └── extdata
#' #│       └── errorcodes.csv <-File referenced by csvErrFile
#' #├── man
#' #├── man-roxygen
#' #├── R
#' #└── tests
#'
#'exportErrSysdata(csvErrFile="errorcodes.csv")
#'
#'#or also, cause default value of csvErrFile:
#'exportErrSysdata()
#'
#' #USECASE 2 - Working directory is not set to folder of package 'CoopGame' (under development) and CSV file with error codes and error message is external
#'
#' setwd("SVN_REPO")
#'
#' #SVN_REPO  <-Working directory
#' #├── CommunicationGames
#' #├── CoopGame
#' #└── errcodes.csv
#'
#' exportErrSysdata(csvErrFile="./errorcodes.csv", packagePath="./CoopGame", boolExtdata=FALSE)
#'#or also, cause default value of csvErrFile:
#' exportErrSysdata(packagePath="./CoopGame", boolExtdata=FALSE)


exportErrSysdata<-function(csvErrFile=CSV_ERRFILE,packagePath=".",boolExtdata=TRUE,...){
  # csvErrFilePath<-getCSVFilePath(packagePath = packagePath, fileName = csvErrFile, boolExtdata);
  errorcodes<-importErrDataFrame(packagePath=packagePath, csvErrFile = csvErrFile, boolExtdata=boolExtdata);
  exportPackageSysdata(packagePath=packagePath,errorcodes=errorcodes,getSysdataErrorEntry=getSysdataErrorEntry,getSysdataObject=getSysdataObject,fillParamCheckResult=fillParamCheckResult,...)
}

#  Version 1.0
#  Date: 20161217
#' @name exportPackageSysdata
#' @title exportPackageSysdata
#' @family MaintenananceSysdata
#' @description exportPackageSysdata creates 'sysdata.rda' file containing any objects for package
#' After creating file 'sysdata.rda' gets exported.
#' @aliases exportPackageSysdata
#' @export exportPackageSysdata
#' @import devtools
#' @author Johannes Anwander
#' @param packagePath represents path to package
#' @param ... represents objects which get exported to 'sysdata.R'
#' @examples
#'
#' #USECASE 1: Simple export of different objects to package when working directory is not set to package
#' setwd("CoopGame");
#'
#' #CoopGame  <-Working Directory
#' #├── inst
#' #├── man
#' #├── man-roxygen
#' #├── R
#' #└── tests
#'
#'
#' a=5;
#' b=c;
#' funcDummy<-function(){print("HelloWorld!")}
#' createErrSysdataCoopGame(a,b,funcDummy)
#'
#' #USECASE 2: Simple export of different objects to package when working directory is not set to package
#'
#' setwd("SVN_REPO");
#'
#' #SVN_REPO  <-Working directory
#' #├── CommunicationGames
#' #├── CoopGame
#' #└── errcodes.csv
#'
#'
#' a=5;
#' b=c;
#' funcDummy<-function(){print("HelloWorld!")}
#' errorcodes=createErrSysdataCoopGame(a,b,funcDummy, packagePath="./CoopGame");
#' exportPackageSysdata(a,b,funcDummy,errorcodes,packagePath="./CoopGame");

exportPackageSysdata<-function( packagePath=".", ...){
  #backup path to old backup directory
  oldWDDir <- getwd();
  setwd(packagePath);

  pkgName<-basename(getwd())
  SYSDATA_OBJECTS<-list(...);
  #set path to package path


  SYSDATA_OBJECTS<-sapply(SYSDATA_OBJECTS,function(obj,ns){obj2=obj; environment(obj2)<-asNamespace(ns); eval.parent(substitute(obj<-obj2));},ns=pkgName)

  #Export sysdata.R with errcodes to corresponding package - condition: pathCoopGame needs to be set correctly
  devtools::use_data(pkg = packagePath,overwrite = TRUE,internal = TRUE,...=SYSDATA_OBJECTS );
  #set old working directory
  setwd(oldWDDir);
}

#  Version 1.0
#  Date: 20161217
#' @name importErrDataFrame
#' @title importErrDataFrame
#' @family MaintenananceSysdata
#' @description importErrDataFrame imports errorcodes and corresponding attributes out of specified csv file.
#' After creating file 'sysdata.rda' gets exported to 'CoopGame/R.
#' @aliases importErrDataFrame
#' @export importErrDataFrame
#' @import devtools
#' @author Johannes Anwander
#' @param pathCoopGame represents path to package CoopGame
#' @param fileErrCodes is name of file containing the error codes of package CoopGame.
#' @param sep represents character seperating CSV columns (on default ';')
#' @examples
#'
#' #USECASE 1: Simple import of error codes and messages out of csv file
#' setwd("CoopGame");
#'
#' #CoopGame  <-Working Directory
#' #├── inst
#' #|    └── extdata
#' #|        └── errorcodes.csv  <- csv file with errorcodes referenced by param fileErrCodes
#' #├── man
#' #├── man-roxygen
#' #├── R
#' #└── tests
#'
#'csvErrFile="errorcodes.csv"
#'
#'#or also, cause default value of csvErrFile:
#'importErrDataFrame()
#'
#'
#' #USECASE 2: Simple import of different objects when csv file is not within package
#'
#' setwd("SVN_REPO");
#'
#' #SVN_REPO  <-Working directory
#' #├── CommunicationGames
#' #├── CoopGame
#' #└── errorcodes.csv <- csv file with errorcodes referenced by param fileErrCodes
#'
#'importErrDataFrame(csvErrFile="errorcodes.csv",packagePath="./CoopGame",boolExtdata=FALSE)
#'
#'#or also, cause default value of csvErrFile:
#'importErrDataFrame(packagePath="./CoopGame",boolExtdata=FALSE)



importErrDataFrame<-function(csvErrFile=CSV_ERRFILE,packagePath=".",boolExtdata=TRUE){
  csvErrFilePath <- getCSVFilePath(packagePath = packagePath, fileName = csvErrFile, boolExtdata);
  errorcodes=readCSV(file=csvErrFilePath);
  return(errorcodes);
}

#  Version 1.0
#  Date: 20161217
#' @name maintainCSVDataWithFix
#' @title maintainCSVDataWithFix
#' @family MaintenananceSysdata
#' @description maintainCSVDataWithFix maintains any csv file with editor supplied by function fix
#' @aliases maintainCSVDataWithFix
#' @export maintainCSVDataWithFix
#' @author Johannes Anwander
#' @param fileName represents path to csv file
#' @param  newFile is on default  FALSE: Original csv file gets overwritten with maintained version if there are changes.
#'                              if TRUE: User is asked on prompt for new file name to save
#'                         if character: File gets saved with name supplied by user
#' @examples
#'
#' #USECASE 1: Maintanance of errorcodes and messages out of csv file within package
#' setwd("CoopGame");
#'
#' #CoopGame  <-Working Directory
#' #└── inst
#' #|    └── extdata
#' #|        └── errorcodes.csv  <- csv file with errorcodes referenced by param fileErrCodes
#' #├── man
#' #├── man-roxygen
#' #├── R
#' #└── tests
#'
#'maintainCSVDataWithFix(fileCSV="errorcodes.csv");
#'
#'#or also, cause default value of csvErrFile:
#'maintainCSVDataWithFix();
#'
#'
#' #USECASE 2: Simple import of different objects when csv file is not within package
#'
#' setwd("SVN_REPO");
#'
#' #SVN_REPO  <-Working directory
#' #├── CommunicationGames
#' #├── CoopGame
#' #└── errorcodes.csv <- csv file with errorcodes referenced by param fileErrCodes
#'
#' maintainCSVDataWithFix<-function(fileCSV="errorcodes.csv",packagePath="./CoopGame",boolExtdata=FALSE)

maintainCSVDataWithFix<-function(fileCSV=CSV_ERRFILE,packagePath=".",newFile=FALSE, sep=CSV_SEPERATOR, boolExtdata=TRUE){
  #Path either <fileCSV> when boolExtdate false else path is <packagePath>/inst/extdata/<fileCSV>
  filePath<-getCSVFilePath(packagePath = packagePath, fileName = fileCSV, boolExtdata);
  errcodes<-readCSV(filePath);
  errcodes[is.na(errcodes)]<-"";
  errcodesTmp<-fix(errcodes);
  #Path for output when <boolExdata> false either <CSV_ERRFILE> or <newFile> (is.character)
  #                when <boolExdata> true either <packagePath>/inst/extdata/<fileCSV> or <packagePath>/inst/extdata/<newFile>
  outputFileName<-fileCSV;
  if(is.character(newFile)){
    outputFileName <-newFile;
  }else if(newFile==TRUE){
    tmpNewFileName<- readline(prompt="Please enter file name to save (Hit enter for original)")
    if(tmpNewFileName!=""){
      outputFileName=tmpNewFileName;
    }
  }
  outputFilePath<-getCSVFilePath(packagePath = packagePath, fileName=outputFileName, boolExtdata);

  if(!identical(errcodes,errcodesTmp)){
    writeCSV(errcodesTmp,file = outputFilePath,sep = sep)
  }else{
    print("No changes")
  }
}


#  Version 1.0
#  Date: 20161217
#' @name exportEmptyErrFile
#' @title exportEmptyErrFile
#' @family MaintenananceSysdata
#' @description exportEmptyErrFile creates new empty csv file with column names according to fields to be maintained for error codes
#' @aliases exportEmptyErrFile
#' @export exportEmptyErrFile
#' @author Johannes Anwander
#' @param file represents: in case boolExtdata is set   'FALSE': Path under which newly created csv file gets stored
#'                                                      'TRUE':  Name file gets stored in extdata directory of package
#' @param boolExtdata is boolean variable, when         'TRUE':  CSV file gets created in extdata directory of package
#'                                         when default 'FALSE': CSV file get created according to referenced path by param 'csvErrFile'
#' @param errCodesStart  First number where error code range should start
#' @param errCodesEnd    Last number where error code range should end
#' @examples
#'
#' #USECASE 1: C csv file within package
#' setwd("CoopGame");
#'
#' #CoopGame  <-Working Directory
#' #└── inst
#' #|    └── extdata
#' #|        └── errorcodes.csv  <- csv file with errorcodes referenced by param fileErrCodes which will be created
#' #├── man
#' #├── man-roxygen
#' #├── R
#' #└── tests
#'
#'exportEmptyErrFile(fileCSV="errorcodes.csv");
#'
#'#or also, cause default value of csvErrFile:
#'exportEmptyErrFile();
#'
#'
#' #USECASE 2: Simple import of different objects when csv file is not within package
#'
#' setwd("SVN_REPO");
#'
#' #SVN_REPO  <-Working directory
#' #├── CommunicationGames
#' #├── CoopGame
#' #└── errorcodes.csv <- csv file with errorcodes referenced by param fileErrCodes
#'
#'exportEmptyErrFile(fileCSV="./errorcodes.csv",packagePath="./CoopGame",boolExtdata=FALSE);
#'
#'#or also, cause default value of csvErrFile:
#'exportEmptyErrFile(packagePath="./CoopGame",boolExtdata=FALSE);

exportEmptyErrFile<-function(csvErrFile=CSV_ERRFILE,packagePath=".",errCodesStart=numeric(),errCodesEnd=numeric(),boolExtdata=TRUE){
  filePath=getCSVFilePath(packagePath = packagePath, fileName = csvErrFile, boolExtdata);
  errDataFrame=getEmptyErrDataFrame(errCodesStart,errCodesEnd);
  writeCSV(errDataFrame,filePath);
}

#FIRST SECTION END - maintenance functions

#SECOND SECTION START - helper functions functions
#Helper functions: Reads csv file
readCSV<-function(file,sep=CSV_SEPERATOR){
  tmpFrame<-read.csv(file,sep=sep)
  #First column represents ID (numeric)
  tmpFrame[,1]<-sapply(tmpFrame[,1],as.numeric)
  tmpFrame[,2:length(tmpFrame)]<-sapply(tmpFrame[,2:length(tmpFrame)],as.character)
  return(tmpFrame);
}

#Helper functions: writes to csv file
writeCSV<-function(object,file,sep=CSV_SEPERATOR){
  # write.csv(object,file = file, row.names=FALSE, sep = ";")
  write.table(object, file=file, append=FALSE, quote=FALSE, sep=sep,row.names=FALSE)
}

#Helper functions: Returns path of csv file in dependency of boolExtdata.
#If boolExtdata is TRUE:  The csv file is within the directory inst/extdata of the specified package (ref. by packagePath)
#                         with name specified by fileName
#                  FALSE: The csv file path is only specified fileName
getCSVFilePath<-function(packagePath,fileName,boolExtdata){
  if(boolExtdata){
    filePath<-getExtdataFilePath(packagePath = packagePath,fileName = fileName);
  }else{
    filePath<-fileName;
  }
  return(filePath);
}

#Helper functions: Returns path of csv file within the directory inst/extdata of the specified package (ref. by packagePath)
getExtdataFilePath<-function(packagePath, fileName){
  return(paste(packagePath,"/","inst/extdata/",fileName,sep=""))
}

#Helper functions: Returns empty data frame for errorcodes within specified number range (errCodeStart, and errCodeEnd)
getEmptyErrDataFrame<-function(errCodesStart,errCodesEnd){
  numberOfEntries=errCodesEnd-errCodesStart+1;
  errorCodes=c(errCodesStart:errCodesEnd);
  characterCol=rep(" ",numberOfEntries);
  return(data.frame(errCode=errorCodes,errMessage=characterCol,description=characterCol,solution=characterCol,refFunction=characterCol));
}

#SECOND SECTION END - helper functions functions

