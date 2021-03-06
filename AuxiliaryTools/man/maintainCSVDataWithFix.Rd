% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MaintenanceSysdata.R
\name{maintainCSVDataWithFix}
\alias{maintainCSVDataWithFix}
\title{maintainCSVDataWithFix}
\usage{
maintainCSVDataWithFix(fileCSV = CSV_ERRFILE, packagePath = ".",
  newFile = FALSE, sep = CSV_SEPERATOR, boolExtdata = TRUE)
}
\arguments{
\item{newFile}{is on default  FALSE: Original csv file gets overwritten with maintained version if there are changes.
     if TRUE: User is asked on prompt for new file name to save
if character: File gets saved with name supplied by user}

\item{fileName}{represents path to csv file}
}
\description{
maintainCSVDataWithFix maintains any csv file with editor supplied by function fix
}
\examples{

#USECASE 1: Maintanance of errorcodes and messages out of csv file within package
setwd("CoopGame");

#CoopGame  <-Working Directory
#└── inst
#|    └── extdata
#|        └── errorcodes.csv  <- csv file with errorcodes referenced by param fileErrCodes
#├── man
#├── man-roxygen
#├── R
#└── tests

maintainCSVDataWithFix(fileCSV="errorcodes.csv");

#or also, cause default value of csvErrFile:
maintainCSVDataWithFix();


#USECASE 2: Simple import of different objects when csv file is not within package

setwd("SVN_REPO");

#SVN_REPO  <-Working directory
#├── CommunicationGames
#├── CoopGame
#└── errorcodes.csv <- csv file with errorcodes referenced by param fileErrCodes

maintainCSVDataWithFix<-function(fileCSV="errorcodes.csv",packagePath="./CoopGame",boolExtdata=FALSE)
}
\author{
Johannes Anwander
}
\seealso{
Other MaintenananceSysdata: \code{\link{SYSDATA_OBJECTS$getSysdataErrorEntry}},
  \code{\link{SYSDATA_OBJECTS}},
  \code{\link{exportEmptyErrFile}},
  \code{\link{exportErrSysdata}},
  \code{\link{exportPackageSysdata}},
  \code{\link{importErrDataFrame}}
}

