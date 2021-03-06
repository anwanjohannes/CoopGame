% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MaintenanceSysdata.R
\name{importErrDataFrame}
\alias{importErrDataFrame}
\title{importErrDataFrame}
\usage{
importErrDataFrame(csvErrFile = CSV_ERRFILE, packagePath = ".",
  boolExtdata = TRUE)
}
\arguments{
\item{pathCoopGame}{represents path to package CoopGame}

\item{fileErrCodes}{is name of file containing the error codes of package CoopGame.}

\item{sep}{represents character seperating CSV columns (on default ';')}
}
\description{
importErrDataFrame imports errorcodes and corresponding attributes out of specified csv file.
After creating file 'sysdata.rda' gets exported to 'CoopGame/R.
}
\examples{

#USECASE 1: Simple import of error codes and messages out of csv file
setwd("CoopGame");

#CoopGame  <-Working Directory
#├── inst
#|    └── extdata
#|        └── errorcodes.csv  <- csv file with errorcodes referenced by param fileErrCodes
#├── man
#├── man-roxygen
#├── R
#└── tests

csvErrFile="errorcodes.csv"

#or also, cause default value of csvErrFile:
importErrDataFrame()


#USECASE 2: Simple import of different objects when csv file is not within package

setwd("SVN_REPO");

#SVN_REPO  <-Working directory
#├── CommunicationGames
#├── CoopGame
#└── errorcodes.csv <- csv file with errorcodes referenced by param fileErrCodes

importErrDataFrame(csvErrFile="errorcodes.csv",packagePath="./CoopGame",boolExtdata=FALSE)

#or also, cause default value of csvErrFile:
importErrDataFrame(packagePath="./CoopGame",boolExtdata=FALSE)
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
  \code{\link{maintainCSVDataWithFix}}
}

