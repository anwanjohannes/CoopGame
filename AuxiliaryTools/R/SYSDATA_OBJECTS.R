#' @title SYSDATA_OBJECTS
#' @rdname SYSDATA_OBJECTS
#' @name SYSDATA_OBJECTS
#' @family SYSDATA_OBJECTS
#' @family MaintenananceSysdata
#' @description A list containtaining the objects which are exported to sysdata.rda of corresponding package by function \code{\link{exportErrSysdata}}.
#' List items can be accessed for internal usage in script file of package.
#' @format A list of objects which are list items
#' \tabular{lll}{
#' \strong{List items:}                                \tab \strong{Short Description} \cr
#' \code{\link{SYSDATA_OBJECTS$errorcodes}}            \tab Dataset containtaining the error codes entries \cr
#' \code{\link{SYSDATA_OBJECTS$getSysdataErrorEntry}}  \tab Function to retrieve specified whole error entry or single attribute \cr
#' \code{\link{SYSDATA_OBJECTS$getSysdataObject}}      \tab Function to retrieve object out of sysdata.rda\cr
#' \code{\link{SYSDATA_OBJECTS$fillParamCheckResult}}  \tab Function to fill error object with error code and message\cr
#' }
NULL

#' @title Error codes and messages of paramater checks executed
#' @rdname errorcodes
#' @name SYSDATA_OBJECTS$errorcodes
#' @family SYSDATA_OBJECTS
#' @description A dataset containtaining the error codes, messages, descriptions, solutions and references to function each belongs to.
#' @format A data frame based on the data of inst/extdata/errorcodes.csv file within corresponding package.
#' \describe{
#' \item{\strong{errCode}}{ Field for number which identifies error entry}
#' \item{\strong{errMessage}}{ Field for message shown to user when error is detected by parameter check}
#' \item{\strong{description}}{ Field for description of error}
#' \item{\strong{solution}}{ Field for possible solution which hints at how to handle error }
#' \item{\strong{refFunction}}{ Field which references parameter check function where error entry belongs to}
#' }
#' @examples
#' #USAGE in script within package COOP_GAME - e.g. selecting error entry where errCode is 1000:
#' SYSDATA_OBJECTS$errorcodes[SYSDATA_OBJECTS$errorcodes[,"errCode"]==1000,]
NULL

#  Version 1.0
#  Date: 20170103
#' @name SYSDATA_OBJECTS$getSysdataErrorEntry
#' @title getSysdataErrorEntry
#' @family MaintenananceSysdata
#' @family SYSDATA_OBJECTS
#' @description getSysdataErrorEntry returns entry for specified error code
#' @aliases getSysdataErrorEntry
#' @author Johannes Anwander
#' @param errCode references specific error entry
#' @param SYSDATA_OBJECTS  list containing objects which gets exported  sysdata.rda
#' @examples
#'
#' #USAGE in script within package COOP_GAME:
#' SYSDATA_OBJECTS$getSysdataErrorEntry(errCode = 1000, packageName = "CoopGame")
#'
#' #Output:
#' #errCode                         errMessage description solution              refFunction
#' #1    1000 Game vector A is invalid as 'NULL'        <NA>     <NA> stopOnInvalidGameVectorA

getSysdataErrorEntry<-function(errCode,attributeName="",packageName=getPackageName(), SYSDATA_OBJECTS=getFromNamespace("SYSDATA_OBJECTS",ns=packageName)){
  retVal=FALSE;
  execCommand="SYSDATA_OBJECTS$errorcodes[SYSDATA_OBJECTS$errorcodes$errCode==errCode,]";
  if(attributeName!="" && is.character(attributeName)){
    execCommand=paste(execCommand,"$",attributeName,sep="");
  }
  retVal=eval(parse(text=execCommand));
  return(retVal);
}


#  Version 1.0
#  Date: 20170103
#' @name SYSDATA_OBJECTS$getSysdataObject
#' @title getSysdataObject
#' @family getSysdataObject
#' @family SYSDATA_OBJECTS
#' @description getSysdataObjects returns list object of SYSDATA_OBJECTS with specified object name.
#' @aliases getSysdataObject
#' @author Johannes Anwander
#' @param errCode references specific error entry
#' @param SYSDATA_OBJECTS  list containing the objects which gets exported to sysdata.rda
#' @examples
#'
#' #USAGE in script within package COOP_GAME - e.g. retrieve error object
#' SYSDATA_OBJECTS$getSysdataObject(objectName = "errorcodes", packageName = getPackageName())



getSysdataObject<-function(objectName="",packageName=getPackageName(), SYSDATA_OBJECTS=getFromNamespace("SYSDATA_OBJECTS",ns=packageName)){
  boolIndices=TRUE;

  if(objectName!=""){
    boolIndices=names(SYSDATA_OBJECTS)==objectName;
  }
  return(SYSDATA_OBJECTS[boolIndices]);
}


##Third Section: START
#  Version 1.0
#  Date: 20170107
#' @name SYSDATA_OBJECTS$fillParamCheckResult
#' @title fillParamCheckResult - fill error object with error code and message
#' #' @family SYSDATA_OBJECTS
#' @description fillParamCheckResult retrieves error message out of data frame \code{\link{SYSDATA_OBJECTS$errorcodes}} by errCode and fills error object with error message and code.
#' @param paramCheckObject 
#' @author Johannes Anwander
fillParamCheckResult<-function(paramCheckObject, errCode, packageName=getPackageName()){
  checkResult=paramCheckObject;
  checkResult$errCode=errCode;
  checkResult$errMessage=SYSDATA_OBJECTS$getSysdataErrorEntry(errCode,"errMessage",packageName);
  eval.parent(substitute(paramCheckObject<-checkResult));
}






