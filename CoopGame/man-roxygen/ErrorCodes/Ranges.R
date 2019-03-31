#' <%#############################################################################################################################################################################%>
#' <%# Rd-Files Roxygen-Template for Error Code Ranges                                                                                                                            %>
#' <%# Version: 1.0                                                                                                                                                               %>
#' <%# Date: 20170107                                                                                                                                                             %>
#' <%# Author: Johannes Anwander                                                                                                                                                  %> 
#  <%#############################################################################################################################################################################%>
#' <%#                                                                                                                                                                            %>
#' <%# USAGE:                                                                                                                                                                     %> 
#' <%#  Include template by:                   #' @template ErrorCodes/Ranges                                                                                                     %>
#' <%#  Include tempate variables by scheme:   #' @templateVar <VAR_NAME> <VAR_VALUE>                                                                                             %>
#' <%#                                                                                                                                                                            %>
#' <%# Template Variables of this template:                                                                                                                                       %>
#' <%#  none                                                                                                                                                                      %>
#' <%#                                                                                                                                                                            %>
#' <%#############################################################################################################################################################################%>
#' <%#                                                                                                                                                                            %>
#' <%#                                                                                                                                                                            %> 
#' <%# HELPER FUNCTIONS %>                                                                                                                                                        %>        
#' <% getErrorRangeOfFunction <- function(funcName){ errorcodes=subset(SYSDATA_OBJECTS$errorcodes,refFunction==funcName)$errCode; return(c(min(errorcodes), max(errorcodes))) }   %>
#' <% getErrorRowOutput <- function(funcName){ er=getErrorRangeOfFunction(funcName); return(paste("\\code{\\link{",funcName,"} }      \\tab ",er[1],"-",er[2]," \\cr ",sep="")) } %>
#' <% getErrorRowsOutput <- function(paramFuncs){ out=""; for(o in paramFuncs){ out=paste(out, getErrorRowOutput(o)) }; return(out) }                                             %>
#' <%#                                                                                                                                                                            %>
#' <%#PREPARATION STEPS                                                                                                                                                           %>
#' <%#                                                                                                                                                                            %>
#' <%#SECTION for retrieving names of parameter check functions - START                                                                                                           %>
#' <% paramFunctions <- unique(SYSDATA_OBJECTS$errorcodes$refFunction)                                                                                                            %>
#' <% ignoreNames <- c(NA,"")                                                                                                                                                     %>
#' <% paramFunctions <- paramFunctions[! paramFunctions %in% ignoreNames ]                                                                                                        %>
#' <%#SECTION for retrieving names of parameter check functions - END                                                                                                             %>
#' <%#                                                                                                                                                                            %>
#' <%#SECTION for generating variable 'OUTPUT_ERRORCODE_RANGES' - START                                                                                                           %>
#' <% OUTPUT_ERRORCODE_RANGES=""                                                                                                                                                  %>
#' <% OUTPUT_ERRORCODE_RANGES=paste(OUTPUT_ERRORCODE_RANGES,"\\tabular{lll}{"                                                                               ,sep="")              %>
#' <% OUTPUT_ERRORCODE_RANGES=paste(OUTPUT_ERRORCODE_RANGES,"\\strong{Function Name}                        \\tab \\strong{Error Code Range} \\cr "         ,sep="")              %>
#' <%# OUTPUT_ERRORCODE_RANGES=paste(OUTPUT_ERRORCODE_RANGES,"\\code{\\link{stopOnInvalidGameVectorA}}       \\tab 1000-1009 \\cr "                         ,sep="")              %>
#' <%# OUTPUT_ERRORCODE_RANGES=paste(OUTPUT_ERRORCODE_RANGES,getErrorRowOutput(paramFunctions[1])                                                           ,sep="")              %>
#' <% OUTPUT_ERRORCODE_RANGES=paste(OUTPUT_ERRORCODE_RANGES,getErrorRowsOutput(paramFunctions)                                                              ,sep="")              %>
#' <% OUTPUT_ERRORCODE_RANGES=paste(OUTPUT_ERRORCODE_RANGES,"}"                                                                                             ,sep="")              %>
#' <%#SECTION for generating variable 'OUTPUT_ERRORCODE_RANGES' - END                                                                                                             %>
#' <%#                                                                                                                                                                            %>
#' <%#############################################################################################################################################################################%>
#' 
#' <%#TEMPLATE OUTPUT %>
#' @section Parameter Check Functions:
#' References and overview of supplied parameter check functions and their error code range.
#' <%=cat(OUTPUT_ERRORCODE_RANGES) %>
#' 
#' 
