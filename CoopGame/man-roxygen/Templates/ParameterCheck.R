#' <%#############################################################################################################################################################################%>
#' <%# Rd-Files Roxygen-Template for Parameter Check Functions                                                                                                                    %>
#' <%# Version: 1.0                                                                                                                                                               %>
#' <%# Date: 20170107                                                                                                                                                             %>
#' <%# Author: Johannes Anwander                                                                                                                                                  %> 
#  <%#############################################################################################################################################################################%>
#' <%#                                                                                                                                                                            %>
#' <%# USAGE:                                                                                                                                                                     %> 
#' <%#  Include template by:                   #' @template Templates/ParameterChecks                                                                                             %>
#' <%#  Include tempate variables by scheme:   #' @templateVar <VAR_NAME> <VAR_VALUE>                                                                                             %>
#' <%#  Add description in source file:        #' @description <AUTHOR>                                                                                                           %>
#' <%#  Add export in source file:             #' @export <PARAMETERCHECK_NAME>                                                                                                   %>
#' <%#  Add examples in source:                #' @examples <EXAMPLES>                                                                                                            %>                                                                                                                                                                                                                                                                                                                                                    %>
#' <%# Template Variables of this template:                                                                                                                                       %>
#' <%#  PARAMETERCHECK_NAME - Name of the corresponding parameter check function                                                                                                  %>
#' <%#                                                                                                                                                                            %>
#' <%#                                                                                                                                                                            %>
#' <%#############################################################################################################################################################################%>
#' <%#                                                                                                                                                                            %>
#' <%#PREPARATION STEPS                                                                                                                                                           %>
#' <%#                                                                                                                                                                            %>
#' <%#SECTION for retrieving error codes for specific parameter check function and their number - START                                                                           %>
#' <% errorObjects=subset(SYSDATA_OBJECTS$errorcodes,refFunction==PARAMETERCHECK_NAME & errMessage!="")                                                                           %>
#' <% numberErrorObjects=nrow(errorObjects)                                                                                                                                       %>
#' <%#SECTION for retrieving error codes for specific parameter check function and their number - END                                                                             %>
#' <%#                                                                                                                                                                            %>
#' <%#SECTION for generating variable 'OUTPUT_ERRORCODES' - START                                                                                                                 %>
#' <%#'OUTPUT_ERRORCODES' contains output string for table with the attributes                                                                                                    %>
#' <% OUTPUT_ERRORCODES=""                                                                                                                                                        %>
#' <% OUTPUT_ERRORCODES=paste(OUTPUT_ERRORCODES,"\\tabular{lll}{",sep="")                                                                                                         %>
#' <% OUTPUT_ERRORCODES=paste(OUTPUT_ERRORCODES,"\\strong{Error Code} \\tab \\strong{Message} \\cr ",sep="")                                                                      %> 
#' <% for(i in 1:numberErrorObjects){ OUTPUT_ERRORCODES=paste(OUTPUT_ERRORCODES,errorObjects[i,]$errCode," \\tab ",errorObjects[i,]$errMessage," \\cr ",sep="") }                 %> 
#' <% OUTPUT_ERRORCODES=paste(OUTPUT_ERRORCODES,"}",sep="")                                                                                                                       %> 
#' <%#SECTION for generating variable 'OUTPUT_ERRORCODES' - END                                                                                                                   %> 
#' <%#                                                                                                                                                                            %>
#' <%#############################################################################################################################################################################%>
#' 
#' <%#TEMPLATE OUTPUT %>
#' 
#' @title Parameter Function <%=PARAMETERCHECK_NAME%>
#' @name <%=PARAMETERCHECK_NAME%>
#' @family ParameterChecks_CoopGame
#' @section Error Code Ranges:
#' Error codes and messages shown to user if error on parameter check occurs
#'  <%= cat(OUTPUT_ERRORCODES) %>





