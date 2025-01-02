#' Generate matrix of fishery-specific revenue by vessel or port
#'
#' Generate matrix of vessel or port-level participation in each species group
#' Based on first ~100 lines of the participation_network_crabyear() function
#'
#' @param tickets fish tickets data frame
#' @param matrix_scale whether to construct a matrix for the entire coast (`"coastwide"`) or specific port (`"port"`)
#' @param pcid_choose specify an IOPAC port group (only needs to be specified if `matrix_scale == "port`; will produce an error otherwise)
#' @param year_choose Specify a year


revenue_matrix <- function(tickets, pcid_choose = NA, year_choose = NA){
  if(matrix_scale == "coastwide" & !is.na(pcid_choose)){
    stop("ERROR: IOPAC port should not be specified for coastwide matrices")
  }
  if(matrix_scale == "port" & is.na(pcid_choose)){
    stop("ERROR: Specify an IOPAC port for port-level matrix construction")
  }
  
  
}