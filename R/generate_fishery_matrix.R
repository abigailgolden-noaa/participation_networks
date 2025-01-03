#' Generate matrix of fishery-specific revenue by vessel or port
#'
#' Generate matrix of vessel or port-level participation in each species group
#' Based on first ~100 lines of the participation_network_crabyear() function
#'
#' @param tickets fish tickets data frame
#' @param matrix_scale whether to construct a matrix for the entire coast (`"coastwide"`) or specific port (`"port"`)
#' @param pcid_choose specify an IOPAC port group (only needs to be specified if `matrix_scale == "port`; will produce an error otherwise)
#' @param year_choose Specify a year
#' @param min_rev the minimum revenue (in dollars) generated from all fisheries for a given vessel in a given year
#' @param min_rev_indiv the minimum revenue (in dollars) generated from any one fishery for a given vessel in a given year
#' @return matrix of fishery-specific revenue in a given year, where each row is a vessel,
#' each column is a species grouping, and values are revenue


revenue_matrix <- function(tickets, matrix_scale = "coastwide", pcid_choose = NA, year_choose = NA,
                           min_rev = 1, min_rev_indiv = 1){
  
  # make sure that the appropriate inputs are included
  if(matrix_scale == "coastwide" & !is.na(pcid_choose)){
    stop("ERROR: An IOPAC port should not be specified for coastwide matrices")
  }
  if(matrix_scale == "port" & is.na(pcid_choose)){
    stop("ERROR: An IOPAC port must be specified for port-level matrix construction")
  }
  
  # filter data by year (coastwide matrices) or year and port (port-level matrices)
  if(matrix_scale == "coastwide" & is.na(pcid_choose)){
    tickets <- dplyr::filter(tickets, year %in% year_choose)
  }
  
  if(matrix_scale == "port" & (!is.na(pcid_choose))){
    tickets <- dplyr::filter(tickets, year %in% year_choose & IOPAC %in% pcid_choose)
  }
  
  # get total number of boats
  fleet_size <- length(unique(filter(tickets, drvid!='NONE')$drvid))
  
  # create a df with 2 columns: SPGRPN2 and max_boats, the maximum boats that participated in the fishery during the specified year(s)
  n_boats <- tickets %>% filter(drvid!='NONE') %>%
    group_by(year, SPGRPN2) %>%
    summarise(n_boats = length(unique(drvid)), .groups = "drop") %>%
    group_by(SPGRPN2) %>%
    summarise(max_boats = max(n_boats), .groups = "drop")
  
  # create a df where each column is a SPGRPN2, and values represent the total revenue for a boat in a year from that SPGRPN2
  boats <- tickets %>% filter(drvid != 'NONE') %>%
    group_by(drvid, SPGRPN2, year) %>% 
    summarise(revenue = sum(adj_afi_revenue), .groups = "drop") %>%
    pivot_wider(names_from=SPGRPN2, values_from=revenue, values_fill = NA)
  boats <- as.data.frame(boats)
  rownames(boats) <- paste(boats$drvid, boats$year, sep="_")
  boats$drvid <- NULL
  boats$year <- NULL
  
  # if statement to handle issue of filters leading to no boats that meet rev cutoffs
  if(is.null(nrow(boats))==TRUE){
    return(NA)
  }
  
  # remove boats that don't generate at least min_rev in revenue annually
  if(any(rowSums(boats,na.rm=T)<min_rev)){boats <- boats[-which(rowSums(boats, na.rm=T)<min_rev),]}
  
  
  ##### for each vessel remove fisheries with below min_rev_indiv #####
  # for each boat, set filtered_boats table value as "NA" for fisheries that don't generate at least min_rev_indiv in revenue annually.
  boats_mat <- as.matrix(boats)
  boats_mat[which(boats_mat<min_rev_indiv)] <- NA
  filtered_boats <- as.data.frame(boats_mat, row.names=rownames(boats_mat), col.names=colnames(boats_mat))
  
  return(filtered_boats)
}