#' Calculate Effective Shannon Index
#' 
#' Calculate Effective Shannon Index based on Holland and Kasperski 2025 (CJFAS)
#' Can output indices either at the individual vessel level coastwide (= all fish tickets associated with a particular vessel in a given year)
#' Or at the port level (= all fish tickets associated with a particular vessel and landed in a given IOPAC port group in a given year)
#' 
#' Can output indices either across species groups (for fishery diversification index)
#' Or across weeks of the year (for revenue diversification index)
#' 
#' @param mat data input in matrix form. Rows are vessels, columns are species groupings or weeks of the year, 
#' cell values are annual revenue for that vessel from that species grouping/week
#' @return Effective Shannon Index for each vessel in the input matrix
#' 
calculate_esi <- function(mat){
  
  # make a new df with annual % revenue from each species for each boat
  percent_boats <- mat/rowSums(mat, na.rm = T)
  percent_boats_mat <- as.matrix(percent_boats)

  # calculate `-(p*log(p))` for all cells `p` in the revenue matrix

  mat1 <- -(percent_boats_mat*log(percent_boats_mat))

  # sum these values
  rev_sums <- rowSums(mat1, na.rm = TRUE)

  # exponentiate
  esi <- as.matrix(exp(rev_sums))
  outdat <- as.data.frame(esi)
  colnames(outdat) <- "esi"
  
  return(esi)

 }