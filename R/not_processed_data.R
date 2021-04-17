#' Not-processed data for matching cases with controls
#'
#' A dataset containing cases and controls using the Intego registry data. But not the final dataset.
#' The variables are as follows:
#'
#' \itemize{
#'   \item {Patient_Id}: {Unique identifier for each patient}
#'   \item {JCG}: {Year of Contact}
#'   \item {Birth_Year}: {Patient's year of birth}
#'   \item {Gender}: {Patient's Gender}
#'   \item {Practice_Id}: {Patient's general practice}
#'   \item {case_control}: {binary, if case==Colorectal Cancer, else control}
#'   \item {entry_year}: {the year that the patient first entrered the database}
#'   \item {fup_diff}: {difference of follow-up between cases and controls}
#'   \item {CI}: {Comorbidity Index. Count of chronic diseases before index data}
#' }
#'
#' @docType data
#' @keywords datasets
#' @name not_processed
#' @usage data(not_processed)
#' @format A data frame with 656506 rows and 9 variables
NULL

