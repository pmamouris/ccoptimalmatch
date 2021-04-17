#' Data for matching cases with controls
#'
#' A dataset containing cases and controls using the Intego registry data.
#' The variables are as follows:
#'
#' \itemize{
#'   \item {cluster_case}: {each case forms a cluster with all poosible controls to be matched}
#'   \item {Patient_Id}: {Unique identifier for each patient}
#'   \item {case_control}: {binary, if case==Colorectal Cancer, else control}
#'   \item {case_ind}: {binary, if 1==case, else control}
#'   \item {JCG}: {Year of Contact}
#'   \item {entry_year}: {the year that the patient first entrered the database}
#'   \item {CI}: {Comorbidity Index. Count of chronic diseases before index data}
#'   \item {age_diff}: {difference of age between cases and controls}
#'   \item {fup_diff}: {difference of follow-up between cases and controls}
#'   \item {total_control_per_case}: {total controls that are available to be pooled per case}
#'   \item {freq_of_controls}: {how many times the control is available to be matched for different cases}
#' }
#'
#' @docType data
#' @keywords datasets
#' @name being_processed
#' @usage data(being_processed)
#' @format A data frame with 77110 rows and 11 variables
NULL

