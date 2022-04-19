##' Samples from the posterior distribution of BP variance
##' components
##'
##'
##' Samples from the posterior
##' 
##' 
##' \itemize{
##' \item \code{BP_typ} : type of BP measure (diastolic (\code{dia}) or systolic (\code{sys}))
##' \item \code{tt_htn} : if TRUE, variance components for the subpopulation of patients treated for HTN
##' \item \code{age} : age, by year
##' \item \code{sex} : sex, 1 for men, 2 for women
##' \item \code{samp} : number of the sample, ordered by chain (i.e. 1-1000 : first chain; 1001:2000, second chain...). 
##' \item \code{s_i} : estimated standard deviation for BP between individuals
##' \item \code{s_v} : estimated standard deviation for BP between within individual, between visits
##' \item \code{s_m} : estimated standard deviation for BP between within individual, within visits, between measures
##' }
##' @docType data
##' @keywords datasets
##' @name  dt_post_samp 
##' @usage data( dt_post_samp )
##' @format A data frame with  2368000  rows and  8  variables
"dt_post_samp"
