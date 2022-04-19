##' Prevalence of hypertension (HT) corrected from sampling variability
##'
##' This function allows to calculate the prevalence of HT in a population
##' after correcting from variations due to blood pressure variability that occurs in an indivuidual
##' between visits and within visit.
##' For each individual, a correction factors adapted to scheme of BP prelevement (i.e. number of visits and number of measure within a visit, systolic or diastolic measure), age and sex, is calculated. These correction factor are derived from BP variances estimted from NHANES III datas. The corrections factors are then applied to indivuduals BP measures in the population under study to infer corrected HT prevalence.
##'
##'
##'
##' @param form A formula giving covariates by which HT prevalence is
##'   calculated. If `htn` is placed in the left hand side of
##'   `formula`, then variation of prevalence according to the
##'   covariates given in the right hand side of are estimated with a
##'   `glm` (`survey::svyglm` if `surv_des` is not NULL) model with
##'   quasibinomial distribution. Else, prevalence is tabulated
##'   according to the covariates given in the right hand side (simple
##'   tabulation or `survey::svyby` if `surv_des` is not NUL).
##' @param subpop A boolean covariate that defines a subpopulation
##'   over which to filter the calculation of prevalence. `subpop`
##'   must be given in a formula form, e.g. ~subpop. See Examples.
##' @param n_samp The number of posterior sample of the correction
##'   factor to be used in the estimation. Default (NULL) resumes to
##'   the maximum number of available posterior samples.
##' @param data_long Data frame giving the BP measurements of the
##'   population under study. Data must be given in a long format,
##'   e.g. one raw per BP measure (see `dt_nhanes` format).  The data
##'   frame must have the following columns: \itemize{
##'   \item{"id"}{Patient identifyer} \item{"age"}{Age of the patient, in years}
##'   \item{"sex"}{Sex of the patient} \item{"tt_htn"}{Boolean with
##'   value `TRUE` if the patient is under anti-hypertensive
##'   treatment} \item{"visit"}{Identifyer of the visit}
##'   \item{"BP_typ"}{type of BP prelevement, "dia" for diastolic,
##'   "sys" for systolic} \item{"bp"}{Value of blood pressure
##'   measurement} }
##' @param surv_des If applicable, the survey design of the study,
##'   specifyed with `svydesign` from `survey` package.
##' @param correct Boolean set to `TRUE` (the default) to correct
##'   prevalence.
##' @param tresh A data frame giving the BP thresholds that defines
##'   hypertension. Default to 140 for systolic BP, 90 for diastolic.
##' @return A data frame with the estimates of HT prevalance tabulated according to the covariates given in `form`
##' @author Edouard Chatignoux
##' @export
##' @examples
##' data(dt_nhanes)
##'
##' ## Uncorrected HT prevalence by sex
##' correct_htn(form= ~ sex,
##' data_long = dt_nhanes,
##' correct = FALSE)
##'
##' ## Corrected HT prevalence by sex
##' correct_htn(form=htn ~ sex,
##' data_long = dt_nhanes,
##' n_samp = 10,
##' correct = TRUE)
##'
##' ## Same using quasi-binomial glm
##' correct_htn(form= ~ sex,
##' data_long = dt_nhanes,
##' n_samp = 10,
##' correct = TRUE)
##'

correct_htn <- function (form = htn ~ 1,
                         subpop = NULL,
                         n_samp = 1000,
                         data_long = dt_nhanes,
                         surv_des = NULL,
                         correct = TRUE,
                         tresh = dplyr::tibble(BP_typ = c("sys", "dia"), BP_tresh = c(140, 90))) {

    data_long <- dplyr::as_tibble(data_long)
    if (is.null(n_samp))
        n_samp <- max(dt_post_samp$samp)
    if (!is.null(surv_des)) {
        surv_var <- c(all.vars(surv_des$call$ids), all.vars(surv_des$call$variables),
            all.vars(surv_des$call$probs), all.vars(surv_des$call$weights))
        data_long$w <- data_long[, all.vars(surv_des$call$weights)] %>%
            unlist
    }
    else {
        surv_des <- NULL
        surv_var <- c()
        data_long$w <- 1
    }
    vars <- all.vars(form)
    all_vars <- setdiff(unique(c(vars, all.vars(subpop), surv_var)),
                        "htn")
    if (is.null(formula.tools::lhs.vars(form))){
        glm <- FALSE
        form<-update(form, htn~.)
        }
    else
        glm <- (formula.tools::lhs.vars(form)=="htn")

    if (correct) {
        dt_ratio <- dt_post_samp %>% dplyr::filter(samp <= n_samp)
        data_long <- data_long %>% dplyr::group_by(id, BP_typ, visit) %>%
            dplyr::mutate(nb_meas = length(id)) %>% dplyr::group_by(id, BP_typ) %>%
            dplyr::mutate(nb_vis = length(unique(visit))) %>% dplyr::ungroup()
        mut_ratio <- . %>% dplyr::mutate(ratio = sqrt(s_i^2/(s_i^2 +
            s_v^2/nb_vis + s_m^2/nb_meas)))
    }
    else {
        dt_ratio <- dt_post_samp %>% dplyr::filter(samp == 1)
        mut_ratio <- . %>% dplyr::mutate(ratio = 1)
    }
    pb <- progress::progress_bar$new(total = n_samp, format = "(:spin) [:bar] :percent in :elapsed eta: :eta",
        force = T)
    res <- dt_ratio %>% dplyr::group_by(samp) %>% tidyr::nest() %>%
        dplyr::mutate(data = purrr::map(data, function(dt) {
            pb$tick()
            dt <- data_long %>%
              dplyr::left_join(dt, by = c("age", "sex", "BP_typ","tt_htn")) %>%
              mut_ratio %>%
              dplyr::left_join(tresh, by = "BP_typ") %>%
              dplyr::group_by(age, sex, BP_typ) %>%
              dplyr::mutate(bp_mean = sum(bp * w)/sum(w)) %>% dplyr::group_by(id, BP_typ) %>%
              dplyr::mutate(bp = mean(bp)) %>%
              dplyr::mutate(htn = (tt_htn | (bp_mean + ratio * (bp - bp_mean)) >= BP_tresh)) %>%
              dplyr::ungroup() %>% dplyr::mutate(cst = 1) %>%
              dplyr::group_by(!!!rlang::parse_exprs(unique(c("id", "age", "sex", all_vars)))) %>%
              dplyr::summarise_at(dplyr::vars(htn),max) %>% dplyr::ungroup()
            if (!is.null(surv_des)) {
                surv_des$call$data <- quote(dt)
                surv_des <- eval(surv_des$call)
                if (!is.null(subpop)) {
                  sub <- dt[, all.vars(subpop)] == 1
                  surv_des <- subset(surv_des, sub)
                }
                if (!glm)
                  tab <- survey::svyby(~all.vars(form)[1], form, design = surv_des,
                    svymean)
                else {
                  glm_fit <- survey::svyglm(form, design = surv_des,
                    family = "quasibinomial")
                  tab <- dplyr::tibble(beta = list(coefficients(glm_fit)),
                    V = list(vcov(glm_fit) %>% as.matrix))
                }
            }
            else {
                if (!is.null(subpop)) {
                  sub <- dt[, all.vars(subpop)] == 1
                  dt <- dt[sub, ]
                }
                if (!glm)
                    tab <- dt %>% dplyr::mutate(n = 1) %>%
                        dplyr::group_by_at(dplyr::vars(all.vars(form)[-1])) %>%
                    dplyr::summarise(htn = mean(htn), se = sqrt(htn *
                      (1 - htn)/length(n)), .groups = "keep")
                else {
                  glm_fit <- glm(form, data = dt, family = "quasibinomial")
                  tab <- dplyr::tibble(beta = list(coefficients(glm_fit)),
                    V = list(vcov(glm_fit) %>% as.matrix))
                }
            }
            tab
        })) %>% tidyr::unnest(data)
    if (glm) {
        inv.logit <- function(x) exp(x)/(1 + exp(x))
        beta <- res %$% beta %>% simplify2array
        if (!is.array(beta))
            beta <- mean(beta)
        else beta <- beta %>% apply(., 1, mean)
        V <- res %$% V %>% simplify2array
        if (!is.array(V))
            V <- mean(V)
        else V <- V %>% apply(., 1:2, mean) + tidyr::replace_na(cov(res %$%
                                                             beta %>% simplify2array %>% t), 0)
        if (!is.null(subpop)) {
          sub <- data_long[, all.vars(subpop)] == 1
          dt_p <- data_long[sub, ]
        }
        else{
          dt_p<-data_long
        }
        dt_p %<>% dplyr::mutate(cst = 1) %>% dplyr::select_at(dplyr::vars(all.vars(update(form,
            cst ~ .)))) %>% unique()
        X <- model.matrix(update(form, cst ~ .), data = dt_p)
        pred <- X %*% beta %>% as.vector
        se <- sqrt(diag(X %*% V %*% t(X)))
        res <- dt_p %>% dplyr::mutate(htn = pred, se = se, low = htn -
            1.96 * se, up = htn + 1.96 * se) %>% dplyr::mutate(se = se *
            exp(htn)/(exp(htn) + 1)^2) %>% dplyr::mutate_at(dplyr::vars(htn,
            low, up), inv.logit) %>% dplyr::select(-cst)
    }
    else {
        res <- res %>% dplyr::group_by(!!!(rlang:::parse_exprs(vars))) %>%
            dplyr::summarise(se = sqrt(sum(mean(se^2), var(htn), na.rm = T)),
                htn = mean(htn), low = htn - 1.96 * se, up = htn +
                  1.96 * se) %>% dplyr::select(!!!(rlang:::parse_exprs(vars)),
            htn, se, low, up)
    }
    res
}



