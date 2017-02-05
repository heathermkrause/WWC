#' 
#' Veracio example
#' July 23, 2016
#' 
#'
#' 
#' 


dw <- read.csv("Veracio Example Survey_2.csv")
head(dw)
sum(dw$Weight)
dim(dw)

#' To use a jacknife estimator of variance we need to find the values of 
#' weighted estimators obtained by dropping each observation. Since the
#' weights reflect both the number of subjects in the population in each
#' stratum and the number in the sample, dropping a subject changes the 
#' weights as a result of changing the number in the stratum in the sample.
#' (Okay, this could be expressed much better)
#' 
#' Let ${\tilde w_k}$ represent the jacknife weight when dropping an
#' observation from stratum $k$, let $w_k$ be the original
#' weight and let $n_k$ be the number of observations in stratum $k$. Then:
#' 
#' \[{\tilde w_k} = {w_k}\frac{{{n_k}}}{{{n_k} - 1}}\]
#' 
#' 


# Here is a small data set with post-stratification weights 
# proportional to 'N_k / n_k' in stratum k.
#
# Note that post-stratification weights are constant with strata.
#
# To illustrate the principle that a variable that is constant within
# strata has a jacknife estimated variance of 0, the variable 'Response2'
# is constant within strata.
#


zd <- read.table(header = TRUE, text = "
Response Gender Education Weight Response2
10       M      L         20     1
10       M      L         20     1 
10       M      L         20     1
10       M      H         30     2
12       M      H         30     2
14       M      H         30     2
16       M      H         30     2 
11       F      L         25     3
11       F      L         25     3
11       F      L         25     3
11       F      L         25     3
20       F      H         35     4
20       F      H         35     4
20       F      H         35     4
")
zd

wtd_mean <- function(x, w) sum(x*w) / sum(w)    

jk_wtd_means <- function(x, by, w, check = TRUE){
        # Value: vector of 'drop-one' estimates for jacknife estimator of SE
        #
        # x: response variable, a numeric vector
        # by: vector or list of vectors defining strata
        # w: numeric vector of sampling weights constant within each stratum
        # Check that weights are constant within levels of 'by'
        if(check) {
                constant <- function(z) length(unique(z)) == 1 
                constant_by <- function(z, by) all(sapply(split(z, by), constant))
                if(!constant_by(w, by)) stop("w must constant within strata")
        }
        byi <- interaction(by)
        ns <- ave(x, byi, FUN = length)
        force(x)
        force(w)
        wtd_mean_drop_i <- function(i) {
                ns_drop <- ave(x[-i], byi[-i], FUN = length)
                w_drop <- w[-i] * ns[-i] / ns_drop
                wtd_mean(x[-i], w_drop)
        }
        sapply(seq_along(x), wtd_mean_drop_i)
}

with(zd, 
     jk_wtd_means(Response, list(Gender,Education), Weight)
)

jk_wtd_mean_se <- function(x, by = rep(1, length(x)), w = rep(1, length(x))) {
        # Value: jacknife estimator of the standard error of a weighted mean
        # using post-stratification
        #
        # x: response variable, a numeric vector
        # by: vector or list of vectors defining strata
        # w: numeric vector of sampling weights constant within each stratum
        #
        theta_hat <- wtd_mean(x, w)
        byi <- interaction(by)
        theta_hat_i <- jk_wtd_means(x, byi, w)
        ns <- ave(x, byi, FUN = length)
        sqrt(sum((theta_hat_i - theta_hat)^2 * (ns-1) / ns))
}

# Testing:

attach(zd)
     jk_wtd_mean_se(Response, list(Gender,Education), Weight)
     # SE of a variable that is constant within strata
     jk_wtd_mean_se(Response2, list(Gender,Education), Weight) 
detach(zd)

# Veracio example

attach(dw)
        jk_wtd_mean_se(Response, list(Gender,Education), Weight)
        # The following is the SE of the mean if the reponse were obtained from a SRS
        jk_wtd_mean_se(Response)
        # same as:
        sd(Response)/sqrt(length(Response))
detach(dw)

