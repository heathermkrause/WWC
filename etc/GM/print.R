ht_est <- function(sam, pop, fmla){
        # assume
        strat.vars <- names(pop) %less% "N"
        sam <- merge(sam, pop, all = T) 
        sel.mf <- sam[strat.vars]
        strat.fmla <- formula(sel.mf)
        
        sam$n <- capply(1:nrow(sam), sam[strat.vars], length)
        sam$ht <- with(sam, N/n)
        ests <- model.frame(fmla, sam, na.action = na.include)
        n_ests <- length(ests)
        names_ests <- names(ests)
        ht <- sam$ht
        inds <- 1:nrow(sam)
        
        for(i in seq_len(n_ests)) {
                xx <- ests[[i]]
                xn <- names_ests[i]
                sam[[paste0(xn,'.est')]]  <- wtd_mean(xx,ht)
                sam[[paste0(xn,'.est.se')]]  <- jk_wtd_mean_se(xx,sel.mf,ht)
                #                sam$x_est.g <- capply(sam, sel.mf, with, wtd_mean(x,ht))
                sam[[paste0(xn,'.est.g')]]  <- 
                        capply(inds, sel.mf, function(ii) wtd_mean(xx[ii],ht[ii]))
                sam[[paste0(xn,'.est.g.se')]]  <- 
                        capply(inds, sel.mf, function(ii) 
                                jk_wtd_mean_se(xx[ii],sel.mf[ii,],ht[ii]))
                
                # sam$x_est.g <- capply(sam, sel.mf, with, jk_wtd_mean_se(x,sel.mf,ht))
                
        }
        up(sam, sel.mf)
}
system.time(
        zz<-ht_est(ds,dpop, ~Income)
)
