culculate_NPV <- function(vProfit, S = 100000) {
        
        profit <- vProfit
        m_0 = mean(profit)
        s2_0 = var(profit)
        n_0 = length(profit)
        v_0 = n_0 - 1
        
        phi = rgamma(S, v_0/2, s2_0*v_0/2)
        sigma = 1/sqrt(phi)
        mu = rnorm(S, mean=m_0, sd=sigma/(sqrt(n_0)))
        Y = rnorm(S, mu, sigma)
        Years <- 1/(length(which(Y < 0))/length(Y))
        sd_profit <- sd(profit)*Years
        
        NPV = rnorm(S, mu, sigma)*Years
        
        return(NPV)
}