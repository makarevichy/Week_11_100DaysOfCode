library(psych)
pc <- principal(USJudgeRatings[ ,-1], nfactors = -1)
pc

fa.parallel(Harman23.cor$cov, n.obs = 302, fa = 'pc', show.legend = F)

PC <- principal(Harman23.cor$cov, nfactors = 2, rotate = 'none')
PC

rc <- principal(Harman23.cor$cov, nfactors = 2, rotate = 'varimax')
rc

pc <- principal(USJudgeRatings[ ,-1], nfactors = 1, scores = T)
head(pc$scores)
cor(USJudgeRatings$CONT, pc$scores)

rc <- principal(Harman23.cor$cov, nfactors = 2, rotate = 'varimax')
round(unclass(rc$weights), 2)

#Factorial analysis
covariances <- ability.cov$cov
correlations <- cov2cor(covariances)
correlations

fa.parallel(correlations, n.obs = 112, fa = 'both', n.iter = 100)

fa_c <- fa(correlations, nfactors = 2, rotate = 'none', fm = 'pa')
fa_c

fa.varimax <- fa(correlations, nfactors = 2, rotate = 'varimax', fm = 'pa')
fa.varimax

fa.promax <- fa(correlations, nfactors = 2, rotate = 'promax', fm = 'pa')
fa.promax
fa.plot(fa.promax, labels = rownames(fa.promax$loadings))
fa.diagram(fa.promax, simple = F)