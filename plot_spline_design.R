pdf('Boundary_splines.pdf', width=14, height=7)
flag <- dplyr::between(ages, min(df$ageori), max(df$ageori))
matplot(ages[flag],
        spline.basis[flag,c(1:3,22:24)], type='l', lty=1, xlim=c(0,120),
        xlab='Age',ylab='Value',col=c(4,4,4,2,2,2)
        )
matplot(ages[dplyr::between(ages, -1, min(df$ageori))],
        spline.basis[dplyr::between(ages, -Inf, min(df$ageori)),c(1:3,22:24)], type='l', lty=2,
        add=TRUE,col=c(4,4,4,2,2,2)
)
matplot(ages[dplyr::between(ages, max(df$ageori), +1000)],
        spline.basis[dplyr::between(ages, max(df$ageori), +1000),c(1:3,22:24)], type='l', lty=2,
        add=TRUE,col=c(4,4,4,2,2,2)
)
abline(lty='dotted', v=range(df$ageori))
points(y=rep(0,length(knot.list[[1]])),x=knot.list[[1]],pch='X')
dev.off()