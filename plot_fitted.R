library(tidyverse)
pdf('flex_diagnosis.pdf',width=14,height=7)
# df$fitted <- covar.list[[1]] %*% rowMeans(coefs[,1,-(1:5000)]) + rowMeans(REs[df$ID,1,-(1:5000)])
# names(df)
# df$fitted[is.na(df$Y)] <- NA
df_0 <- df %>% select(ageori, Y, ID) %>% mutate(type='Observed') %>% rename(value=Y)
x_test <- seq(0,120,by=0.1)
covar_test <- splines2::ibs(x_test, knots = knot.list[[1]],
                            Boundary.knots = boundary.knot,
                            degree = 2, intercept = TRUE) %>%
  cbind(1, .)
df_1 <- data.frame(value = covar_test %*% rowMeans(coefs[-(2:3),1,-(1:5000)]),
ageori = x_test,
type='Fitted',
ID=-1
)
df_2 <- data.frame(value = f_sshape(x_test,mode1,range_L1,range_R1)*2 + true_fixed_effect[1,2],
ageori = x_test,
type='Truth',
ID=-1
)
df_plot <- rbind(df_0, df_1, df_2) 

print(ggplot(df_plot) +
        geom_line(aes(x=ageori,y=value,group=interaction(ID, type),
                      color=type), 
                  alpha=0.3) +
    scale_x_continuous(limits=c(0,120),breaks=seq(0,120,by=10)) +
    scale_y_continuous(limits=c(-3,7),breaks=seq(-3,7,by=1)) +
    scale_color_manual(values=c('Fitted'='red','Observed'='black','Truth'='blue')) +
    labs(x='Age',y='Y'))

# print(mean(sigmaws[-(1:5000)]))
# print(mean(sigmays[-(1:5000)]))
# #jpeg(sprintf('curve_plus_intercept_%d.jpg',seed))
# df$cwi <- covar.list[[1]][,-(2:3)] %*% rowMeans(coefs[-(2:3),1,-(1:5000)])
# df$true_cwi <- f_sshape(df$ageori,mode1,range_L1,range_R1)*2 + true_fixed_effect[1,2]
# 
# df0 <- rbind(df %>% select(ageori, value=cwi) %>% mutate(type='Fitted'),
#              df %>% select(ageori, value=true_cwi) %>% mutate(type='Truth'))
# 
# cwi_quantile <- apply(covar.list[[1]][,-(2:3)] %*% (coefs[-(2:3),1,-(1:5000)]),
#       1,quantile,probs=c(.025,.975)) %>% t()
# df$cwi_lower <- cwi_quantile[,1]
# df$cwi_upper <- cwi_quantile[,2]
# print(ggplot(df0) + geom_line(aes(x=ageori,y=value,color=type,group=type),linetype='solid') +
#         scale_x_continuous(limits=c(50,110),breaks=seq(50,110,by=10)) +
#         scale_y_continuous(limits=c(0,5)) +
#         geom_ribbon(aes(x=ageori,ymin=cwi_lower,ymax=cwi_upper),data=df,fill='red',alpha=0.2) +
#         labs(x='Age',y='Value') +
#         scale_color_manual(values=c('Fitted'='red','Truth'='black')))
# all_age <- seq(0, 120, by=0.1)
# all_cwi <- splines2::ibs(all_age, knots=knot.list[[1]], intercept = TRUE, degree=2,
#                          Boundary.knots = c(0,120)) %*% rowMeans(coefs[-(1:3),1,-(1:5000)]) +
#   mean(coefs[1,1,-(1:5000)])
# all_true_cwi <- f_sshape(all_age,mode1,range_L1,range_R1)*2 + true_fixed_effect[1,2]
dev.off()