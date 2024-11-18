library(tidyverse)
pdf('flex_diagnosis.pdf',width=14,height=7)
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
dev.off()