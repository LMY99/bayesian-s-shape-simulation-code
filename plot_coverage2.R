library(tidyverse)
load('coveragerate.rda')
ci_coverage %>% lapply(function(x) data.frame(age=as.double(names(x)),prob=x)) -> ci_list
for(j in names(ci_list)){
  ci_list[[j]] <- ci_list[[j]] %>% mutate(
    setting = str_sub(as.character(j), -8, -8),
    true_curve = str_sub(as.character(j), -7, -7),
    method = str_sub(as.character(j), 1, -13))
}
ci_list %>% reduce(rbind) -> ci_list
ci_list <- ci_list %>% mutate(setting = c('Short Age Range\n N = 250*10',
                                          'Long Age Range\n N = 250*10',
                                          'Short Age Range\n N = 750*5',
                                          'Long Age Range\n N = 1000*15')[as.integer(setting)],
                              true_curve = c('Logit', 'Moderately Asymmetric', 'Highly Asymmetric',
                                             'Non-S-Shape')[as.integer(true_curve)],
                              method = case_match(method, 
                                                  'flex'~'Flexible 0-120',
                                                  'sflex'~'Flexible 30-90',
                                                  'S'~'S-shape 0-120',
                                                  'sS'~'S-shape 30-90',
                                                  'para'~'Logistic Model')) %>% 
  mutate(
    method = factor(method, levels=c('Flexible 0-120','Flexible 30-90','S-shape 0-120','S-shape 30-90','Logistic Model'))
  ) %>% 
  mutate(
    true_curve = factor(true_curve, levels=c('Logit', 'Moderately Asymmetric', 'Highly Asymmetric',
                                   'Non-S-Shape'))
  ) %>%
  mutate(
    setting = factor(setting, levels=c('Short Age Range\n N = 250*10',
                                       'Long Age Range\n N = 250*10',
                                       'Short Age Range\n N = 750*5',
                                       'Long Age Range\n N = 1000*15'))
  )
ci_list %>% filter(age %in% seq(55,85,by=5)) -> sub_ci
sub_ci %>% relocate(true_curve, setting, method) %>%
  rename(curve = true_curve) -> sub_ci 
sub_ci %>% arrange(curve, setting, method) %>%
  pivot_wider(names_from=age, values_from=prob, names_prefix = 'Prob') -> sub_ci
saveRDS(sub_ci, file='Coverage_sub.rds')
pdf('coverage_function.pdf', width=12, height=7)
print(ci_list %>%# filter(method!='Logistic Model') %>%
        ggplot() + geom_line(aes(x=age,y=prob,group=method,col=method)) +
        facet_grid(rows=vars(setting),cols=vars(true_curve)) +
        scale_color_manual(values=
                             c('Flexible 0-120'='red',
                               'Flexible 30-90'='orange',
                               'S-shape 0-120'='blue',
                               'S-shape 30-90'='lightblue',
                               'Logistic Model'='green')
        ) +
        scale_y_continuous(limits=c(-0.05,1.05),breaks=(0:4)/4) +
        scale_x_continuous(limits=c(30,110),breaks=seq(from=30,to=110,by=20))
)
dev.off()