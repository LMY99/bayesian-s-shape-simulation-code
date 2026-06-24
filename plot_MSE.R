library(tidyverse)
ci_list <- dir(pattern='_CIs\\d\\d\\.rdata')
whole_ci <- data.frame()
for(n in 1:80){
  load(ci_list[n])
  idx <- which(CI_repeat[,"age"] %in% seq(0,120,by=0.1))
  whole_ci <- rbind(whole_ci, CI_repeat[idx,] %>% as.data.frame() %>% mutate(filename=ci_list[n]))
}
whole_ci %>% mutate(
  true_curve = stringr::str_sub(filename, -7, -7) |> as.integer(),
  setting = stringr::str_sub(filename, -8, -8) |> as.integer(),
  method = str_sub(as.character(filename), 1, -13)
) %>% mutate(
  curve = c('Logit', 'Moderately Asymmetric', 'Highly Asymmetric',
            'Non-S-Shape')[true_curve],
  setting = c('Short Age Range\n N = 250*10',
              'Long Age Range\n N = 250*10',
              'Short Age Range\n N = 750*5',
              'Long Age Range\n N = 1000*15')[setting],
  method = method %>%
    case_match('flex'~'Flexible 0-120',
               'sflex'~'Flexible 30-90',
               'S'~'S-shape 0-120',
               'sS'~'S-shape 30-90',
               'para'~'Logistic Model')
) %>% 
  mutate(
    method = factor(method, levels=c('Flexible 0-120','Flexible 30-90','S-shape 0-120','S-shape 30-90','Logistic Model'))
  ) %>% 
  mutate(
    curve = factor(curve, levels=c('Logit', 'Moderately Asymmetric', 'Highly Asymmetric',
                                             'Non-S-Shape'))
  ) %>%
  mutate(
    setting = factor(setting, levels=c('Short Age Range\n N = 250*10',
                                       'Long Age Range\n N = 250*10',
                                       'Short Age Range\n N = 750*5',
                                       'Long Age Range\n N = 1000*15'))
  )-> whole_ci
whole_ci %>% 
  group_by(method, setting, curve, age) %>% 
  summarise(MSE=mean(MSE),bias2=mean(bias2),var=mean(var)) %>%
  pivot_longer(cols = c(MSE, bias2, var), ) -> whole_ci_grouped
gc()

whole_ci_grouped %>% filter(age %in% seq(55,85,by=5)) %>% filter(name=='MSE') -> 
  sub_table
sub_table %>% select(-name) %>% 
  pivot_wider(names_from=age, values_from=value, names_prefix = 'MSE') %>%
  relocate(curve, setting, method) %>% arrange(curve, setting, method) -> sub_table
sink('MSE_sub.txt')
print(
  sub_table %>% ungroup() %>% mutate(across(1:3, as.numeric)) %>% 
    kableExtra::kable(format='latex', digits = 4)
)
sink()

pdf('MSE.pdf', width=12, height=5)
for(m in sort(unique(whole_ci$curve))){
  #if(m == 'Logistic Model') next
  whole_ci_grouped %>% filter(curve == m) -> df0
  #jpeg(sprintf('Intervals %s.jpeg', m), width = 1152, height = 480)
  print(
    ggplot(data=df0, aes(x=age, y=value, group=name, col=name)) +
      geom_line() +
      facet_grid(rows=vars(setting),cols=vars(method), scales='free_y') +
      labs(title = paste('Squared Bias, Variance and MSE of Curve Estimates:', m)) +
      scale_x_continuous(limits=c(30,120),breaks=(1:4)*30) +
      scale_color_manual(values = c('MSE'='blue','bias2'='red','var'='green'))
  )
  gc()
}
dev.off()

pdf('MSE_Parametric_And_S_only.pdf', width=12, height=5)
for(m in sort(unique(whole_ci$curve))){
  #if(m == 'Logistic Model') next
  whole_ci_grouped %>% filter(curve == m) %>% filter(str_starts(method, 'Flexible', TRUE)) -> df0
  #jpeg(sprintf('Intervals %s.jpeg', m), width = 1152, height = 480)
  print(
    ggplot(data=df0, aes(x=age, y=value, group=name, col=name)) +
      geom_line() +
      facet_grid(rows=vars(setting),cols=vars(method), scales='free_y') +
      labs(title = paste('Squared Bias, Variance and MSE of Curve Estimates:', m)) +
      scale_x_continuous(limits=c(30,120),breaks=(1:4)*30) +
      scale_color_manual(values = c('MSE'='blue','bias2'='red','var'='green'))
  )
  gc()
}
dev.off()

pdf('MSE_Logit_sS_only.pdf', width=12, height=5)
for(m in sort(unique(whole_ci$curve))){
  #if(m == 'Logistic Model') next
  whole_ci_grouped %>% filter(curve == m) %>% 
    filter(method %in% c('Logistic Model', 'S-shape 30-90')) %>% 
    filter(name=='MSE') -> df0
  #jpeg(sprintf('Intervals %s.jpeg', m), width = 1152, height = 480)
  print(
    ggplot(data=df0, aes(x=age, y=value, group=method, col=method)) +
      geom_line() +
      geom_vline(xintercept = c(38.0366242218018, 41.7954032137857, 44.8363998560688, 47.5407223743789, 
                                50.0592851037502, 52.4630050145024, 54.7947227305235, 57.0832258700611, 
                                59.343127800968, 61.5847582906105, 63.8128327525643, 66.0320928173799, 
                                68.2423693091307, 70.444385633288, 72.6395465175233, 74.8311482025867, 
                                77.0259773386592, 79.2370141919141, 81.4891164675852, 83.8299252759576, 
                                86.3880509480576),
                 linetype = 'dotted', alpha = 0.3) +
      facet_grid(rows=vars(setting), scales='free_y') +
      labs(title = paste('MSE of Curve Estimates:', m), y = 'MSE') +
      scale_x_continuous(limits=c(30,120),breaks=(1:4)*30) +
      scale_color_manual(values = c('S-shape 30-90'='blue',
                                    'Logistic Model'='red'))
  )
  gc()
}
dev.off()
