rm(list=ls())

library(statnet)
library(tidyverse)
library(ergm.terms.contrib)
library(ergm.userterms)
library(pbapply)
library(tidyr)
library(broom)
library(btergm)
library(texreg)
load('scratch/ergm_mcmc_psock_results_short.RData')

coef_list = lapply(list(m1,m2,m3),function(x){
temp_df = data.frame(estimate = coef(x),data.frame(confint(x)))
temp_df$term = rownames(temp_df)
colnames(temp_df) <- c('estimate','lower','upper','term')
temp_df$model = as.character(x$formula[[2]])[[3]]
temp_df[temp_df$estimate>-Inf,]})

coef_df = Reduce(full_join,coef_list)
coef_df$term = gsub("trans_.*_mat",'trans_mat',coef_df$term)
coef_df$term = gsub("mat_.*_nearest",'mat_nearest',coef_df$term)

coef_df$term <- fct_recode(coef_df$term, `GWD District (a=0.75)`="gwb1deg0.75",`Edges` = "edges",`District degree = 0` = "b1deg0",
                           `GWD Personnel (a=0.75)`="gwb2deg0.75",`GWNSP District (a=0.25)` =  "gwb1nsp.fixed.0.25"  ,`GWNSP Personnel (a=0.25)` =  "gwb2nsp.fixed.0.25"  ,
                           `Provides surface water`="b1factor.Water_Type.SW", `Self-sourced water` = "b1factor.Make_Buy.Self-Source",`ln(acreage)` = "nodeocov.Log_Acreage",`District age` = "nodeocov.AGE",
                           `ln(service pop.)` = 'nodeocov.Log_Service_Pop',`ln(debt/connections)` = "nodeocov.Log_Bonds_Outstanding_Per_Connection",
                           `ln(revenue/connections)` = "nodeocov.Log_Revenue_Per_Connection",
                           `Same GCD` = "b1nodematch.Same_GCD_GW_User",`Exchange partners` = "edgecov.trans_mat",
                           `ln(nearest other employer)` = "edgecov.mat_nearest_other")


coef_df$sig = ifelse(coef_df$lower<0 & coef_df$upper>0,0,1)
coef_df$term  <- fct_inorder(coef_df$term)
coef_df$term <- fct_relevel(coef_df$term,"ln(nearest other employer)",after = 6)
coef_df$term <- fct_rev(coef_df$term)
#coef_df$term <-  fct_relevel(coef_df$term,"Personnel with degree=1","GWD District (a=0.75)","GWD Personnel (a=0.75)","GWD District (a=0.75)","GWNSP Personnel (a=0.25)",
#                             "ln(acreage)","District age","ln(service pop.)","SDWA violation points (5 yrs)",
#                             "ln(outstanding debt/service connections)" ,"ln(revenue/service connections)",
#                             "Provides surface water","Self-sourced water","Nearest other employer" ,"Works for shared groundwater user" ,"Works for exchange partner"  )
library(ggthemes)
library(scales)
#show_col(colorblind_pal()(8))
cs = c('black','#E69F00','#56B4E9')
plot_df = coef_df %>% filter(term!='Edges') %>% 
  mutate(term = fct_rev(term))


gg2 = ggplot( data = plot_df ) + 
  geom_hline(yintercept=0,lty=2,col='grey50') + 
  geom_segment(aes(y=lower,yend=upper,x=term,xend=term,
        fill = paste(model,sig),colour = paste(model,sig)),
    lwd=1.5,width=0.2,position = position_dodge(width=0.75),lineend = 'round') + 
  geom_point(aes(y=estimate,x=term,
                     fill = paste(model,sig),colour = paste(model,sig),
                     shape = as.factor(sig)), size=3,position = position_dodge(width=0.75)) +
  coord_flip() + theme_bw() + 
  theme(axis.title.y = element_blank(),legend.position = c(0.62,0.6),
        axis.text = element_text(size=11),axis.title.x = element_text(size=14),
        legend.background = element_rect(fill = alpha('white',0.5)),
        legend.text = element_text(size=10),legend.title=element_blank()) +
  scale_y_continuous(name = '95% confidence interval') +
  scale_color_manual(name = '',values = rep(cs,each=2),labels = c('Financial (p>0.05)','Financial (p<0.05)','Managerial (p>0.05)','Managerial (p<0.05)',
                                                                  'Technical (p>0.05)','Technical (p<0.05)')) + 
  scale_fill_manual(name = '',values = c('white',cs[1],'white',cs[2],'white',cs[3]),
                    labels = c('Financial (p>0.05)','Financial (p<0.05)','Managerial (p>0.05)','Managerial (p<0.05)',
                               'Technical (p>0.05)','Technical (p<0.05)')) + 
  facet_wrap(~model,ncol = 3) + 
  scale_shape_manual(name = '95% CI includes 0',values=c(21,19),labels=c('includes 0','')) + 
  ggtitle('95% confidence intervals by personnel type') +
  guides(fill = FALSE,color=FALSE,shape = 
           guide_legend(override.aes = list(shape = c(21,NA),labels=c('Yes',''))))

ggsave('figure2.tiff',gg2,width = 6,height=3.5,units = 'in',dpi = 600)


library(texreg)

pdat = coef_df %>% filter(term!='Edges') %>% mutate(term = fct_rev(term))
pdat$nsig = (pdat$sig==0) + 0



  pdat %>% s
       aes(y=estimate,ymin=conf.low,ymax=conf.high,x=term,
                                             fill = paste(model,sig),colour = model)) + 
  geom_errorbar(lwd=1.5,width=0.2,position = position_dodge(width=0.75)) + 
  geom_point(aes(shape = as.factor(sig)), size=3,position = position_dodge(width=0.75)) +
  coord_flip() + theme_bw() + 
  geom_hline(yintercept=0,lty=2,col='grey50') + 
  theme(axis.title.y = element_blank(),legend.position = c(0.90,0.7),axis.ticks=element_blank(),
        axis.text = element_text(size=12),axis.title.x = element_text(size=14),
        legend.text = element_text(size=12),legend.title=element_text(size = 14)) +
  scale_y_continuous(name = '95% confidence interval') +
  #scale_fill_manual(name = '',#values = c('white',cs[1],'white',cs[2],'white',cs[3]),
                    #labels = c('Financial (p>0.05)','Financial (p<0.05)','Managerial (p>0.05)','Managerial (p<0.05)',
                      #         'Technical (p>0.05)','Technical (p<0.05)')) + 
  #)
  scale_fill_colorblind() +
  scale_color_colorblind(name = 'Graph',labels=c('financial',
                                                 'managerial','technical')) +
  #facet_wrap(~model,ncol = 2) + 
  geom_vline(aes(xintercept=5.5))+
  #guides(fill = FALSE) + 
  scale_shape_manual(name = '0 w/in 95% CI',values=c(21,19),labels=c('Yes','No')) + 
  ggtitle('95% confidence intervals for ERGMs by personnel type')




htmlreg(list(m1B,m2B,m3B),ci.force = T,single.row = T,omit.coef = '(mindeg)|(^b2deg0)',
        custom.model.names = c('Financial','Managerial','Technical'),digits=3,
        file = 'scratch/table3.html')
screenreg(list(m1B,m2B,m3B),ci.force = T,single.row = T,omit.coef = '(mindeg)|(^b2deg0)',
          custom.model.names = c('Financial','Managerial','Technical'),digits = 3)


rev_diff = outer((net_list$FINANCIAL %v% 'Log_Revenue_Per_Connection')[1:net_list$FINANCIAL$gal$bipartite],
             (net_list$FINANCIAL %v% 'Log_Revenue_Per_Connection')[1:net_list$FINANCIAL$gal$bipartite],FUN = "-")
rev_diff = abs(rev_diff)

rev_diff = rev_diff %>% as.data.frame() %>% mutate(V1 = rownames(.)) %>% gather(V2,Value,-V1) %>% 
  mutate(V2 = as.numeric(gsub('V','',V2)), V1 = as.numeric(V1)) %>% rename(i = V1,k = V2)

summary(exp(net_list$FINANCIAL %v% 'Log_Revenue'))
summary(exp((net_list$FINANCIAL %v% 'Log_Revenue')[1:532]))
summary(exp((net_list$FINANCIAL %v% 'Log_Revenue_Per_Connection')[1:532]))

sort((net_list$FINANCIAL %v% 'Log_Revenue_Per_Connection')[1:532])
10.742822 - (-11.512925)




library(pbapply)


full_combos = expand.grid(1:net_list$FINANCIAL$gal$bipartite,1:net_list$FINANCIAL$gal$bipartite,net_list$FINANCIAL$gal$bipartite + 1:((network.size(net_list$FINANCIAL) - net_list$FINANCIAL$gal$bipartite)) ) %>% as.data.frame() %>% rename(i = Var1,k = Var2,j = Var3) %>%
  filter(i<k) 
district_combos = expand.grid(1:net_list$FINANCIAL$gal$bipartite,1:net_list$FINANCIAL$gal$bipartite) %>% as.data.frame() %>% rename(i = Var1,k = Var2) %>%
  filter(i<k) 

rev_probs_fin <- ep1 %>% gather(Param,Change,-tie,-i,-j,-t,-probability) %>%
  filter(Param == "b1absdiff.Log_Revenue_Per_Connection")

rev_fin_samp =sample(x =  1:nrow(full_combos),replace = F,size = 1000000)
joint_rev_probs_fin = pbsapply(rev_fin_samp,function(i) rev_probs_fin$probability[rev_probs_fin$i == combos$i[i] & rev_probs_fin$j == combos$j[i]] *
                                 rev_probs_fin$probability[rev_probs_fin$i == combos$k[i] & rev_probs_fin$j == combos$j[i]],cl = 8)


test = data.frame(joint_rev_probs_fin = joint_rev_probs_fin,full_combos[rev_fin_samp,])
test$i_log_rev = (net_list$FINANCIAL %v% 'Log_Revenue_Per_Connection')[test$i]
test$k_log_rev = (net_list$FINANCIAL %v% 'Log_Revenue_Per_Connection')[test$k]
test = test %>% filter(i_log_rev>0,k_log_rev>0)


ggplot(test,
       aes(x = log(abs(exp(i_log_rev) - exp(k_log_rev))),y = joint_rev_probs_fin)) + 
  geom_point(pch=21,alpha=0.4) + geom_smooth()

exp(1.988)
exp(1.824)
exp(2.665)

1.988 [1.842; 2.135]*
  1.824 [1.605; 2.043]*
  2.665 [2.467; 2.863]*
  
0.014 [0.002; 0.025]*
  0.068 [0.035; 0.101]*
  0.066 [0.023; 0.109]*
  
     

,fill = joint_rev_probs_fin,
                col = joint_rev_probs_fin)) 


hist(test$joint_rev_probs_fin[test$i_log_rev>0 & test$k_log_rev>0])


table(test$joint_rev_probs_fin[test$i_log_rev>0 & test$k_log_rev>0]>0.01)
summary(exp(net_list$FINANCIAL %v% 'Log_Revenue'))
summary(round(exp(rev_diff$Value),2))



combos = expand.grid(1:net_list$MANAGERIAL$gal$bipartite,1:net_list$MANAGERIAL$gal$bipartite,net_list$MANAGERIAL$gal$bipartite + (1:network.size(net_list$MANAGERIAL) - net_list$MANAGERIAL$gal$bipartite)) %>% as.data.frame() %>% rename(i = Var1,k = Var2,j = Var3) %>%
  filter(i<k) 
rev_probs_man <- ep2 %>% gather(Param,Change,-tie,-i,-j,-t,-probability) %>%
  filter(Param == "b1absdiff.Log_Revenue_Per_Connection")
joint_rev_probs_man = pbsapply(1:nrow(combos),function(i) rev_probs_man$probability[rev_probs_man$i == combos$i[i] & rev_probs_man$j == combos$j[i]] *
                                 rev_probs_man$probability[rev_probs_man$i == combos$k[i] & rev_probs_man$j == combos$j[i]],cl = 8)


combos = expand.grid(1:net_list$TECHNICAL$gal$bipartite,1:net_list$TECHNICAL$gal$bipartite,net_list$TECHNICAL$gal$bipartite + (1:network.size(net_list$TECHNICAL) - net_list$TECHNICAL$gal$bipartite)) %>% as.data.frame() %>% rename(i = Var1,k = Var2,j = Var3) %>%
  filter(i<k) 
rev_probs_tech <- ep2 %>% gather(Param,Change,-tie,-i,-j,-t,-probability) %>%
  filter(Param == "b1absdiff.Log_Revenue_Per_Connection")
joint_rev_probs_tech = pbsapply(1:nrow(combos),function(i) rev_probs_tech$probability[rev_probs_tech$i == combos$i[i] & rev_probs_tech$j == combos$j[i]] *
                                  rev_probs_tech$probability[rev_probs_tech$i == combos$k[i] & rev_probs_tech$j == combos$j[i]],cl = 8)



ggplot(data=probs %>% filter(Param == "b1absdiff.Log_Revenue_Per_Connection"),aes(x=Change,y=probability))+
  geom_point()

ggplot(data=probs %>% filter(Param ==  "b1absdiff.Viol_Points_5yr"),aes(x=Change,y=probability))+
  geom_point()



