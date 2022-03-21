library(scico)
compare_thresholds <- function(data, lb = 1.5, ub = 6.5) {
  
  # we have two parameters:
  # lb = lower bound
  # ub = upper bound
  
  # primary data wrangling
  p <-
    bind_rows(
      data %>% 
        gather(name, threshold, -iter) %>% 
        group_by(iter) %>% 
        mutate(theta_bar = mean(threshold)),
      data %>% 
        gather(name, threshold, -iter) %>% 
        group_by(iter) %>% 
        mutate(threshold = scales::rescale(threshold, to = c(lb, ub))) %>% 
        mutate(theta_bar = mean(threshold))
    ) %>% 
    mutate(model = rep(c("brms parameterization", "Kruschke's parameterization"), each = n() / 2)) 
  
  # compute the means by model and threshold for the vertical lines
  means <-
    p %>% 
    ungroup() %>% 
    group_by(model, name) %>% 
    summarise(mean = mean(threshold))
  
  # plot!
  p %>% 
    ggplot(aes(x = threshold, y = theta_bar)) +
    geom_vline(data = means,
               aes(xintercept = mean, color = name),
               linetype = 2) +
    geom_point(aes(color = name),
               alpha = 1/10, size = 1/2) +
    scale_color_scico_d(palette = "lajolla", begin = .25) +
    ylab("mean threshold") +
    theme(legend.position = "none") +
    facet_wrap(~model, ncol = 1, scales = "free")
  
}
  
post <- 
  posterior_samples(M1) %>%
  mutate(iter = 1:n())

# visualize the parametrization
post %>% 
  select(`b_Intercept[1]`:`b_Intercept[4]`, iter) %>% 
  compare_thresholds(lb = 1.5, ub = 4.5)

sl <- scico(palette = "lajolla", n = 9)

scales::show_col(sl)

library(tidybayes)
# the effects size for religion in the model
post %>% 
  ggplot(aes(x = b_familyBariReligiousAfter, y = 0)) +
  stat_halfeye(point_interval = mode_hdi, .width = .95,
               fill = sl[4], color = sl[8]) +
  scale_y_continuous(NULL, breaks = NULL)

### maybe yes (above)

# plot posteriors for all variables in model
conditional_effects(M1) %>% 
  plot(line_args = list(color = sl[7], fill = sl[3]))


# get draws
conditional_effects(M1,
                    spaghetti = TRUE, 
                    nsamples = 100) %>% 
  plot(points = T,
       line_args = list(size = 0),
       point_args = list(alpha = 1/3, color = sl[9]),
       spaghetti_args = list(colour = alpha(sl[6], .2)))

# make location categorical as it should be and choose which variable to condition on
 m <- conditional_effects(M1, categorical = T, effects = "familyBariReligiousAfter", probs=c(0.25,0.75))

str(m)


# how many fitted lines do you want?
n_iter <- 50

# define the `X_s` values you want to condition on
# because the lines are nonlinear, you'll need many of them
#religious_seq <- tibble(religiosity = seq(from = -1, to = 1, by = 1))
attach(non_rels)
nd <- tibble(
  kids_in_hh = mean(kids_in_hh),
  age_wife = mean(age_wife),
  religion=mean(religion),
  familyBariReligiousAfter = c(-1,0,1),
  religious_knowledge_scale=mean(religious_knowledge_scale),
  MI_geo_proximity=mean(MI_geo_proximity),
  MI_economic_capital=mean(MI_economic_capital),
  MI_human_capital=mean(MI_human_capital)
)
detach(non_rels)


f <-
  fitted(M1,
         newdata = nd,
         summary = F,
         nsamples = n_iter)

# inspect the output
f %>% 
  str()


# rearrange the output
q <- rbind(
  f[, , 1],
  f[, , 2],
  f[, , 3],
  f[, , 4],
  f[, , 5]
) %>% 
  # wrangle
  data.frame() %>% 
  set_names(nd %>% pull(familyBariReligiousAfter)) %>% 
  mutate(iter   = rep(1:n_iter, times = 5),
         location = rep(1:5, each = n_iter)) %>% 
  pivot_longer(-c(iter, location),
               names_to = "familyBariReligiousAfter",
               values_to = "probability") %>% 
  mutate(location = str_c("Y: ", location),
         familyBariReligiousAfter    = familyBariReligiousAfter %>% as.integer()) 


  
  # plot
  plot <- q %>% ggplot(aes(x = familyBariReligiousAfter, y = probability, 
             group = interaction(iter, location),
             color = location)) +
  geom_line(size = 1/4, alpha = 1/2)  +
  scale_color_scico_d(palette = "lajolla", begin = .25) +
  scale_y_continuous(breaks = c(0, .5, 1), limits = c(0, 1), expand = c(0, 0)) +
  theme(legend.position = "none") +
  facet_wrap(~location, nrow = 5)
  
# replot with summary statistics
  
# use summary statistics

attach(non_rels)
  nd <- tidyr::crossing (
    kids_in_hh = mean(kids_in_hh),
    age_wife = mean(age_wife),
    religion=mean(religion),
    location=c(1,2,3,4,5),
    familyBariReligiousAfter = c(-1,0,1),
    religious_knowledge_scale=mean(religious_knowledge_scale),
    MI_geo_proximity=mean(MI_geo_proximity),
    MI_economic_capital=mean(MI_economic_capital),
    MI_human_capital=mean(MI_human_capital)
  )
  detach(non_rels)
  
  
  p1 <-
    fitted(M1, 
           scale = "linear",
           newdata = nd) %>% 
    data.frame() %>% 
    bind_cols(nd) 
    
plot2 <- p1 %>%  ggplot(aes(x = familyBariReligiousAfter, y = Estimate, ymin = Q2.5, ymax = Q97.5)) +
    geom_smooth(stat = "identity",
                alpha = 1/2, color = sl[7], fill = sl[3]) +
    labs(title = "summary statistics",
         y = "underlying standard normal")

# try to plot facet plot using summary instead of draws from posterior


str(mu_summary2)



# plot
plot2 <- mu_summary2 %>% ggplot(aes(x = factor(religiosity), y = estimate, ymin='5CI',ymax='95CI')) +
  
  geom_errorbar()+
                         #geom_smooth(stat="identity", alpha=1/2,color = sl[7], fill = sl[3]) +
                         
  # geom_line(size = 1/4, alpha = 1/2)  +
  # scale_color_scico_d(palette = "lajolla", begin = .25) +
  # scale_y_continuous(breaks = c(0, .5, 1), limits = c(0, 1), expand = c(0, 0)) +
 # theme(legend.position = "none") +
  facet_wrap(~location, ncol = 5)

plot2

plot2 <- mu_summary2 %>% ggplot(aes(x = factor(religiosity), y = estimate)) +
  geom_errorbar(aes(x=religiosity,ymin='5CI',ymax='95CI'))

mu_summary2$lower <- mu_summary2$`5CI`
mu_summary2$upper <- mu_summary2$`95CI`

mu_summary2$religiosity<-as.factor(mu_summary2$religiosity)
non_rels$religiosity<-as.factor(non_rels$religiosity)

# tack on predicted values to non_rels
b <- non_rels %>% left_join(mu_summary2, by=c("religiosity"="religiosity", "location"= "location"))

plot2 <- ggplot(mu_summary2, aes(x=religiosity, y=estimate)) + 
  geom_errorbar(aes(ymin=lower, ymax=upper), 
                colour="black", width=.1, position=pd) +
  geom_line( aes(x=as.numeric(religiosity), y=estimate)) + 
  geom_jitter(data=b,aes(y=estimate , x= religiosity),  size=1, color='red')+
  facet_wrap(~location, nrow = 5)


geom_line(data=Data2, aes(x=C, y=D), color='red')
## convenience function


  # compute the means by model and threshold for the vertical lines
  means <-
    p %>% 
    ungroup() %>% 
    group_by(model, name) %>% 
    summarise(mean = mean(threshold))
  
  # plot!
  p %>% 
    ggplot(aes(x = threshold, y = theta_bar)) +
    geom_vline(data = means,
               aes(xintercept = mean, color = name),
               linetype = 2) +
    geom_point(aes(color = name),
               alpha = 1/10, size = 1/2) +
    scale_color_scico_d(palette = "lajolla", begin = .25) +
    ylab("mean threshold") +
    theme(legend.position = "none") +
    facet_wrap(~model, ncol = 1, scales = "free")
  
}