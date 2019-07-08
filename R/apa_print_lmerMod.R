apa.print.lmerMod <- function(
  lmerMod_fit
  , conf.level = 0.95
  , ...
){
  
  tidy_lmerMod_fit <- broom.mixed::tidy(lmerMod_fit, conf.int=TRUE, conf.level=conf.level)
  
  tidy_fixed_eff <- subset(as.data.frame(tidy_lmerMod_fit), effect=="fixed")
  tidy_random_eff <- subset(as.data.frame(tidy_lmerMod_fit), effect=="ran_pars")
  
  tidy_fixed_eff <- tidy_fixed_eff %>% rename("Effect" = "term"
                                              , "$b$" = "estimate"
                                              , "SE" = "std.error"
                                              , "$t$" = "statistic")
  
  tidy_fixed_eff$Effect <- papaja:::prettify_terms(tidy_fixed_eff$Effect)
  
  tidy_fixed_eff <- printnum(tidy_fixed_eff)
  
  tidy_fixed_eff <- tidy_fixed_eff %>% mutate("$CI$"= paste0(conf.low, " - ", conf.high))
  
  tidy_fixed_eff <- tidy_fixed_eff %>% select(Effect, `$b$`, `SE`, `$t$`, `$CI$`)
  
}
  
  