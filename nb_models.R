but_m0 <- stan_glmer(Butting ~ 1 + (1|Colony/Dish),
                     data = all_data,
                     family=neg_binomial_2,
                     diagnostic_file="but_m0.csv",
                     adapt_delta = 0.99)
but_m1 <- stan_glmer(Butting ~ Treatment + (1|Colony/Dish),
                     data = all_data,
                     family=neg_binomial_2,
                     diagnostic_file="but_m1.csv",
                     adapt_delta = 0.99)
but_m2 <- stan_glmer(Butting ~ Day + (1|Colony/Dish),
                     data = all_data,
                     family=neg_binomial_2,
                     diagnostic_file="but_m2.csv",
                     adapt_delta = 0.99)
but_m3 <- stan_glmer(Butting ~ Treatment + Day + (1|Colony/Dish),
                     data = all_data,
                     family=neg_binomial_2,
                     diagnostic_file="but_m3.csv",
                     adapt_delta = 0.99)
but_m4 <- stan_glmer(Butting ~ Treatment * Day + (1|Colony/Dish),
                 data = all_data,
                 family=neg_binomial_2,
                 diagnostic_file="but_m4.csv",
                 adapt_delta = 0.99)

sg_m0 <- stan_glmer(Self.grooming ~ 1 + (1|Colony/Dish),
                     data = all_data,
                     family=neg_binomial_2,
                     diagnostic_file="sg_m0.csv",
                     adapt_delta = 0.99)
sg_m1 <- stan_glmer(Self.grooming ~ Treatment + (1|Colony/Dish),
                     data = all_data,
                     family=neg_binomial_2,
                     diagnostic_file="sg_m1.csv",
                     adapt_delta = 0.99)
sg_m2 <- stan_glmer(Self.grooming ~ Day + (1|Colony/Dish),
                     data = all_data,
                     family=neg_binomial_2,
                     diagnostic_file="sg_m2.csv",
                     adapt_delta = 0.99)
sg_m3 <- stan_glmer(Self.grooming ~ Treatment + Day + (1|Colony/Dish),
                     data = all_data,
                     family=neg_binomial_2,
                     diagnostic_file="sg_m3.csv",
                     adapt_delta = 0.99)
sg_m4 <- stan_glmer(Self.grooming ~ Treatment * Day + (1|Colony/Dish),
                     data = all_data,
                     family=neg_binomial_2,
                     diagnostic_file="sg_m4.csv",
                     adapt_delta = 0.99)

but_comp <- bayesfactor_models(but_m1, but_m3, but_m3, but_m4, denominator = but_m0)
but_comp
but_inc <- bayesfactor_inclusion(but_comp)
but_inc

sg_comp <- bayesfactor_models(sg_m1, sg_m3, sg_m3, sg_m4, denominator = sg_m0)
sg_comp
sg_inc <- bayesfactor_inclusion(sg_comp)
sg_inc

