#Lab 10 Code Template
rm(list = ls())

rope = read.csv(here("data", "rope.csv"))
rope$rope.type = factor(rope$rope.type)


n_obs = nrow(rope)
n_groups = length(levels(rope$rope.type))


ss_tot = ss_tot_1 = sum(((mean(rope$p.cut))- rope$p.cut)^2)
df_tot = n_obs - 1

agg_sq_resids = 
  aggregate(
    x = rope$p.cut,
    by = list(rope$rope.type),
    FUN = function(x) (sum((x- mean(x))^2)))

ss_within = ss_within = sum(agg_sq_resids$x)
df_within = df_within = n_obs - n_groups

ss_among = ss_tot- ss_within
df_among = df_tot - df_within

ms_within = ss_within / (n_obs - n_groups)
ms_among  = ss_among / (n_groups - 1)

f_ratio = ms_among/ms_within
f_pval = pf(q= f_ratio , df1= df_among , df2= df_within, lower.tail= FALSE)
