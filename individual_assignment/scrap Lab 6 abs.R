n = 200
mean_differences = c()
for (i in 1:n)
{
  mean_differences = c(
    mean_differences,
    two_group_resample(dat_pen$flipper_length_mm, 68, 152)
  )
}
hist(abs(mean_differences))

sum(abs(mean_differences) >= diff_observed)
dev.off()
