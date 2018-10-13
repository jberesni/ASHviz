########################################################
# Investigation into ASH sampler timing consistency
########################################################
#
# Samples data frame: 
#    > one row per instance ASH sample
#    > ashDF assumed to have single DBID value
#    > DIFF_MSECS = milliseconds since last sample
Samples <- ashDF %>% 
           select(INSTANCE_NUMBER,SAMPLE_TIME,SAMPLE_ID) %>% 
           group_by(INSTANCE_NUMBER) %>% distinct(SAMPLE_TIME,SAMPLE_ID) %>%
           mutate(PREV_TIME=lag(SAMPLE_TIME,order_by=SAMPLE_ID)) %>%
           mutate(DIFF_MSECS=1000*
             as.numeric(difftime(SAMPLE_TIME, PREV_TIME, units = "secs")))

# density plot of computed sample interval in secs
qplot(Samples$DIFF_MSECS,geom="density")
ggsave("./Plots/msDiff01.png", device="png")

# plot as violin plot
qplot(1,Samples$DIFF_MSECS,geom="violin")
ggsave("./Plots/msDiff02.png", device="png")

# point plot over time of sample intervals against expected valUe (1000)
qplot(Samples$SAMPLE_ID,Samples$DIFF_MSECS-1000)
ggsave("./Plots/msDiff03.png", device="png")

# plot and color points by instance
qplot(SAMPLE_ID,DIFF_MSECS-1000, data=Samples
         ,color=INSTANCE_NUMBER
         ,show.legend=TRUE)
ggsave("./Plots/msDiff04.png", device="png")

# plot and color points by instance
qplot(SAMPLE_ID,DIFF_MSECS-1000 
      ,data=filter(Samples, DIFF_MSECS != 1000)
      ,color=INSTANCE_NUMBER
      ,show.legend=TRUE)
ggsave("./Plots/msDiff05.png", device="png")

# histogram of samples off 1000ms interval
qplot(DIFF_MSECS - 1000 
      ,data=filter(Samples, DIFF_MSECS != 1000)
      ,geom="histogram"
      ,fill=INSTANCE_NUMBER, position="stack"
      ,binwidth=10
)
ggsave("./Plots/msDiff06.png", device="png")

# zoom in on the bulk of distribution
qplot(DIFF_MSECS - 1000 
      ,data=filter(Samples, DIFF_MSECS != 1000)
      ,geom="histogram"
      ,fill=INSTANCE_NUMBER, position="stack"
      ,binwidth=10
) + xlim(-10,100)
ggsave("./Plots/msDiff07.png", device="png")

# change binwidth to narrower buckets...AHA!
qplot(DIFF_MSECS - 1000 
      ,data=filter(Samples, DIFF_MSECS != 1000)
      ,geom="histogram"
      ,fill=INSTANCE_NUMBER, position="stack"
      ,binwidth=1
) + xlim(-10,100)
ggsave("./Plots/msDiff08.png", device="png")

# add custom scales to see if centiseconds line up
# ?? needed to instantiate plot to add scale ??
p <-
qplot(DIFF_MSECS - 1000 
      ,data=filter(Samples, DIFF_MSECS != 1000)
      ,geom="histogram"
      ,fill=INSTANCE_NUMBER, position="stack"
      ,binwidth=1
) 
p  + scale_x_continuous(breaks = c(0,10,20,30,40) , limits=c(-10,100) ) 
ggsave("./Plots/msDiff09.png", device="png")

# now facet by instance to see if consistent
p <-
qplot(DIFF_MSECS - 1000 
      ,data=filter(Samples, DIFF_MSECS != 1000)
      ,geom="histogram"
      ,fill=INSTANCE_NUMBER, position="stack"
      ,binwidth=1
      ,facets = ~INSTANCE_NUMBER
) 
p  + scale_x_continuous(breaks = c(0,10,20,30,40) , limits=c(-10,100) ) 
ggsave("./Plots/msDiff10.png", device="png")

rm(p) # cleanup

###########################################################

