###########################################################
# AAS over time  by Instance/Wait Class etc (Top Activity)
###########################################################
#
# build CPU, WAIT summary dataset of all instances over all samples
d <- ashDF %>% group_by(INSTANCE_NUMBER,SAMPLE_TIME,SAMPLE_ID,WAITCLASS) %>% summarize(AS=n())
#
# use SAMPLE_TIME for x-axis, peaks line up
ggplot(data=d,aes(x=SAMPLE_TIME,y=AS,color=WAITCLASS)) + geom_point(alpha=.5)
ggsave("./Plots/stdViz01.png", device="png")

ggplot(data=d,aes(x=SAMPLE_TIME,y=AS,color=INSTANCE_NUMBER)) + geom_point(alpha=.5)
ggsave("./Plots/stdViz02.png", device="png")

# facet by INSTANCE to compare                                                
ggplot(data=d,aes(x=SAMPLE_TIME,y=AS,color=WAITCLASS)) + geom_point() + facet_wrap(~INSTANCE_NUMBER)
ggsave("./Plots/stdViz03.png", device="png")
#
# use SAMPLE_ID for x-axis, note difference
ggplot(data=d,aes(x=SAMPLE_ID,y=AS,color=WAITCLASS)) +  geom_point(alpha=.5)
ggsave("./Plots/stdViz04.png", device="png")

# color by INSTANCE
ggplot(data=d,aes(x=SAMPLE_ID,y=AS,color=INSTANCE_NUMBER)) +  geom_point(alpha=.5)
ggsave("./Plots/stdViz05.png", device="png")

# facet by INSTANCE and see shifts in peak
ggplot(data=d,aes(x=SAMPLE_ID,y=AS,color=WAITCLASS)) +  geom_point(alpha=.5) + facet_wrap(~INSTANCE_NUMBER)
ggsave("./Plots/stdViz06.png", device="png")
#
# investigate relationship of SAMPLE_TIME and SAMPLE_ID
d2 <- d %>% distinct(INSTANCE_NUMBER,SAMPLE_ID,SAMPLE_TIME)
#
ggplot(data=d2, aes(x=SAMPLE_ID, y=SAMPLE_TIME, color=INSTANCE_NUMBER)) + geom_line(size=2)
ggsave("./Plots/stdViz07.png", device="png")
#
#
# create one-minute summaries of AAS by WAITCLASS and INSTANCE
d3 <- ashDF %>% 
      group_by(INSTANCE_NUMBER,SAMPLE_TIME,WAITCLASS) %>% 
      summarize(AS=n()) %>%
      mutate(MIN=as.character(trunc(SAMPLE_TIME,units=c("mins")))) %>%
      group_by(INSTANCE_NUMBER,MIN,WAITCLASS) %>%
      summarize(AAS = sum(AS)/60)
#
# plot global AAS by WAITCLASS per minute
ggplot(data=d3,aes(x=MIN,y=AAS,color=WAITCLASS)) + 
     geom_col(aes(fill=WAITCLASS)) +
     scale_x_discrete(labels = NULL) + ylab("Avg Active Sessions")
ggsave("./Plots/stdViz08.png", device="png")


# cleanup
rm(d)
rm(d2)
rm(d3)

