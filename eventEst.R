##################################################
# investigation into event count estimaties
# as obtained from real ASH dumps
##################################################
# Events data frame is ashDF filtered for WAITs
Events <- ashDF %>% filter(STATE=="WAIT")
#
##########################################################
# use boxplots to show features of latency distributions
ggplot(data=Events, aes(x="ALL WAITS" ,y=log10(TIME_WAITED))) +
  geom_boxplot()
ggsave("./Plots/evtCount01.png", device="png")

# boxplots of log10(time_waited) by waitclass 
# add intercepts at 1 sec and 1 msec
ggplot(data=Events, aes(x=WAITCLASS ,y=log10(TIME_WAITED))) +
  geom_boxplot() + 
  geom_hline(yintercept=c(3,6),color="red")
ggsave("./Plots/evtCount02.png", device="png")

# by event_id
ggplot(data=Events, aes(x=EVENT_ID ,y=log10(TIME_WAITED))) +
  geom_boxplot() + 
  geom_hline(yintercept=c(3,6),color="red") + scale_x_discrete(labels=NULL)
ggsave("./Plots/evtCount03.png", device="png")

# color by waitclass, ugh
ggplot(data=Events %>% group_by(WAITCLASS), aes(x=EVENT_ID ,y=log10(TIME_WAITED))) +
  geom_boxplot(aes(color=WAITCLASS)) + 
  geom_hline(yintercept=c(3,6),color="red") + scale_x_discrete(labels=NULL)
ggsave("./Plots/evtCount04.png", device="png")

# facet by waitclass, better
ggplot(data=Events %>% group_by(WAITCLASS), aes(x=EVENT_ID ,y=log10(TIME_WAITED))) +
  geom_boxplot() + facet_wrap(~WAITCLASS) +
  geom_hline(yintercept=c(3,6),color="red") + scale_x_discrete(labels=NULL)
ggsave("./Plots/evtCount05.png", device="png")

# free scales on x-axis, nice!
ggplot(data=Events %>% group_by(WAITCLASS), aes(x=EVENT_ID ,y=log10(TIME_WAITED))) +
  geom_boxplot() + facet_wrap(~WAITCLASS, scales="free_x") +
  geom_hline(yintercept=c(3,6),color="red") + scale_x_discrete(labels=NULL)
ggsave("./Plots/evtCount06.png", device="png")

#
###################################################################
# density plots of log10(time_waited)
qplot(data=Events, x=log10(TIME_WAITED), geom=("density"))
ggsave("./Plots/evtCount07.png", device="png")

# density plot of log10(time_waited) weighted by est_count
qplot(data=Events, x=log10(TIME_WAITED), geom=("density")
      ,weight=EST_COUNT/sum(EST_COUNT))
ggsave("./Plots/evtCount08.png", device="png")

# put 2 densities together using ggplot
ggplot(data=Events, aes(x=log10(TIME_WAITED))) +
      geom_density(aes(color=I("blue"))) +
      geom_density(aes(color=I("red"),  weight=EST_COUNT/sum(EST_COUNT)))
ggsave("./Plots/evtCount09.png", device="png")

######################################################################
# use violin plots to show densities
ggplot(data=Events, aes(x="ALL", y=log10(TIME_WAITED))) +
  geom_violin()
ggsave("./Plots/evtCount10.png", device="png")

# compare instances
ggplot(data=Events, aes(x=INSTANCE_NUMBER, y=log10(TIME_WAITED))) +
  geom_violin()
ggsave("./Plots/evtCount11.png", device="png")

# violin plot weighted by EST_COUNT
ggplot(data=Events, aes(x="EST COUNT", y=log10(TIME_WAITED))) +
  geom_violin(aes(weight=EST_COUNT/sum(EST_COUNT)))
ggsave("./Plots/evtCount12.png", device="png")

# look better side-by-side
ggplot(data=Events, aes(x="COUNT", y=log10(TIME_WAITED))) +
  geom_violin() +
  geom_violin(aes(x="EST_COUNT", weight=EST_COUNT/sum(EST_COUNT)))
ggsave("./Plots/evtCount13.png", device="png")

# plot densities on top of one another with color fill and alpha=.5
ggplot(data=Events, aes(x="1", y=log10(TIME_WAITED))) +
  geom_violin(aes(fill=I("blue")), alpha=.5) +
  geom_violin(aes(x="1", weight=EST_COUNT/sum(EST_COUNT), 
                  fill=I("red")),alpha=.5) +
  scale_x_discrete(labels=NULL)
ggsave("./Plots/evtCount14.png", device="png")

# show both densities faceted by waitclass
ggplot(data=Events, aes(x=WAITCLASS, y=log10(TIME_WAITED))) +
  geom_violin(scale="count", aes(fill=I("blue")),alpha=.5) +
  geom_violin(scale="count", aes(weight=EST_COUNT),fill=I("red"),alpha=.5) +
  facet_wrap("WAITCLASS", scales="free_x") 
ggsave("./Plots/evtCount15.png", device="png")

#

