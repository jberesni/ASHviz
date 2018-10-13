###### vignette 4: back to reality #######
# plot events log10(time_waited) by sample_time

# geom_point defaults have lots of overplotting
ggplot(data=Events, aes(x=SAMPLE_TIME ,y=log10(TIME_WAITED))) +
  geom_point(aes(color=WAITCLASS))
ggsave("./Plots/LatencyHist01.png", device="png")

# reduce size and alpha of points to reduce overplotting effects
ggplot(data=Events, aes(x=SAMPLE_TIME ,y=log10(TIME_WAITED))) +
       geom_point(aes(color=WAITCLASS),size=.01,alpha=.1)
ggsave("./Plots/LatencyHist02.png", device="png")

# vector of waitclass colors, I/O=blue, cluster=red
WCcol <- c(Application="yellow", Cluster="red", Commit="yellow", Concurrency="green"
           ,Configuration="black", Idle="pink1", Network="yellow", Other="pink2"
           ,"System I/O" = "blue2", Unknown="pink3", "User I/O"="blue")

# bigger size, smaller alpha, custom colors
ggplot(data=Events, aes(x=SAMPLE_TIME ,y=log10(TIME_WAITED))) +
  geom_point(aes(color=WAITCLASS),size=.1,alpha=.05 ) +
  scale_color_manual(values = WCcol) 

# change to theme dark and new feature is shown
last_plot() + theme_dark()

# facet by waitclass
last_plot() + facet_wrap(~WAITCLASS)

# remove color scale, faceting is by waitclass
last_plot() + scale_color_manual(values=WCcol,guide=FALSE) 

# full code for same final graph as above  
ggplot(data=Events, aes(x=SAMPLE_TIME ,y=log10(TIME_WAITED))) +
  geom_point(aes(color=WAITCLASS),size=.1,alpha=.05 ) +
  scale_color_manual(values = WCcol, guide=FALSE) +
  theme_dark() + facet_wrap(~WAITCLASS)
ggsave("./Plots/LatencyHist03.png", device="png")

# change to theme_bw and raise alpha to see smaller collections better
ggplot(data=Events, aes(x=SAMPLE_TIME ,y=log10(TIME_WAITED))) +
  geom_point(aes(color=WAITCLASS),size=.1,alpha=.5 ) +
  scale_color_manual(values = WCcol, guide=FALSE) +
  theme_bw() + facet_wrap(~WAITCLASS)

#
# change some colors and redraw
WCcol["Application"] <- "green"
WCcol["Commit"] <- "green"
WCcol["Network"] <- "black"
WCcol["Idle"] <- "black"

# draw last plot with new colors
last_plot() + scale_color_manual(values = WCcol, guide=FALSE)
ggsave("./Plots/LatencyHist04.png", device="png")

# set size and alpha to .1 to start
ggplot(data=Events, aes(x=SAMPLE_TIME ,y=log10(TIME_WAITED))) +
  geom_point(aes(color=WAITCLASS),size=.1,alpha=.1 ) +
  scale_color_manual(values = WCcol, guide=FALSE) +
  theme_bw() + facet_wrap(~WAITCLASS)

# shrink alpha and point size down to see into thr CLuster and User I/O
# events, all others are obliterated
ggplot(data=Events, aes(x=SAMPLE_TIME ,y=log10(TIME_WAITED))) +
  geom_point(aes(color=WAITCLASS),size=.01,alpha=.01 ) +
  scale_color_manual(values = WCcol, guide=FALSE) +
  theme_bw() + facet_wrap(~WAITCLASS)
ggsave("./Plots/LatencyHist05.png", device="png")

# quick plot of log10(latency) density plots faceted by waitclass
qplot(x=log10(TIME_WAITED), data=Events, color=WAITCLASS, geom="density") +
  facet_wrap(~WAITCLASS)

# quick plot of log10(latency) histograms
qplot(x=log10(TIME_WAITED), data=Events, color=WAITCLASS, fill=WAITCLASS, 
      geom="histogram", binwidth=1) +
  facet_wrap(~WAITCLASS)

# use qqplot, shrink binwidth, use WCcol for colors 
ggplot(data=Events, aes(x=log10(TIME_WAITED), color=WAITCLASS, fill=WAITCLASS)) +
      geom_histogram(binwidth=.1) +
      scale_color_manual(values=WCcol, guide=FALSE) +
      scale_fill_manual(values=WCcol, guide=FALSE) +
      facet_wrap(~WAITCLASS)
ggsave("./Plots/LatencyHist06.png", device="png")


# now weight histograms by est_count
ggplot(data=Events, aes(x=log10(TIME_WAITED), color=WAITCLASS, fill=WAITCLASS)) +
  geom_histogram(binwidth=.1, aes(weight=EST_COUNT)) +
  scale_color_manual(values=WCcol, guide=FALSE) +
  scale_fill_manual(values=WCcol, guide=FALSE) +
  facet_wrap(~WAITCLASS)

# scale y-axis (event count) by log10
ggplot(data=Events, aes(x=log10(TIME_WAITED), color=WAITCLASS, fill=WAITCLASS)) +
  geom_histogram(binwidth=.01, aes(weight=EST_COUNT)) +
  scale_color_manual(values=WCcol, guide=FALSE) +
  scale_fill_manual(values=WCcol, guide=FALSE) +
  scale_y_log10() +
  facet_wrap(~WAITCLASS)

# plot est and actual counts together: blue actual, red estimmated
ggplot(data=Events, aes(x=log10(TIME_WAITED)))  +
  geom_histogram(binwidth=.01, aes(color=I("red"), fill=I("red"),weight=EST_COUNT)) +
  geom_histogram(binwidth=.01, aes(color=I("blue"), fill=I("blue"))) +
  scale_y_log10() +
  facet_wrap(~WAITCLASS)
ggsave("./Plots/LatencyHist07.png", device="png")

################################################
# create estimated latency dataframe
LATS <-ddply(Events, .(INSTANCE_NUMBER,WAITCLASS,MINIT), summarize,
          AVG_LATENCY_MCS = 1000000*n()/sum(EST_COUNT))

# plot estimated latencies by waitclass and instance
ggplot(LATS) +
  geom_line(aes(x=MINIT, y=log10(AVG_LATENCY_MCS),color=INSTANCE_NUMBER),size=.5) +
  facet_grid(WAITCLASS ~ .)
ggsave("./Plots/LatencyHist08.png", device="png")
######
# new df with event names instead of instances
LATS2 <-ddply(Events, .(EVENTNAME,WAITCLASS,MINIT), summarize,
             AVG_LATENCY_MCS = 1000000*n()/sum(EST_COUNT),
             EST_COUNT = sum(EST_COUNT) )

# plot event histograms using log2 latency scale like v$event_histogram
ggplot(LATS2) +
  geom_histogram( bins=1000, aes( x=log10(AVG_LATENCY_MCS/1000), weight=EST_COUNT)) +
  facet_grid(WAITCLASS ~ .)


#
#
qplot(x=MINIT, y=log10(AVG_LATENCY), data=LATS, geom="line", color=INSTANCE_NUMBER) +
      facet_wrap(WAITCLASS ~ .)

# start herei
qplot(data=LATS,x=MINIT, y=AVG_LATENCY, geom="line",
      color=INSTANCE_NUMBER, facet=WAITCLASS ) + facet_wrap()

# focus on User I/O and Cluster waits latencies
ggplot(data=Events %>% filter(WAITCLASS=="User I/O" | WAITCLASS=="Cluster"), 
      aes(x=log10(TIME_WAITED), color=WAITCLASS, fill=WAITCLASS)) +
      geom_histogram(binwidth=.05) +
      scale_color_manual(values=WCcol) +
      scale_fill_manual(values=WCcol) +
      facet_grid(.~WAITCLASS)

# all waitclasses weighted by EST_COUNT
ggplot(data=Events, 
  aes(x=log10(TIME_WAITED), color=WAITCLASS, fill=WAITCLASS)) +
  geom_histogram(binwidth=.05, aes(weight=EST_COUNT)) +
  scale_color_manual(values=WCcol,guide=FALSE) +
  scale_fill_manual(values=WCcol, guide=FALSE) +
  scale_y_log10() +
  facet_wrap(~WAITCLASS)


