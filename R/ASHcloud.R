# run Setup.R to get ASHdf created properly
# source("./Setup.R")
##############################################################
# Data Frame: Starting from ashDF, ungroup and select desired columns,
# add factor EVT_ID: concatenation of waitclass id and event id
myDF <- ashDF %>% 
  ungroup() %>%
  select(WAITCLASS,WAITCLASS_ID, EVENT_ID, EVENTNAME, TIME_WAITED,EST_COUNT,MINIT) %>%
  mutate(EVT_ID=as.factor(paste(WAITCLASS_ID,EVENT_ID)))
#############################################################

###############################################################
# Series 1: scatter plot all sampled events grouped by waitclass

# Plot log10(estimated event counts) and log10(time waited)
# Seeing all data is useful first start
# Try polar coordinates idea
#
# first plot base layer
# use waits only and plot all points by event within wait on x-axis (reorder)
p0 <- ggplot(myDF %>% filter(WAITCLASS != "CPU")
             ,aes(x=reorder(EVT_ID,WAITCLASS_ID))
)

# plot log10(EST_COUNT) and log10(TIME_WAITED)
p0 + geom_point(aes(y=log10(EST_COUNT)))
p0 + geom_point(aes(y=log10(TIME_WAITED)))

# ..add color by waitclass
p0 + geom_point(aes(color=WAITCLASS, y=log10(EST_COUNT)))

# ..shrink point size to reduce overplotting
p0 + geom_point(aes(color=WAITCLASS, y=log10(EST_COUNT)), size=.01)

# ..jitter position to further spread out overplotting
p0 + geom_point(aes(color=WAITCLASS, y=log10(EST_COUNT)), size=.01, position="jitter")

# ..eliminate x-axis labels as they are illegible, meaningless and ugly
p0 + geom_point(aes(color=WAITCLASS, y=log10(EST_COUNT)), size=.01, position="jitter") +
     scale_x_discrete(labels=NULL)

# ..add name for x-axis EVT_ID
p0 + geom_point(aes(color=WAITCLASS, y=log10(EST_COUNT)), size=.01, position="jitter") +
     scale_x_discrete(labels=NULL, name="Event ID by Waitclass")

# ..add red reference lines at 10 and 10^3 event count levels
p0 + geom_point(aes(color=WAITCLASS, y=log10(EST_COUNT)), size=.01, position="jitter") +
     scale_x_discrete(labels=NULL, name="Event ID by Waitclass") +
     geom_hline(yintercept=3,color="red",size=.2,show.legend=FALSE) +
     geom_hline(yintercept=1,color="red",size=.2,show.legend=FALSE)

# ..use polar coordinates with single reference line (circle) at 10^3
p0 + geom_point(aes(color=WAITCLASS, y=log10(EST_COUNT)), size=.01, position="jitter") +
     scale_x_discrete(labels=NULL, name="Event ID by Waitclass") +
     geom_hline(yintercept=3,color="red",size=.2,show.legend=FALSE) +
     coord_polar()

# ..add limits on radius high value to conserve space
p0 + geom_point(aes(color=WAITCLASS, y=log10(EST_COUNT)), size=.01, position="jitter", alpha=.3) +
     scale_x_discrete(labels=NULL, name="Event ID by Waitclass") +
     geom_hline(yintercept=3,color="red",size=.2,show.legend=FALSE) +
     coord_polar() + ylim(0,5)

# ..plot log10(TIME_WAITED) using same plotting features
p0 + geom_point(aes(color=WAITCLASS, y=log10(TIME_WAITED)), size=.01, position="jitter", alpha=.3) +
  scale_x_discrete(labels=NULL, name="Event ID by Waitclass") +
  geom_hline(yintercept=3,color="red",size=.2,show.legend=FALSE) +
  coord_polar() + ylim(0,5)
#
### end Series 1 scatter plotting all events
#
###############################################################################
# Series 2: Bubble charts of event/class DB time and avg latency computed using
# the event count estimator over 1-minute intervals in the ASH time period.
#
# Show changes in latency AND total time spent over time
# Like the Perf Page but with the time "bubbled", area of bubble = DB Time (AAS)
#
############
# Data Frames
# df0 computes DB time, total count, and average latency by class/event/minit
df0 <- myDF %>% ungroup() %>% select(EVENTNAME,WAITCLASS,MINIT,EST_COUNT) %>%
       group_by(WAITCLASS, EVENTNAME, MINIT) %>%
       summarize(DB_TIME_US=n()*10^6,TOT_EST_COUNT=sum(EST_COUNT), AVG_LATENCY_US=DB_TIME_US/TOT_EST_COUNT
                 ,AAS=n()/60)
#####
# df1 is same computations as df0 over waitclass/minit 
df1 <- myDF %>% ungroup() %>% select(WAITCLASS,MINIT,EST_COUNT) %>%
  group_by(WAITCLASS, MINIT) %>%
  summarize(DB_TIME_US=n()*10^6,TOT_EST_COUNT=sum(EST_COUNT), AVG_LATENCY_US=DB_TIME_US/TOT_EST_COUNT
            ,AAS=n()/60)
###
# base plot: waits only, by event, x=minute, y=log10(avg latency), color by waitclass
p0 <- ggplot(data=df0 %>% filter(WAITCLASS != "CPU")
            ,aes(x=MINIT, y=log10(AVG_LATENCY_US), color=WAITCLASS))

# geom line: one line per event faceted by waitclass
p0 + geom_line(aes(group=EVENTNAME)) + facet_grid(WAITCLASS ~.)

# geom point: bubble chart sized by DB time (microsecs) one bubble per event/min
p0 + geom_point(aes(size=DB_TIME_US))

# new base plot using df1 (grouped by waitclass)
p0 <- ggplot(data=df1 %>% filter(WAITCLASS != "CPU")
             ,aes(x=MINIT, y=log10(AVG_LATENCY_US), color=WAITCLASS))

# ..faceted by waitclass
p0 + geom_point(aes(size=DB_TIME_US)) + facet_grid(WAITCLASS ~.)

# ..alpha by total estimated wait count (make infrequent less visible) 
#   with limits, remove faceting
p0 + geom_point(aes(size=DB_TIME_US, alpha=log10(TOT_EST_COUNT))) + ylim(3,5)

# ..use scale_size_area to make area proportional to DB time (even at small vales, i.e. zero=invisible)
p0 + geom_point(aes(size=DB_TIME_US, alpha=log10(TOT_EST_COUNT))) + ylim(3,5) + scale_size_area()

# ..remove alpha adjustment
p0 + geom_point(aes(size=DB_TIME_US)) + ylim(3,5) + scale_size_area()
##
## ...use AAS for bubble area, same look but more meaningful
p0 + geom_point(aes(size=AAS)) + ylim(3,5) + scale_size_area()
#
# set colors for wait classes  (TODO:get perf page colors)
WCcolors <- c(Application = "yellow",
            Concurrency = "yellow",
            "System I/O"= "skyblue",
            "User I/O" = "royalblue",
            Unknown = "pink1",
            Idle = "pink2",
            Cluster = "red",
            Commit = "green",
            Configuration = "grey0",
            Network = "black",
            Other = "pink",
            CPU = "green")
###
# ..use scale_color_manual to set waitclass colors
p0 + geom_point(aes(size=AAS)) + ylim(3,5) + scale_size_area() + 
     scale_color_manual(
       values=WCcolors,
       breaks=c("User I/O","System I/O","Application","Commit","Concurrency","Cluster",
                "Configuration","Network","Other","Idle","Unknown","CPU")
       )
###
# use TOT_COUNT for bubble size
p0 + geom_point(aes(size=TOT_EST_COUNT)) + ylim(3,5) + scale_size_area() + 
  scale_color_manual(
    values=WCcolors,
    breaks=c("User I/O","System I/O","Application","Commit","Concurrency","Cluster",
             "Configuration","Network","Other","Idle","Unknown","CPU")
  ) +
  scale_size_continuous(name="Events/Min") # set label for size scale legend
#####
## base plot isolates CPU, Cluster and User I/O classes only
p1 <- ggplot(data=df1 %>% 
               filter(WAITCLASS == "CPU" | WAITCLASS == "Cluster" | WAITCLASS == "User I/O")
            ,aes(x=MINIT, y=log10(AVG_LATENCY_US), color=WAITCLASS))

# geom point: CPU shows up with latency=1 sec, or log10(AVG_LATENCY)=6 
p1 + geom_point(aes(size=AAS)) + ylim(3,6) + scale_size_area() + 
  scale_color_manual(
    values=WCcolors,
    breaks=c("User I/O","System I/O","Application","Commit","Concurrency","Cluster",
             "Configuration","Network","Other","Idle","Unknown","CPU")
  )

# ..add lines between bubbles
p1 + geom_point(aes(size=AAS)) + ylim(3,6) + scale_size_area() + 
  scale_color_manual(
    values=WCcolors,
    breaks=c("User I/O","System I/O","Application","Commit","Concurrency","Cluster",
             "Configuration","Network","Other","Idle","Unknown","CPU")
  ) + geom_line(aes(group=WAITCLASS))

# new base plot same as above except based on df0
# so aggregated over each event
p1 <- ggplot(data=df0 %>% 
               filter(WAITCLASS == "CPU" | WAITCLASS == "Cluster" | WAITCLASS == "User I/O")
             ,aes(x=MINIT, y=log10(AVG_LATENCY_US), color=WAITCLASS))

# ..take away manual color scale and show each event 
p1 + geom_point(aes(size=AAS)) + ylim(3,6) + scale_size_area()

# ..color by eventname instead of waitclass
p1 + geom_point(aes(size=AAS, color=EVENTNAME)) + ylim(3,6) + scale_size_area()

# ..add lines to track each eventname over time 
p1 + geom_point(aes(size=AAS, color=EVENTNAME)) + ylim(3,6) + scale_size_area() +
  geom_line(aes(group=EVENTNAME, color=EVENTNAME, size=.05))

# ..facet by EVENTNAME 
p1 + geom_point(aes(size=AAS, color=EVENTNAME)) + ylim(3,6) + scale_size_area() +
  geom_line(aes(group=EVENTNAME, color=EVENTNAME, size=.05)) + facet_wrap(~EVENTNAME)

##############################################################
## Idea: radial (polar) bubble chart
## size=AAS, radius=latency, theta=waitclass, color=waitclass
######
# create similar dataframe to df0,df1 but exclude CPU 
df2 <- myDF %>% ungroup() %>% filter(WAITCLASS != "CPU") %>%
  select(EVENTNAME,WAITCLASS,EVT_ID,EST_COUNT,MINIT) %>%
  group_by(WAITCLASS, EVENTNAME, EVT_ID) %>%
  summarize(DB_TIME_US=n()*10^6
           ,TOT_EST_COUNT=sum(EST_COUNT)
           ,AVG_LATENCY_US=DB_TIME_US/TOT_EST_COUNT
           ,MINITS=(max(as.integer(MINIT)) - min(as.integer(MINIT)))/60
           ,ND=n_distinct(MINIT)
           ,AAS=n()/(ND*60)
  )

p2 <- ggplot(df2, aes(x=reorder(EVT_ID,WAITCLASS)))

p2 + geom_point(aes(y=log10(AVG_LATENCY_US),color=WAITCLASS,size=AAS)) +
     scale_size_area()

p2 + geom_point(aes(y=log10(AVG_LATENCY_US),color=WAITCLASS,size=AAS)) +
  scale_size_area() +
  coord_polar()

p2 + geom_point(aes(y=log10(AVG_LATENCY_US),color=WAITCLASS,size=AAS)) +
  scale_size_area() +
  coord_polar() +
  scale_x_discrete(labels=NULL, name="Event ID by Waitclass")

### new plot, aggregate by waitclass
df3 <- df2 %>% ungroup() %>%
      group_by(WAITCLASS) %>%
      summarize(TOT_COUNT=sum(TOT_EST_COUNT)
               ,TOT_DBTME_US=sum(DB_TIME_US)
               ,AVG_LATENCY_US=sum(DB_TIME_US)/TOT_COUNT
               ,AAS=sum(AAS)
               )

p3 <- ggplot(data=df3,
             aes(x=WAITCLASS, y=log10(AVG_LATENCY_US), color=WAITCLASS))

p3 + geom_point(aes(size=AAS))

p3 + geom_point(aes(size=AAS)) + scale_size_area() + coord_polar()

p3 + geom_point(aes(size=TOT_COUNT)) + scale_size_area() + coord_polar()

#### clean up workspace
rm(p0,p1,p2)
rm(myDF)
rm(df0,df1,df2,df3)
rm(WCcolors)
