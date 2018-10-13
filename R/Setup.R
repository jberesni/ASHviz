# drop everything
rm(list=ls())
##############
# proiject directory
setwd("~/Desktop/NoCOUG2018")
# load libraries used
library("plyr")
library("dplyr")
library("ggplot2")
library("pryr")
#library(stats)
# 
require("ggplot2")
require("plyr")
################ event and dump file names ###################
EventFileName<- "~/Desktop/Professional/ASHdumps/EventName/event_name_11g.txt"

# get list of trc files for stmail
ASHfiles <- list.files(path="~/Desktop/Professional/ASHdumps/STMAIL",pattern="(trc)$"
                       ,full.names=TRUE)
#
#################### create V$EVENT_NAME data frame ####################
# data doctored from Oaktable contributed file, skip header record
fw<-c(10,65,10,20) # use deterministic field breaks
# read the file and assign col names
EventNames <-read.fwf(EventFileName, fw,skip=1,
                    col.names=c("EVENT_ID","EVENTNAME","WAITCLASS_ID","WAITCLASS"))
# make all cols factors

EventNames$EVENT_ID <- as.factor(EventNames$EVENT_ID)
EventNames$WAITCLASS_ID <- as.factor(EventNames$WAITCLASS_ID)
#
# trim whitespace from names
EventNames$EVENTNAME <- trimws(EventNames$EVENTNAME)
EventNames$WAITCLASS <- trimws(EventNames$WAITCLASS)
#
rm(fw) # cleanup
#
str(EventNames)
####################### create ASH data frame ########################
# load ASH dump trace files into data frame: comma-delim, no headers
ashDF <- data.frame()
tmp <- data.frame()
for (fileName in ASHfiles) {
    tmp <- read.csv(fileName,header=FALSE)
    # discard last sample rows as they may not be fixed up
    ashDF <- rbind(ashDF,tmp)
}
rm(tmp) # cleanup
#
# assign headers to ashDF using row from header file 
#               (BUG:header line was missing comma)
foo <- pipe("cat ~/Desktop/Professional/ASHdumps/STMAIL/stmail_ashdump_headers.txt | 
            awk '/TRACE DUMP HEADER BEGIN/ {getline;print;exit;}' -", open="r")
Hdr <- read.csv(foo)
colnames(ashDF) <- colnames(Hdr)
close(foo)
rm(foo,Hdr) # cleanup
#
# add STATE column: CPU or WAIT
# NOTE:samples from long waits (>1sec) will incorrectly have state=CPU prior to last sample
ashDF$STATE <- as.factor(ifelse(ashDF$TIME_WAITED >0,c("WAIT"),c("CPU")))

# make all columns factors exept time_waited, wait_time, sample_id
cols <- names(ashDF)
ashDF[cols] <- lapply(ashDF[cols], factor)
rm(cols) # cleanup
#
# coerce specific variables back to integers
ashDF$TIME_WAITED <- as.integer(as.character(ashDF$TIME_WAITED))
ashDF$WAIT_TIME <- as.integer(as.character(ashDF$WAIT_TIME))
ashDF$SAMPLE_ID <- as.integer(as.character(ashDF$SAMPLE_ID))
#
## get sample_time into POSIXct with microsecond timing
#  firstset digits.secs to microseconds
options(digits.secs=6)
# convert sample time to POSIXct type (NOTE: using "OS6" does not error but fails)
ashDF$SAMPLE_TIME<-as.POSIXct(strptime(ashDF$SAMPLE_TIME, "%m-%d-%Y %H:%M:%OS"))
# sanity check first few sample times for microseconds
head(ashDF$SAMPLE_TIME)
#
# compute estimated count using time_waited and default ASH interval
# time_waited >0 means the sample was waiting and has been fixed up
ashDF$EST_COUNT <- as.integer(if_else(ashDF$TIME_WAITED >0 & ashDF$TIME_WAITED <1000000, 
                           round(1000000/ashDF$TIME_WAITED),1))
#
glimpse(tbl_df(ashDF)) # before row removal
# remove last sample rows for each instance (not fixed up)
ashDF <- ashDF %>% 
         group_by(INSTANCE_NUMBER) %>% 
         filter(SAMPLE_ID < max(SAMPLE_ID))
#
#ashDF <- ungroup(ashDF) # 
#
# join in EventNames by event_id, gets warning (?)
ashDF <- ashDF %>% left_join(EventNames,"EVENT_ID")

# set waitclass and eventname to "unknown" for un-joined waits
ashDF<-ashDF %>% 
       mutate(EVENTNAME = ifelse(!is.na(EVENT_ID) & is.na(EVENTNAME),"unknown",EVENTNAME)
             ,WAITCLASS = ifelse(!is.na(EVENT_ID) & is.na(WAITCLASS),"Unknown",WAITCLASS)
# can't get this right  ,WAITCLASS_ID = ifelse(!is.na(EVENT_ID) & is.na(WAITCLASS_ID),as.double(0),WAITCLASS_ID )
       )

# set waitclass and eventname to "CPU" for non-wait samples
ashDF$WAITCLASS <- if_else(as.character(ashDF$STATE)=="WAIT",
                         as.character(ashDF$WAITCLASS),
                         as.character(ashDF$STATE))
#
ashDF$EVENTNAME <- if_else(as.character(ashDF$STATE)=="WAIT",
                           as.character(ashDF$EVENTNAME),
                           as.character(ashDF$STATE))

#
# MINIT = SAMPLE_TIME truncated to minute
ashDF$MINIT <- as.POSIXct(trunc(ashDF$SAMPLE_TIME,units="mins"))

# re-cast to factors for consistency
cols <- c("EVENTNAME","WAITCLASS","EVENT_ID")
ashDF[cols] <- lapply(ashDF[cols],factor)
rm(cols) # cleanup

# glimpse qt and size of final product
glimpse(tbl_df(ashDF)) # after row removal
object_size(ashDF)
###########################
# END of ashDF setup
###########################
