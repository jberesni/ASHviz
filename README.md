# ASHviz
Visualizations of ASH dump data using R and ggplot

Abstract: NoCOUG presentation

Active Session History (ASH) data is a high dimensional, fine-grained record of Oracle database activity over time.  Looking into ASH data can provide a detailed view of what happened in a database, however using SQL for this purpose requires aggregation or high-level of filtering to produce interpretable results. Looking at lots of data over many dimensions really requires data visualization.

This presentation will briefly discuss the ASH mechanism, its key features, and the visualization of ASH data as presented in Enterprise Manager. Next we present several visualization investigations into a set of ASH dumps collected from a 4-node RAC cluster using the R programming language and the ggplot package to visualize the data. Some of the investigations make use of a little known technique for estimating event counts from ASH samples. It is hoped that participants will have new or renewed enthusiasm for ASH data and perhaps even an interest in data visualization.

Repository Files:

