#Grouped bar plots created by Chad C. Williams from the University of Victoria's Neuroeconomics Laboratory, 2016.

###################################################################################################################

                                    ###MODIFY THIS SECTION TO YOUR LIKING###                                                                        

###################################################################################################################

#First, change your working directory to the folder with your data
#Data must be laid out as Factor, Condition1, Condition2, Condition3, Condition4

#Filenames
filename = "grouped_plot_data.txt"
OutputName = "GroupedPlot.tiff"

#Condition Names
Condition1 = "Delta"
Condition2 = "Theta"
Condition3 = "Alpha"
Condition4 = "Beta"


###Plot for all data###

#Colours of your bars
  groupcolours = c('#404040','#808080')
#Colours of your error bars
  grouperrorcol = "#606060"
#Y Axis Values range
  groupylim = range(0,25)
#Y Axis label
  groupylab = expression(paste("Power (", mu, "V"^2,")", sep ="")) #Power (μV^2)

###################################################################################################################

                ###DO NOT CHANGE ANYTHING BEYOND THIS POINT UNLESS YOU KNOW WHAT YOU ARE DOING###

###################################################################################################################

#Load Packages
library(reshape2)
library(ggplot2)
library(Hmisc)

#Load Data
data = read.table(filename)
#Change column names - This is important because it will be what appears on the x axis of your plot. 
#This was determined at the top of the script
colnames(data) = c("Factor",Condition1,Condition2,Condition3,Condition4)
#The melt function transforms the columns in the measured variable from your bardata data frame. Look over the new 
#data frame and compare it to your original table to ensure you understand what the function did. 
longdata = melt(data, id = c("Factor"))

###################################################################################################################

#Plot Data - Creates your variable, designating the x and y column from your data frame and grouped by factor
Plot = ggplot(longdata, aes(x=variable, y=value, fill=Factor))
print(  #Because ggplot is meant to be done in the console tab, we must print any ggplot in a script. 
        #Print essentially puts it into the console tab
Plot #Here, we recall plot so we can add properties to the variable, anything below with a + in front of it is us adding a property
   +stat_summary(fun.y = mean, #Calculates the mean of each condition
                 geom = "bar", #Determines how the mean will be presented
                 width=0.8, #Size of bars
                 position = position_dodge(width=0.85)) # Distance between bars within each level
   +scale_fill_manual(values=groupcolours) #Colours of the bars
   +stat_summary(fun.data = mean_cl_normal, #Calculates the 95% confidence interval of each condition
                 geom = "errorbar", #Determines how the confidence interval will be presented
                 width = .2, #This is the length of the horizontal line of your error bars
                 size = 1, #This is the thickness of your entire error bar
                 colour = grouperrorcol, #This is the colour of your error bar. This was determined at the top of the script
                 position=position_dodge(0.85)) #Displaces the error bars so it lines up with the factors
   +scale_y_continuous(expand = c(0,0)) #This makes it so that your bars touch the x axis (otherwise there will be a gap)
   +coord_cartesian(ylim = groupylim) #This determines the y axis limits. This was determined at the top of the script
   +xlab("") #Because each condition has a label, we have removed a general x label, but you could have something such as Frequency for this data
   +ylab(groupylab) #This is the label for the y axis. This was determined at the top of the script

   #Legend
   +theme_bw() #ggplot has several themes. You can look up different one's to see what it has to offer
   + theme(legend.position = c(0.8, 0.8)) #Position of a legend
   + theme(legend.text=element_text(size=13)) #Text size within the legend
   + theme(legend.key.size = unit(.6, "cm")) #Size of legend box colour
   + theme(legend.key = element_rect(colour = FALSE)) #Remove borders around colours
   + theme(legend.title=element_blank()) #Removed title of legend

   #Background and border
   +geom_hline(yintercept = c(0) ) #Here we create a line to indicate the 0 score on the y axis
   +theme(plot.margin=unit(c(.5,.5,.5,.5),"cm")) #Adds white space around your plot
   +theme(axis.line.x = element_line(color="black", size = 0.5), #This adds a x axis line
       axis.line.y = element_line(color="black", size = 0.5), #This adds a y axis line
       panel.grid.major = element_blank(), #Removes grid
       panel.grid.minor = element_blank(), #Removes more grid
       panel.background = element_blank(), #Removes grey background
       panel.border = element_blank())) #Removes lines around the plot

###################################################################################################################
    #Save Plot - Here, we can save the plot as an image. This will save the current plot in your R plot tab. 
    #Output name was determined at the top of the script
   ggsave(filename = OutputName, width = 6.54, height = 4.36, dpi = 600)
