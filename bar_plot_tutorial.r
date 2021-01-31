#Bar plots created by Chad C. Williams from the University of Victoria's Neuroeconomics Laboratory, 2016.

###################################################################################################################

                                      ###MODIFY THIS SECTION TO YOUR LIKING###                                                                        

###################################################################################################################
#This section is an easy way to set many of the variables that we will use later when plotting. This is a practice you should 
#get familiar with. The reason is, if you need to change something quickly (such as bar colours), you don't have to go 
#search through your script to find it. You can come right here and change it quickly. It is also helpful if you use the
#same variable multiple times. That way, you change it once up here and it all changes below.

#First, change your working directory to the folder with your data
#Data must be laid out as Condition1, Condition2, Condition3, Condition4

#Filenames
filename = "bar_plot_data.txt"
OutputName = "BarPlot.tiff"

#Condition Names
Condition1 = "Delta"
Condition2 = "Theta"
Condition3 = "Alpha"
Condition4 = "Beta"

###Plot for all data###

#Colours of your bars
  barcolours = c("#303030", "#505050", "#707070", "#909090")
#Colours of your error bars
  errorbarcolour = "#606060"
#Y Axis Values range
  barylim = range(-2,6)
#Y Axis label
  barylab = expression(paste("Power (", mu, "V"^2,")", sep ="")) #Power (μV^2)

###################################################################################################################

                ###DO NOT CHANGE ANYTHING BEYOND THIS POINT UNLESS YOU KNOW WHAT YOU ARE DOING###

###################################################################################################################
  
#Load Packages
library(reshape2)
library(ggplot2)

#Load Data
data = read.table(filename)
#Change column names - This is important because it will be what appears on the x axis of your plot. This was determined at the top of the script
colnames(data) = c(Condition1, Condition2, Condition3, Condition4)
#The melt function transforms the columns in the measured variable from your bardata data frame. Look over the new 
#data frame and compare it to your original table to ensure you understand what the function did. 
longdata = melt(data, measured = c(Condition1, Condition2, Condition3, Condition4))

###################################################################################################################

#Plot Data
Plot = ggplot(longdata, aes(x=variable, y=value)) #Creates your variable, designating the x and y column from your data frame
print( #Because ggplot is meant to be done in the console tab, we must print any ggplot in a script. 
       #Print essentially puts it into the console tab
Plot #Here, we recall plot so we can add properties to the variable, anything below with a + in front of it is us adding a property
   +stat_summary(fun.y = mean, #Calculates the mean of each condition
                 geom = "bar", #Determines how the mean will be presented
                 fill = (values=barcolours) #These are the colours of each bar. This was determined at the top of the script
                 )
   +stat_summary(fun.data = mean_cl_normal, #Calculates the 95% confidence interval of each condition
                 geom = "errorbar", #Determines how the confidence interval will be presented
                 width = .2, #This is the length of the horizontal line of your error bars
                 size = 1, #This is the thickness of your entire error bar
                 colour = errorbarcolour) #This is the colour of your error bar. This was determined at the top of the script
   +scale_y_reverse(expand = c(0,0)) #This makes it so that your bars touch the x axis (otherwise there will be a gap)
   +coord_cartesian(ylim = barylim) #This determines the y axis limits. This was determined at the top of the script
   +xlab("") #Because each condition has a label, we have removed a general x label, but you could have something such as Frequency for this data
   +ylab(barylab) #This is the label for the y axis. This was determined at the top of the script
   + geom_hline(yintercept = 0) #This data is both positive and negative, here we create a line to indicate the 0 score on the y axis

   #Background and border - To create a professional manuscript plot, the following makes your plot standard. 
   +theme(plot.margin=unit(c(.5,.5,.5,.5),"cm")) #Adds white space around your plot
   +theme_bw() #ggplot has several themes. You can look up different one's to see what it has to offer.
   +theme(axis.line.x = element_line(color="black", size = 0.5), #This adds a x axis line
          axis.line.y = element_line(color="black", size = 0.5), #This adds a y axis line
       panel.grid.major = element_blank(), #Removes grid
       panel.grid.minor = element_blank(), #Removes more grid
       panel.background = element_blank(), #Removes grey background
       panel.border = element_blank(), #Removes lines around the plot
       legend.position = 'none')) #Removes any legend


###################################################################################################################
   #Save Plot - Here, we can save the plot as an image. This will save the current plot in your R plot tab. 
   #Output name was determined at the top of the script
   ggsave(filename = OutputName, width = 6.54, height = 4.36, dpi = 600)
