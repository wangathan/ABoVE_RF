#####################################
#
#		To build a big grid of the confusion matrix
#
#
#

library(caret)
library(ggplot2)
library(data.table)

conf_pam20 = get(load('../../data/rf/clusters_20171201/conf_pam20_remap'))
table20 = conf_pam20$table

referenceTotals = colSums(table20)

table20prop = lapply(1:ncol(table20), function(x)table20[,x]/referenceTotals[x])
table20prop = do.call(cbind,table20prop)
colnames(table20prop) = rownames(table20prop)
round(table20prop, 2)*100

plot <- ggplot(confusion)
plot + geom_tile(aes(x=Var1, y=Var2, fill=Freq)) + 
	scale_x_discrete(name="Actual Class") + 
	scale_y_discrete(name="Predicted Class") + 
	scale_fill_gradient(breaks=seq(from=-.5, to=4, by=.2)) + 
	labs(fill="Normalized\nFrequency")

TClass <- factor(c(0, 0, 1, 1))
PClass <- factor(c(0, 1, 0, 1))
Y      <- c(2816, 248, 34, 235)
df <- data.frame(TClass, PClass, Y)

confdt = data.table(TClass = factor(rep(1:10,10)),
										PClass = factor(unlist(lapply(10:1,function(x)rep(x,10)))),
										Y = unlist(lapply(1:10,function(x)table20[,x])),
										Yp = unlist(lapply(1:10,function(x)table20prop[,x])))

										#totals = factor(unlist(lapply(10:1,function(x)rep(x,10)))),
axislabsx = c("1" = "Barren", "2" = "Decid. F", "3" = "Everg. F", "4" = "Herbaceous", "5" = "Mixed F", "6" = "Shallows", "7" = "Water", "8" = "Wetland", "9" = "Woodland", "10" = "Woody Wetland")
axislabsy = c("10" = "Barren", "9" = "Decid. F", "8" = "Everg. F", "7" = "Herbaceous", "6" = "Mixed F", "5" = "Shallows", "4" = "Water", "3" = "Wetland", "2" = "Woodland", "1" = "Woody Wetland")

ggplot(data =  confdt, mapping = aes(x = TClass, y = PClass)) +
	geom_tile(aes(fill = Yp), colour = "white") +
	geom_text(aes(label = sprintf("%1.0f", Y)), vjust = 1, size = 6) +
	scale_fill_gradient("Proportion",low = "#3333FF", high = "#FF3333", breaks=seq(from=0,to=1,by=0.1)) +
	theme_bw() + 
	ylab("Predicted Class") + xlab("Reference Class") + 
	scale_x_discrete(labels = axislabsx) + 
	scale_y_discrete(labels = axislabsy) +  
	theme(axis.text.x = element_text(angle = -35, vjust = 1, hjust = 0),
				axis.text = element_text(size = 12)) + 
ggtitle("Confusion Matrix of Classifier")
	# + theme(legend.position = "none")
