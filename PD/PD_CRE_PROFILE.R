##########
# Profiling Plots


##########
# Start
# Plot  average PD by risk rating
# Risk rating
cre_dev_training$risk_rate <- ifelse(cre_dev_training$boh_rating1_R1==1,"R1",ifelse(cre_dev_training$boh_rating1_R2==1,"R2","R3"))

#Making average actual default by risk rating
defaulters_risk <- cre_dev_training %>% group_by(fileDate, risk_rate) %>% summarise(Defaulters = sum(y)) %>% data.frame()
total_risk <- cre_dev_training %>% group_by(fileDate, risk_rate) %>% count() %>% data.frame()
defaulters_risk <- merge(defaulters_risk, total_risk)
colnames(defaulters_risk) <- c("fileDate", "Risk","Default","Total")
defaulters_risk <- defaulters_risk %>% group_by(fileDate, Risk) %>% mutate(default_per = Default/Total) %>% data.frame()
head(defaulters_risk)

#Make actual nondefault by risk rating
nondefault_risk <- cre_dev_training %>% group_by(fileDate, risk_rate) %>% filter(y==0) %>% count() %>% data.frame()
colnames(nondefault_risk) <- c("fileDate", "Risk","NonDefault")
risk_df <- merge(x = defaulters_risk, y = nondefault_risk, by.x = c("fileDate","Risk"), by.y = c("fileDate","Risk"))

risk_df <- risk_df %>% group_by(fileDate,Risk) %>% mutate(nondefault_per = NonDefault/Total) %>% data.frame() 
risk_df <- risk_df %>% group_by(Risk) %>% mutate(pd_actual = default_per/lag(nondefault_per)) %>% data.frame() %>% na.omit()
head(risk_df)

pd_actual_risk <- aggregate(risk_df$pd_actual, list(risk_df$Risk), mean)
colnames(pd_actual_risk) <- c("Risk","PD_Actual")

##########

# Plot Avereage PD by Risk Rating
#Observations per bin
risk_rate_in_graph <- as.data.frame(count(cre_dev_training, risk_rate))
# Mean PD of each bin
risk_rate_in_graph$mean <- aggregate(cre_dev_training$p_hat, list(cre_dev_training$risk_rate), mean)[,2]

colnames(risk_rate_in_graph) <- c("Risk","Observations","Mean")

df <- merge(risk_rate_in_graph, pd_actual_risk)

green <- rgb(.5, 1, .5, alpha=0.2)

pdf(paste("./R output", paste("Default Rate - ",colnames(df)[1],"_PROF.pdf", sep=""),sep ="/"), height = 5, width = 10)
par(mar = c(5,5,2,5))

par(new = T)
with(df, plot(as.numeric(row.names(df)), PD_Actual, col="blue", xaxt = "n",type = "l", lwd=2,
              ylab="", xlab="", ylim=c(0,max(apply(df[,3:4], 2, max)))))
par(new = T)
bp <- with(df, barplot(df$Observations,axes=F, xlab=NA, ylab=NA, col=green))
axis(side = 4)
mtext(side = 4, line = 3, 'Observation Count')
legend("topleft",
       legend=c("Average PD","Obs."),
       lty=1,lwd=5, col=c("blue", green))
axis(1, at=bp, labels=df[,1])
dev.off()

##########

# End
# Plot  average PD by risk rating
##########

##########
# Start
# Plot  average PD by CA UR

#CA UR Bins
cre_dev_training$caur_bins <- cut(cre_dev_training$CAUR_yd,breaks = 4)

# Make actual default 
defaulters_ur <- cre_dev_training %>% group_by(fileDate, caur_bins) %>% summarise(Defaulters = sum(y)) %>% data.frame()
total_ur <- cre_dev_training %>% group_by(fileDate, caur_bins) %>% count() %>% data.frame()
defaulters_ur <- merge(defaulters_ur, total_ur)
colnames(defaulters_ur) <- c("fileDate", "UR","Default","Total")
defaulters_ur <- defaulters_ur %>% group_by(fileDate, UR) %>% mutate(default_per = Default/Total) %>% data.frame()
head(defaulters_ur)

#Make actual nondefault
nondefault_ur <- cre_dev_training %>% group_by(fileDate, caur_bins) %>% filter(y==0) %>% count() %>% data.frame()
colnames(nondefault_ur) <- c("fileDate", "UR","NonDefault")
ur_df <- merge(x = defaulters_ur, y = nondefault_ur, by.x = c("fileDate","UR"), by.y = c("fileDate","UR"))

ur_df <- ur_df %>% group_by(fileDate,UR) %>% mutate(nondefault_per = NonDefault/Total) %>% data.frame() 
ur_df <- ur_df %>% group_by(UR) %>% mutate(pd_actual = default_per/lag(nondefault_per)) %>% data.frame() %>% na.omit()
head(ur_df)

ur_pd_actual <- aggregate(ur_df$pd_actual, list(ur_df$UR), mean)
colnames(ur_pd_actual) <- c("UR","PD_Actual")

#Observations per bin
ur_in_graph <- as.data.frame(count(cre_dev_training, caur_bins))
# Mean PD of each bin
ur_in_graph$mean <- aggregate(cre_dev_training$p_hat, list(cre_dev_training$caur_bins), mean)[,2]

colnames(ur_in_graph) <- c("UR","Observations","Mean")

df <- merge(ur_in_graph, ur_pd_actual)

green <- rgb(.5, 1, .5, alpha=0.2)

pdf(paste("./R output", paste("Default Rate - ",colnames(df)[1],"_PROF.pdf", sep=""),sep ="/"), height = 5, width = 10)
par(mar = c(5,5,2,5))

par(new = T)
with(df, plot(as.numeric(row.names(df)), PD_Actual, col="blue", xaxt = "n",type = "l", lwd=2,
              ylab="", xlab="", ylim=c(0,max(apply(df[,3:4], 2, max)))))
par(new = T)
bp <- with(df, barplot(df$Observations,axes=F, xlab=NA, ylab=NA, col=green))
axis(side = 4)
mtext(side = 4, line = 3, 'Observation Count')
legend("topleft",
       legend=c("Average PD","Obs."),
       lty=1,lwd=5, col=c("blue", green))
axis(1, at=bp, labels=df[,1])
dev.off()

# End
# Plot   average PD by CA UR
##########


##########
# Start
# Plot  average PD by CA HPI

#CA HPI Bins
cre_dev_training$cahpi_bins <- cut(cre_dev_training$CAHPI_ag,breaks = 3)

# Make actual default 
defaulters_hpi <- cre_dev_training %>% group_by(fileDate, cahpi_bins) %>% summarise(Defaulters = sum(y)) %>% data.frame()
total_hpi <- cre_dev_training %>% group_by(fileDate, cahpi_bins) %>% count() %>% data.frame()
defaulters_hpi <- merge(defaulters_hpi, total_hpi)
colnames(defaulters_hpi) <- c("fileDate", "HPI","Default","Total")
defaulters_hpi <- defaulters_hpi %>% group_by(fileDate, HPI) %>% mutate(default_per = Default/Total) %>% data.frame()
head(defaulters_hpi)

#Make actual nondefault
nondefault_hpi <- cre_dev_training %>% group_by(fileDate, cahpi_bins) %>% filter(y==0) %>% count() %>% data.frame()
colnames(nondefault_hpi) <- c("fileDate", "HPI","NonDefault")
hpi_df <- merge(x = defaulters_hpi, y = nondefault_hpi, by.x = c("fileDate","HPI"), by.y = c("fileDate","HPI"))

hpi_df <- hpi_df %>% group_by(fileDate,HPI) %>% mutate(nondefault_per = NonDefault/Total) %>% data.frame() 
hpi_df <- hpi_df %>% group_by(HPI) %>% mutate(pd_actual = default_per/lag(nondefault_per)) %>% data.frame() %>% na.omit()
head(hpi_df)

hpi_pd_actual <- aggregate(hpi_df$pd_actual, list(hpi_df$HPI), mean)
colnames(hpi_pd_actual) <- c("HPI","PD_Actual")

#Observations per bin
hpi_in_graph <- as.data.frame(count(cre_dev_training, cahpi_bins))
# Mean PD of each bin
hpi_in_graph$mean <- aggregate(cre_dev_training$p_hat, list(cre_dev_training$cahpi_bins), mean)[,2]

colnames(hpi_in_graph) <- c("HPI","Observations","Mean")

df <- merge(hpi_in_graph, hpi_pd_actual)

green <- rgb(.5, 1, .5, alpha=0.2)

pdf(paste("./R output", paste("Default Rate - ",colnames(df)[1],"_PROF.pdf", sep=""),sep ="/"), height = 5, width = 10)
par(mar = c(5,5,2,5))

par(new = T)
with(df, plot(as.numeric(row.names(df)), PD_Actual, col="blue", xaxt = "n",type = "l", lwd=2,
              ylab="", xlab="", ylim=c(0,max(apply(df[,3:4], 2, max)))))
par(new = T)
bp <- with(df, barplot(df$Observations,axes=F, xlab=NA, ylab=NA, col=green))
axis(side = 4)
mtext(side = 4, line = 3, 'Observation Count')
legend("topleft",
       legend=c("Average PD","Obs."),
       lty=1,lwd=5, col=c("blue", green))
axis(1, at=bp, labels=df[,1])
dev.off()


# End
# Plot  average PD by CA HPI
##########

##########
# Start
# Plot  average PD by GDP

#GDP Bins
cre_dev_training$gdp_bins <- cut(cre_dev_training$rgdp_qg_lag_2_neg, breaks = 2)

# Make actual default 
defaulters_gdp <- cre_dev_training %>% group_by(fileDate, gdp_bins) %>% summarise(Defaulters = sum(y)) %>% data.frame()
total_gdp <- cre_dev_training %>% group_by(fileDate, gdp_bins) %>% count() %>% data.frame()
defaulters_gdp <- merge(defaulters_gdp, total_gdp)
colnames(defaulters_gdp) <- c("fileDate", "GDP","Default","Total")
defaulters_gdp <- defaulters_gdp %>% group_by(fileDate, GDP) %>% mutate(default_per = Default/Total) %>% data.frame()
head(defaulters_gdp)

#Make actual nondefault
nondefault_gdp <- cre_dev_training %>% group_by(fileDate, gdp_bins) %>% filter(y==0) %>% count() %>% data.frame()
colnames(nondefault_gdp) <- c("fileDate", "GDP","NonDefault")
gdp_df <- merge(x = defaulters_gdp, y = nondefault_gdp, by.x = c("fileDate","GDP"), by.y = c("fileDate","GDP"))

gdp_df <- gdp_df %>% group_by(fileDate,GDP) %>% mutate(nondefault_per = NonDefault/Total) %>% data.frame() 
gdp_df <- gdp_df %>% group_by(GDP) %>% mutate(pd_actual = default_per/lag(nondefault_per)) %>% data.frame() %>% na.omit()
head(gdp_df)

gdp_pd_actual <- aggregate(gdp_df$pd_actual, list(gdp_df$GDP), mean)
colnames(gdp_pd_actual) <- c("GDP","PD_Actual")

#Observations per bin
gdp_in_graph <- as.data.frame(count(cre_dev_training, gdp_bins))
# Mean PD of each bin
gdp_in_graph$mean <- aggregate(cre_dev_training$p_hat, list(cre_dev_training$gdp_bins), mean)[,2]

colnames(gdp_in_graph) <- c("GDP","Observations","Mean")

df <- merge(gdp_in_graph, gdp_pd_actual, all.x = T)

green <- rgb(.5, 1, .5, alpha=0.2)

pdf(paste("./R output", paste("Default Rate - ",colnames(df)[1],"_PROF.pdf", sep=""),sep ="/"), height = 5, width = 10)
par(mar = c(5,5,2,5))

par(new = T)
with(df, plot(as.numeric(row.names(df)), PD_Actual, col="blue", xaxt = "n",type = "l", lwd=2,
              ylab="", xlab="", ylim=c(0,max(apply(df[,3:4], 2, function(x) max(x, na.rm=T))))))
par(new = T)
bp <- with(df, barplot(df$Observations,axes=F, xlab=NA, ylab=NA, col=green))
axis(side = 4)
mtext(side = 4, line = 3, 'Observation Count')
legend("topleft",
       legend=c("Average PD","Obs."),
       lty=1,lwd=5, col=c("blue", green))
axis(1, at=bp, labels=df[,1])
dev.off()

# End
# Plot  average PD by GDP
##########

##########
# Start
# Plot  average PD by Prop Type



# Make actual default 
defaulters_prop <- cre_dev_training %>% group_by(fileDate, property_type) %>% summarise(Defaulters = sum(y)) %>% data.frame()
total_prop <- cre_dev_training %>% group_by(fileDate, property_type) %>% count() %>% data.frame()
defaulters_prop <- merge(defaulters_prop, total_prop)
colnames(defaulters_prop) <- c("fileDate", "Prop_type","Default","Total")
defaulters_prop <- defaulters_prop %>% group_by(fileDate, Prop_type) %>% mutate(default_per = Default/Total) %>% data.frame()
head(defaulters_prop)

#Make actual nondefault
nondefault_prop <- cre_dev_training %>% group_by(fileDate, property_type) %>% filter(y==0) %>% count() %>% data.frame()
colnames(nondefault_prop) <- c("fileDate", "Prop_type","NonDefault")
prop_df <- merge(x = defaulters_prop, y = nondefault_prop, by.x = c("fileDate","Prop_type"), by.y = c("fileDate","Prop_type"))

prop_df <- prop_df %>% group_by(fileDate,Prop_type) %>% mutate(nondefault_per = NonDefault/Total) %>% data.frame() 
prop_df <- prop_df %>% group_by(Prop_type) %>% mutate(pd_actual = default_per/lag(nondefault_per)) %>% data.frame() %>% na.omit()
head(prop_df)

prop_pd_actual <- aggregate(prop_df$pd_actual, list(prop_df$Prop_type), mean)
colnames(prop_pd_actual) <- c("Prop_type","PD_Actual")

#Observations per bin
prop_in_graph <- as.data.frame(count(cre_dev_training, property_type))
# Mean PD of each bin
prop_in_graph$mean <- aggregate(cre_dev_training$p_hat, list(cre_dev_training$property_type), mean)[,2]

colnames(prop_in_graph) <- c("Prop_type","Observations","Mean")

df <- merge(prop_in_graph, prop_pd_actual, all.x = T)

green <- rgb(.5, 1, .5, alpha=0.2)

pdf(paste("./R output", paste("Default Rate - ",colnames(df)[1],"_PROF.pdf", sep=""),sep ="/"), height = 5, width = 10)
par(mar = c(5,5,2,5))

par(new = T)
with(df, plot(as.numeric(row.names(df)), PD_Actual, col="blue", xaxt = "n",type = "l", lwd=2,
              ylab="", xlab="", ylim=c(0,max(apply(df[,3:4], 2, function(x) max(x, na.rm=T))))))
par(new = T)
bp <- with(df, barplot(df$Observations,axes=F, xlab=NA, ylab=NA, col=green))
axis(side = 4)
mtext(side = 4, line = 3, 'Observation Count')
legend("topleft",
       legend=c("Average PD","Obs."),
       lty=1,lwd=5, col=c("blue", green))
axis(1, at=bp, labels=df[,1])
dev.off()

# End
# Plot  average PD by Prop type
##########

##########
# Start
# Plot average PD by POB

#POB Bins
cre_dev_outsample$pob_bins <- cut(cre_dev_outsample$POB,breaks = 6)

# Make actual default 
defaulters_pob <- cre_dev_outsample %>% group_by(fileDate, pob_bins) %>% summarise(Defaulters = sum(y)) %>% data.frame()
total_pob <- cre_dev_outsample %>% group_by(fileDate, pob_bins) %>% count() %>% data.frame()
defaulters_pob <- merge(defaulters_pob, total_pob)
colnames(defaulters_pob) <- c("fileDate", "POB","Default","Total")
defaulters_pob <- defaulters_pob %>% group_by(fileDate, POB) %>% mutate(default_per = Default/Total) %>% data.frame()
head(defaulters_pob)

#Make actual nondefault
nondefault_pob <- cre_dev_outsample %>% group_by(fileDate, pob_bins) %>% filter(y==0) %>% count() %>% data.frame()
colnames(nondefault_pob) <- c("fileDate", "POB","NonDefault")
pob_df <- merge(x = defaulters_pob, y = nondefault_pob, by.x = c("fileDate","POB"), by.y = c("fileDate","POB"))

pob_df <- pob_df %>% group_by(fileDate,POB) %>% mutate(nondefault_per = NonDefault/Total) %>% data.frame() 
pob_df <- pob_df %>% group_by(POB) %>% mutate(pd_actual = default_per/lag(nondefault_per)) %>% data.frame() %>% na.omit()
head(pob_df)

pob_pd_actual <- aggregate(pob_df$pd_actual, list(pob_df$POB), mean)
colnames(pob_pd_actual) <- c("POB","PD_Actual")

#Observations per bin
pob_in_graph <- as.data.frame(count(cre_dev_outsample, pob_bins))
# Mean PD of each bin
pob_in_graph$mean <- aggregate(cre_dev_outsample$p_hat, list(cre_dev_outsample$pob_bins), mean)[,2]

colnames(pob_in_graph) <- c("POB","Observations","Mean")

df <- merge(pob_in_graph, pob_pd_actual)

green <- rgb(.5, 1, .5, alpha=0.2)

pdf(paste("./R output", paste("Default Rate - ",colnames(df)[1],"_PROF.pdf", sep=""),sep ="/"), height = 5, width = 10)
par(mar = c(5,5,2,5))

par(new = T)
with(df, plot(as.numeric(row.names(df)), PD_Actual, col="blue", xaxt = "n",type = "l", lwd=2,
              ylab="", xlab="", ylim=c(0,max(apply(df[,3:4], 2, max)))))
par(new = T)
bp <- with(df, barplot(df$Observations,axes=F, xlab=NA, ylab=NA, col=green))
axis(side = 4)
mtext(side = 4, line = 3, 'Observation Count')
legend("topleft",
       legend=c("Average PD","Obs."),
       lty=1,lwd=5, col=c("blue", green))
axis(1, at=bp, labels=df[,1])
dev.off()



# End
# Plot outsample average PD by POB
##########

