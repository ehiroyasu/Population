# Panels for swamping effect

surv <- seq(0.9, 0.81, -0.01)
surv <- surv + rnorm(10,0,0.03)
surv <- c(0.9082105, 0.8583429, 0.8944336, 0.8801466, 0.9053542, 0.8395794, 0.8372672, 0.8354859, 0.7831534, 0.8101140)
surv0 <- surv+seq(0, 0.09, 0.01)
xx <- 2001:2010
summary(lm(surv0~I(1:10)))
plot(xx,surv)

library(ggplot2)
df <- data.frame(survival=surv-0.4, survival0=surv0-0.4, year=xx)
p1 <- qplot(year,survival,data = df)+ geom_point(size=5) + labs(x="YEAR", y="SURVIVAL") + scale_x_continuous(breaks = NULL) + scale_y_continuous(breaks = c(0.4, 0.5))
p1 <- p1 + geom_smooth(method=lm)+geom_line(linetype=2)
print(p1 <- p1+theme_classic(base_size = 24))

p10 <- qplot(year,survival0,data = df)+ geom_point(size=5) + labs(x="YEAR", y="SURVIVAL") + scale_x_continuous(breaks = NULL) #+ scale_y_continuous(breaks = c(0.45, 0.5, 0.55))
p10 <- p10 + geom_line(linetype=2)
print(p10 <- p10+theme_classic(base_size = 24))

fec <- rlnorm(10,-0.5,0.5)
fec <- c(0.6452093, 0.5482832, 0.4515529, 0.5521113, 0.3884673, 0.6521430, 0.5998850, 0.6675646, 0.7137941, 0.6178377)

fec2 <- mean(fec) + resid(lm(fec~xx))
df$fecundity <- fec2-0.05
p2 <- qplot(year,fecundity,data = df)+geom_line(linetype=2) + geom_point(size=5) + labs(x="YEAR", y="FECUNDITY") + scale_x_continuous(breaks = NULL)
print(p2 <- p2+theme_classic(base_size = 24))

df$lambda <- surv+fec2-0.45
p3 <- qplot(year,lambda,data = df) + geom_point(size=5) + labs(x="YEAR", y="GROWTH RATE") + scale_x_continuous(breaks = NULL)
p3 <- p3 + geom_smooth(method=lm)+geom_line(linetype=2)
print(p3 <- p3+theme_classic(base_size = 24))

df$lambda0 <- surv0+fec2-0.45
p30 <- qplot(year,lambda0,data = df) + geom_point(size=5) + labs(x="YEAR", y="GROWTH RATE") + scale_x_continuous(breaks = NULL)
p30 <- p30 + geom_line(linetype=2)
print(p30 <- p30+theme_classic(base_size = 24))

df$N <- 1000*cumprod(df$lambda)
p4<- qplot(year,log(N),data = df) + geom_point(size=5) + labs(x="YEAR", y="ABUNDANCE") + scale_x_continuous(breaks = NULL)
p4 <- p4 + geom_smooth(method=lm, formula=y~poly(x,2))+geom_line(linetype=2)
print(p4 <- p4+theme_classic(base_size = 24))

df$N0 <- 1000*cumprod(df$lambda0)
p40<- qplot(year,log(N0),data = df) + geom_point(size=5) + labs(x="YEAR", y="ABUNDANCE") + scale_x_continuous(breaks = NULL)
p40 <- p40 + geom_smooth(method=lm, formula=y~poly(x,2))+geom_line(linetype=2)
print(p40 <- p40+theme_classic(base_size = 24))

ggsave("p3.png", height=3, width=5)

ggsave("p1.png", p1, height = 1.5*3*diff(range(df$survival))/diff(range(df$lambda)), width=5)
ggsave("p2.png", p2, height = 1.2*3, width=5)

summary(lm(lambda~year, data=df))
summary(lm(survival~year, data=df))

# Panels for histograms

load("../Pop_dem_Output_data.Rdata")
library(ggplot2)
data1 <- output[[1]]

random <- data.frame(x=runif(10000))
lambda_pv <- data.frame(x=data1$lambda_pv)
surv_pv <- data.frame(x=data1$survival_pv)

h0 <- ggplot(random, aes(x=x)) + geom_histogram(aes(y=..count../sum(..count..)), fill="brown", binwidth=0.05, colour="black", linetype="dotted") + theme_classic(base_size = 24) +  ylab("FREQENCY") + xlab("P-VALUE") + geom_abline(intercept=0.05, slope=0, linetype="dashed") + ylim(0,0.06)
h1 <- ggplot(surv_pv, aes(x=x)) + ggtitle("Vital Rate Monitoring") + geom_histogram(aes(y=..count../sum(..count..)), fill="brown", binwidth=0.05, colour="black", linetype="dotted") + theme_classic(base_size = 24) +  ylab("FREQENCY") + xlab("P-VALUE") + ylim(0,0.475) + geom_abline(intercept=0.05, slope=0, linetype="dashed")
h2 <- ggplot(lambda_pv, aes(x=x)) + ggtitle("Abundance Monitoring") + geom_histogram(aes(y=..count../sum(..count..)), fill="brown", binwidth=0.05, colour="black", linetype="dotted")+ theme_classic(base_size = 24) +  ylab("FREQENCY") + xlab("P-VALUE") + geom_abline(intercept=0.05, slope=0, linetype="dashed")

ggsave("h0.png", h0, width=7,height=7)
ggsave("h1.png", h1, width=7,height=7)
ggsave("h2.png", h2, width=7,height=7)



#scatterplot for all species at a specific alpha value
library(DemographicTrend)
prop_demog_0.1<-matrix(data=NA)
prop_N_0.1<-matrix(data=NA)
prop_lambda_0.1<-matrix(data=NA)
names0.1<-matrix(data=NA)
for (i in 1:length(output)){
  prop_demog_0.1[i]<-as.numeric(pv_lessthan_alpha(pvalue=output[[i]]$survival_pv, alpha=0.1))
  prop_N_0.1[i]<-as.numeric(pv_lessthan_alpha(pvalue=output[[i]]$log_abundance_pv, alpha=0.1))
  prop_lambda_0.1[i]<-as.numeric(pv_lessthan_alpha(pvalue=output[[i]]$lambda_pv, alpha=0.1))
  names0.1[i]<-paste(output[[i]]$names, collapse="  ")
  prop_0.1<-data.frame(prop_demog_0.1, prop_N_0.1, prop_lambda_0.1, names0.1)
  prop_0.1<-prop_0.1[complete.cases(prop_0.1), ]
}


alpha0.1_plot<-qplot(prop_lambda_0.1, prop_demog_0.1, data=prop_0.1, size=3, colour="brown")
alpha0.1_plot<-alpha0.1_plot+geom_abline()+scale_size_identity(guide="none")+theme_classic(base_size = 24)+xlab("ABUNDANCE MONITORING")+
  ylab("VITAL RATE MONITORING")+theme(legend.position="none") + ggtitle("Power to detect trends")+xlim(0,1)+ylim(0,1)
print(alpha0.1_plot+aes(size=0))
ggsave("powerblank.png", alpha0.1_plot+aes(size=0), width=7,height=7)
ggsave("power.png", alpha0.1_plot, width=7,height=7)


# Thinking about early worning aspects
library(popbio)
