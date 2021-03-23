#!/usr/bin/env Rscript
require(gplots)

##################
## Extract data ##
##################
t1 <- read.delim('Olympics.tsv')

# Number of Games attended per athlete 
nb_part <- unlist(lapply(split(t1$Year,t1$Athlete_unique_url),function(x) length(unique(x))))

png('plotNumberParticipationsPerAthlete.png',width=6,height=6,unit='in',res=300)
par(mar=c(6,6,5,5))
x <- c(0,sort(unique(nb_part)))
plot(x,
     1-ecdf(nb_part)(x), 
     type='o',
     xlab='Number of participations per athlete',
     ylab='Cumulative distribution P(X>x)',
     main='Number of individual participations to the Olympics\nfollows an exponential distribution',
     pch=19,
     cex=1.3,
     cex.axis=1.3,
     cex.lab=1.3,
     cex.main=1.2,
     col='darkgray',
     log='y')
dev.off()

prop_won_before <- NULL
prop_new_medalists_no_participating_before <- NULL # those who never had medals before but have one now
prop_new_medalists <- NULL # those who never had medals before but have one now

for (tri in unique(t1$Trigram)){
    print(tri)
    inds <- which(t1$Trigram==tri)
    sp = split(t1[inds,],t1[inds,]$Year)

    if (length(sp) <2) next
    nb.medalists = lapply(sp,function(x) sum(x$Total>0))
    nb.participants = lapply(sp,nrow)
    y = unlist(nb.medalists)/unlist(nb.participants)

    years = as.numeric(sort(names(sp)))

    for (i in 2:length(years)){
        athletes <- sp[[i]]$Athlete_unique_url
        if (length(athletes) <10) next
        has_won_before <- rep(0, length(athletes))
        has_participated_before <- rep(0, length(athletes))
        for (j in 1:(i-1)){
            inds1 <- which(athletes %in% sp[[j]]$Athlete_unique_url)
            inds2 <- inds1[which(sp[[j]]$Total[match(athletes[inds1], sp[[j]]$Athlete_unique_url)]>0)]

            if (length(inds1)>0){
                has_participated_before[inds1] <- 1
                has_won_before[inds2] <- 1
            }
        }
        

        
        nb_new_medalists <- sum(!has_participated_before & sp[[i]]$Total > 0)
        nb_new_athletes <- sum(!has_participated_before)
        prop_new_medalists_no_participating_before <- c(prop_new_medalists_no_participating_before, nb_new_medalists / nb_new_athletes)
        
        prop_won_before <- c(prop_won_before, sum(has_won_before)/length(athletes))

    }

}


# logged values:
x <- log10(prop_won_before)
y <- log10(prop_new_medalists_no_participating_before)

# keep finite values
# the last condition gets rid of one outlier for nicer visualization
inds_keep <- which(is.finite(x) & !is.na(x) & is.finite(y) & !is.na(y) & x > -2.5)

xx <- x[inds_keep]
yy <- y[inds_keep]

# Binning in deciles for a nicer visualisation
sp_x <- split(xx, cut(xx, quantile(xx, probs = seq(0, 1, by = 0.1)))) 
sp_y <- split(yy, cut(xx, quantile(xx, probs = seq(0, 1, by = 0.1)))) 
xm <- unlist(lapply(sp_x,mean))
ym <- unlist(lapply(sp_y,mean))
xm_sd <- unlist(lapply(sp_x,function(x) sd(x)/sqrt(length(x)-1)))
ym_sd <- unlist(lapply(sp_y,function(x) sd(x)/sqrt(length(x)-1)))

# linear model for guiding the eye
fit <- lm(yy ~ xx)

# spearman correlation for a non-parametric estimate of the association
cc <- cor.test.0(xx,yy,method='s')

# final plot
png('plotPropNewMedalistsVsWonBefore.png', width=6,height=6,unit='in',res=300)
par(mar=c(6,6,5,5))
plot(xx,yy,
       xlab='Proportion of athletes who won in a previous competition (log10)',
       ylab='Proportion of new athletes earning a medal (log10)',
       main=paste0('Spearman\'s rho = ',signif(cc$estimate,2),', ',
                   'p = ',signif(cc$p.value,2)),
       xlim=c(min(xx),max(xx)),
       xaxt='n',
       yaxt='n',
       cex.main=1,
       cex=0.4,
       cex.lab=1.1,
       cex.axis=1.1,
       pch=19,
       col=rgb.0('gray',0.2)
       )
xticks <- seq(floor(min.0(xx)),ceiling(max.0(xx)))
axis(1, at=xticks,labels=10^xticks)
yticks <- seq(-3,0)
axis(2, at=yticks,labels=10^yticks)
plotCI(xm,
       ym,
       uiw=ym_sd,
       sfrac=0.005,
       lwd=1.,
       cex=0.3,
       pch=19,
       add=T,
       gap=0
       )
plotCI(xm,
       ym,
       uiw=xm_sd,
       err='x',
       sfrac=0.005,
       cex=0.3,
       pch=19,
       lwd=1.,
       add=T,
       gap=0
       )
abline(fit, lwd=0.7)
dev.off()
