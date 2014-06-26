require(reshape2)

tk <- read.csv('problems-logi-trimmed.csv')
positions = c('high-deltoid', 'low-deltoid', 'gap', 'midpoint-gap', 'midpoint', 'low-midpoint', 'below-midpoint')
problems = c("petechia", "discomfort", "ulnar.tingling", "tingling", "radial.tingling", "thumb.motor", "wrist.drop")
locations = c("ideal", "upper.high", "upper.low", "lower.high", "lower.low")
t = expand.grid(locations, problems)
all_problem_names <- paste(t$Var1, t$Var2, sep="_")

tkl <- melt(tk, measure.vars=all_problem_names)
tkl <- tkl[tkl$value==TRUE,]

t <- matrix(
  unlist(
    lapply(
      lapply(as.character(tkl$variable), strsplit, '_'),
      unlist)
    ),
  ncol=2,
  byrow=TRUE
)
tkl$location <- factor(t[,1])
tkl$problem <- factor(t[,2])
tkl$value <- NULL
tkl$variable <- NULL

#require(rCharts)
#nPlot(~ location, data=aggregate(. ~ location + problem, data=tkl, length), type='multiBarChart', group='problem')

percentify <- function(d) {
#   d$high <- round(100 * d$high / d$total)
#   d$low <- round(100 * d$low / d$total)
#   d$ideal <- round(100 * d$ideal / d$total)
#   d[d$total >= 5,]
  d
}
upper.radial <- percentify(aggregate(. ~ placement, data=data.frame(placement=tk[,"upper.placement"],                         
    high=apply(tk[,paste("upper.high", c("radial.tingling", "thumb.motor", "wrist.drop"), sep="_")],1,any),
    low=apply(tk[,paste("upper.low", c("radial.tingling", "thumb.motor", "wrist.drop"), sep="_")],1,any),
    ideal=apply(tk[,paste("ideal", c("radial.tingling", "thumb.motor", "wrist.drop"), sep="_")],1,any),
    total=c(TRUE)
  ), function(l) {length(which(l))}
))
lower.radial <- percentify(aggregate(. ~ placement, data=data.frame(placement=tk[,"lower.placement"],                         
    high=apply(tk[,paste("lower.high", c("radial.tingling", "thumb.motor", "wrist.drop"), sep="_")],1,any),
    low=apply(tk[,paste("lower.low", c("radial.tingling", "thumb.motor", "wrist.drop"), sep="_")],1,any),
    ideal=apply(tk[,paste("ideal", c("radial.tingling", "thumb.motor", "wrist.drop"), sep="_")],1,any),
    total=c(TRUE)
  ), function(l) {length(which(l))}
))
upper.ulnar <- percentify(aggregate(. ~ placement, data=data.frame(placement=tk[,"upper.placement"],                         
                                                                    high=tk[,paste("upper.high", c("ulnar.tingling"), sep="_")],
                                                                    low=tk[,paste("upper.low", c("ulnar.tingling"), sep="_")],
                                                                    ideal=tk[,paste("ideal", c("ulnar.tingling"), sep="_")],
                                                                    total=c(TRUE)
), function(l) {length(which(l))}
))
lower.ulnar <- percentify(aggregate(. ~ placement, data=data.frame(placement=tk[,"lower.placement"],                         
                                                                    high=tk[,paste("lower.high", c("ulnar.tingling"), sep="_")],
                                                                    low=tk[,paste("lower.low", c("ulnar.tingling"), sep="_")],
                                                                    ideal=tk[,paste("ideal", c("ulnar.tingling"), sep="_")],
                                                                    total=c(TRUE)
), function(l) {length(which(l))}
))
upper.other <- percentify(aggregate(. ~ placement, data=data.frame(placement=tk[,"upper.placement"],                         
                                                                    high=apply(tk[,paste("upper.high", c("tingling", "petechia", "discomfort"), sep="_")],1,any),
                                                                    low=apply(tk[,paste("upper.low", c("tingling", "petechia", "discomfort"), sep="_")],1,any),
                                                                    ideal=apply(tk[,paste("ideal", c("tingling", "petechia", "discomfort"), sep="_")],1,any),
                                                                    total=c(TRUE)
), function(l) {length(which(l))}
))
lower.other <- percentify(aggregate(. ~ placement, data=data.frame(placement=tk[,"lower.placement"],                         
                                                                    high=apply(tk[,paste("lower.high", c("tingling", "petechia", "discomfort"), sep="_")],1,any),
                                                                    low=apply(tk[,paste("lower.low", c("tingling", "petechia", "discomfort"), sep="_")],1,any),
                                                                    ideal=apply(tk[,paste("ideal", c("tingling", "petechia", "discomfort"), sep="_")],1,any),
                                                                    total=c(TRUE)
), function(l) {length(which(l))}
))

require(plotly)

uhigh <- list(name="Upper Wraps Too High",
              x=positions,
              y=unlist(lapply(positions, function(p) {upper.radial[upper.radial$placement==p,'high'][1]})),
              type="bar",
              bardir="h")
ulow <- list(name="Upper Wraps Too Low",
             x=positions,
             y=unlist(lapply(positions, function(p) {upper.radial[upper.radial$placement==p,'low'][1]})),
             type="bar",
             bardir="h")
lhigh <- list(name="Lower Wraps Too High",
              x=positions,
              y=unlist(lapply(positions, function(p) {lower.radial[lower.radial$placement==p,'high'][1]})),
              type="bar",
              bardir="h",
              xaxis="x2")
llow <- list(name="Lower Wraps Too Low",
             x=positions,
             y=unlist(lapply(positions, function(p) {lower.radial[lower.radial$placement==p,'low'][1]})),
             type="bar",
             bardir="h",
             xaxis="x2")
uplace <- list(name="Upper Wrap Ideal",
               y=positions,
               x=unlist(lapply(positions, function(p) {length(which(tk$upper.placement==p))})),
               type="scatter")
lplace <- list(name="Lower Wrap Ideal",
               y=positions,
               x=unlist(lapply(positions, function(p) {length(which(tk$lower.placement==p))})),
               type="scatter",
               xaxis="x2")

layout <- list(
  barmode='group',
  yaxis=list(autorange='reversed'),
  xaxis=list(domain=c(0,0.5), range=c(28,0)),
  xaxis2=list(domain=c(0.5,1), range=c(0,28), anchor='y'),
  categories=rev(positions),
  margin=list(l=120),
  title="Incidence of Radial Nerve Symptoms, by Preferred Placement"
  )

p$plotly(uhigh, ulow, lhigh, llow, uplace, lplace, kwargs=list(layout=layout, filename="placement-symptoms-radial", fileopt="overwrite"))

uhigh <- list(name="Upper Wraps Too High",
              x=positions,
              y=unlist(lapply(positions, function(p) {upper.ulnar[upper.ulnar$placement==p,'high'][1]})),
              type="bar",
              bardir="h")
ulow <- list(name="Upper Wraps Too Low",
             x=positions,
             y=unlist(lapply(positions, function(p) {upper.ulnar[upper.ulnar$placement==p,'low'][1]})),
             type="bar",
             bardir="h")
lhigh <- list(name="Lower Wraps Too High",
              x=positions,
              y=unlist(lapply(positions, function(p) {lower.ulnar[lower.ulnar$placement==p,'high'][1]})),
              type="bar",
              bardir="h",
              xaxis="x2")
llow <- list(name="Lower Wraps Too Low",
             x=positions,
             y=unlist(lapply(positions, function(p) {lower.ulnar[lower.ulnar$placement==p,'low'][1]})),
             type="bar",
             bardir="h",
             xaxis="x2")

layout <- list(
  barmode='group',
  yaxis=list(autorange='reversed'),
  xaxis=list(domain=c(0,0.5), range=c(28,0)),
  xaxis2=list(domain=c(0.5,1), range=c(0,28), anchor='y'),
  categories=rev(positions),
  margin=list(l=120),
  title="Incidence of Ulnar Nerve Symptoms, by Preferred Placement"
)

p$plotly(uhigh, ulow, lhigh, llow, uplace, lplace, kwargs=list(layout=layout, filename="placement-symptoms-ulnar", fileopt="overwrite"))

uhigh <- list(name="Upper Wraps Too High",
              x=positions,
              y=unlist(lapply(positions, function(p) {upper.other[upper.other$placement==p,'high'][1]})),
              type="bar",
              bardir="h")
ulow <- list(name="Upper Wraps Too Low",
             x=positions,
             y=unlist(lapply(positions, function(p) {upper.other[upper.other$placement==p,'low'][1]})),
             type="bar",
             bardir="h")
lhigh <- list(name="Lower Wraps Too High",
              x=positions,
              y=unlist(lapply(positions, function(p) {lower.other[lower.other$placement==p,'high'][1]})),
              type="bar",
              bardir="h",
              xaxis="x2")
llow <- list(name="Lower Wraps Too Low",
             x=positions,
             y=unlist(lapply(positions, function(p) {lower.other[lower.other$placement==p,'low'][1]})),
             type="bar",
             bardir="h",
             xaxis="x2")

layout <- list(
  barmode='group',
  yaxis=list(autorange='reversed'),
  xaxis=list(domain=c(0,0.5), range=c(28,0)),
  xaxis2=list(domain=c(0.5,1), range=c(0,28), anchor='y'),
  categories=rev(positions),
  margin=list(l=120),
  title="Incidence of Other/Ambiguous Symptoms, by Preferred Placement"
)

p$plotly(uhigh, ulow, lhigh, llow, uplace, lplace, kwargs=list(layout=layout, filename="placement-symptoms-other", fileopt="overwrite"))

prob_by_loc <- aggregate(X ~ location + problem, data=tkl, length)
prob_by_loc$location <- factor(prob_by_loc$location, levels=rev(locations)) # fix order
prob_by_loc <- dcast(prob_by_loc, location ~ problem, value.var='X')
prob_by_loc <- prob_by_loc[c(1,2,3,7,6,4,5,8)] # Fix order
prob_by_loc_m <- data.matrix(prob_by_loc[,2:8])
colnames(prob_by_loc_m) <- NULL

p$plotly(list(
    z = prob_by_loc_m,
    y = rev(locations), 
    x = colnames(prob_by_loc)[2:8],     
    scl = list(list(0,"rgb(255,255,255)"),list(0.5,"rgb(255,0,0)"),list(1,"rgb(128,0,0)")),
    type = "heatmap"
  ), kwargs=list(filename="problem-heatmap", fileopt="overwrite"))

