


load("data/df_application.Rdata")

# Extract interpreted covariates, t2m and spi
X_I = X[, , , c(which(cov_names == "t2m"), which(cov_names == "spi"))]
X_N = X[, , , -c(which(cov_names == "t2m"), which(cov_names == "spi"))]




library(lattice)
lon_range = range(lonlat[, , 1], na.rm = T) + c(-0.5, 0.5)
lat_range = range(lonlat[, , 2], na.rm = T) + c(-0.5, 0.5)
lat_scale <- pretty(as.vector(lat_range))
lon_scale <- pretty(as.vector(lon_range))

plotmap     <- maps::map(
  'state',
  xlim = lon_range,
  ylim = lat_range,
  interior = T,
  plot = FALSE
)
mappanel <- function(x, y, ...) {
  # I think this is the fn that plots the map lines within the call to contour
  panel.contourplot(x, y, ...)
  llines(plotmap$x, plotmap$y, col = "black")
  #map.grid(plotmap)
}

ncols = 40
col1<-c('#ffffcc','#ffeda0','#fed976','#feb24c','#fd8d3c','#fc4e2a','#e31a1c','#bd0026','#800026')
col2 <- hcl.colors(ncols, palette = "Blue-Red 3")
col3<-c('#8c510a','#bf812d','#dfc27d','#f6e8c3','#f5f5f5','#c7eae5','#80cdc1','#35978f','#01665e')
col4<-c('#f7fcf5','#e5f5e0','#c7e9c0','#a1d99b','#74c476','#41ab5d','#238b45','#006d2c','#00441b')
col5<-c('#d73027','#f46d43','#fdae61','#fee090','#ffffbf','#e0f3f8','#abd9e9','#74add1','#4575b4')

contour = FALSE
region = TRUE
pretty = T


print("Creating plots of observed data")

#t_inds = 1:161
t_inds = t_inds=c(103,138,1,55,156,73)

for (t in t_inds) {
  pdf(
    file = paste0("img/obs_t", t, ".pdf"),
    height = 5,
    width = 8
  )
  temp = log(1 + (Y[t, , ]))
  temp[is.nan(temp)] = NA
  temp[temp == 0] = NA

  c <- 0
  c    <- lattice::contourplot( temp~  lonlat[,,1]*lonlat[,,2],
                       ylab="",xlab="", xlim=lon_range, ylim=lat_range,xaxt="n",
                       scales=list(tck=c(0,0),draw=F),colorkey=list(labels=list(cex=2)),
                       panel=mappanel, aspect=0.625,region=region,
                       main= list(cex=2,label=bquote(log(1+sqrt(Y)) : .(paste(times[t])))),
                       contour=contour, pretty=pretty,   col.regions=col1)
  print(c)
  dev.off()

  pdf(
    file = paste0("img/temperature_t", t, ".pdf"),
    height = 5,
    width = 8
  )
  temp = X_I[t, , , 1]


  c <- 0
  c    <- contourplot( temp~  lonlat[,,1]*lonlat[,,2],
                       ylab="",xlab="", xlim=lon_range, ylim=lat_range,xaxt="n",
                       scales=list(tck=c(0,0),draw=F),colorkey=list(labels=list(cex=2)),
                       panel=mappanel, aspect=0.625, region=region,
                       main= list(cex=2,label=bquote("2m air temp." : .(paste(times[t])))),
                       contour=contour, pretty=pretty,   col.regions=col2 )
  print(c)
  dev.off()

  pdf(
    file = paste0("img/SPI_t", t, ".pdf"),
    height = 5,
    width = 8
  )
  temp = X_I[t, , , 2]


  c <- 0
  c    <- contourplot( temp~  lonlat[,,1]*lonlat[,,2],
                       ylab="",xlab="", xlim=lon_range, ylim=lat_range,xaxt="n",
                       scales=list(tck=c(0,0),draw=F),colorkey=list(labels=list(cex=2)),
                       panel=mappanel, aspect=0.625, region=region,
                       main= list(cex=2,label=bquote("SPI" : .(paste(times[t])))),
                       contour=contour, pretty=T,   col.regions=col3, zlim=c(-max(abs(temp)),max(abs(temp))) )
  print(c)
  dev.off()


  pdf(
    file = paste0("img/grassland_t", t, ".pdf"),
    height = 5,
    width = 8
  )
  temp = X_N[t, , , 32]
  temp[temp == 0] = NA

  c <- 0
  c    <- contourplot( temp~  lonlat[,,1]*lonlat[,,2],
                       ylab="",xlab="", xlim=lon_range, ylim=lat_range,xaxt="n",
                       scales=list(tck=c(0,0),draw=F),colorkey=list(labels=list(cex=2)),
                       panel=mappanel, aspect=0.625, region=region,
                       main= list(cex=2,label=bquote("Grassland" : .(paste(times[t])))),

                       contour=contour, pretty=F,   col.regions=col4, zlim=c(0,1) )
  print(c)
  dev.off()

  print(times[t])
}


## P0 

print("Creating plots for estimates of p0")



load(paste0("data/df_application_statewise.Rdata"))

st = list.files("intermediates/models/p0_local_PINN_fit/")
st <- st[which(grepl("linearcoeffs", st))]


print(paste0("Uncertainty evaluated with ", length(st), " bootstrap samples"))
n.boot = length(st)

load(paste0("intermediates/models/p0_local_PINN_fit/",st[1]))

all_coef1=array(dim=c(nrow(linear_coeffs1),n.boot))
all_coef2=array(dim=c(nrow(linear_coeffs2),n.boot))


print("Loading SVC estimates")

for (it in 1:n.boot) {
  load(paste0("intermediates/models/p0_local_PINN_fit/", st[it]))
  
  all_coef1[, it ] = linear_coeffs1
  all_coef2[, it ] = linear_coeffs2
  
}


CI = apply(all_coef1,
           c(1),
           quantile,
           probs = c(0.025, 0.5, 0.975),
           na.rm = T)

zlim = range(CI)

temp = CI[2, ]
insig.inds = which (CI[3, ] > 0 & CI[1, ] < 0)

for (j in 1:dim(X_I_1)[4])
  temp[states.id == j] = temp[j]
temp[states.id == 0] = NA

library(latticeExtra)

coords2 = lonlat
for (j in unique(c(states.id))[-(insig.inds + 1)])
  coords2[, , 1][states.id == j] = NA
for (j in unique(c(states.id))[-(insig.inds + 1)])
  coords2[, , 2][states.id == j] = NA

pdf(
  file = paste0("img/p0_SVC_temp_median.pdf"),
  width = 8,
  height = 5
)

c <- 0
c    <- contourplot( temp~  lonlat[,,1]*lonlat[,,2],
                     ylab="",xlab="", xlim=lon_range, ylim=lat_range,xaxt="n",
                     scales=list(tck=c(0,0),cex.title=4,draw=F),colorkey=list(labels=list(cex=2)),
                     panel=mappanel, aspect=0.625, region=region, main= list(label=expression(paste(p[0], ": 2m temperature (K)")),cex=2),
                     contour=contour, pretty=pretty,  
                     at=seq(-max(abs(zlim),na.rm=T),max(abs(zlim),na.rm=T),length=ncols-1),
                     col.regions=col2 ) +
  layer(lpoints(
    coords2[, , 1],
    coords2[, , 2],
    pch = "`",
    cex = 1,
    col = "black"
  ))
print(c)
dev.off()

temp = CI[3, ] - CI[1, ]
for (j in 1:dim(X_I_1)[4])
  temp[states.id == j] = temp[j]
temp[states.id == 0] = NA
pdf(
  file = paste0("img/p0_SVC_temp_IQR.pdf"),
  width = 8,
  height = 5
)


c <- 0
c    <- contourplot( temp~  lonlat[,,1]*lonlat[,,2],
                     ylab="",xlab="", xlim=lon_range, ylim=lat_range,xaxt="n",
                     scales=list(tck=c(0,0),draw=F),colorkey=list(labels=list(cex=2)),
                     panel=mappanel, aspect=0.625, region=region, main= list(cex=2,label=expression(paste(p[0], ": 2m temperature (K)"))),
                     contour=contour, pretty=F, col.regions=col4 )
print(c)
dev.off()


CI = apply(all_coef2,
           c(1),
           quantile,
           probs = c(0.025, 0.5, 0.975),
           na.rm = T)

zlim = range(CI)

temp = CI[2, ]
insig.inds = which (CI[3, ] > 0 & CI[1, ] < 0)
for (j in 1:dim(X_I_2)[4])
  temp[states.id == j] = temp[j]
temp[states.id == 0] = NA
pdf(
  file = paste0("img/p0_SVC_SPI_median.pdf"),
  width = 8,
  height = 5
)

coords2 = lonlat
for (j in unique(c(states.id))[-(insig.inds + 1)])
  coords2[, , 1][states.id == j] = NA
for (j in unique(c(states.id))[-(insig.inds + 1)])
  coords2[, , 2][states.id == j] = NA


c <- 0
c    <- contourplot( temp~  lonlat[,,1]*lonlat[,,2],
                     ylab="",xlab="", xlim=lon_range, ylim=lat_range,xaxt="n",
                     scales=list(tck=c(0,0),draw=F),colorkey=list(labels=list(cex=2)),
                     panel=mappanel, aspect=0.625, region=region, main= list(cex=2,label=expression(paste(p[0], ": 3-month SPI"))),
                     contour=contour, pretty=pretty,  
                     at=seq(-max(abs(zlim),na.rm=T),max(abs(zlim),na.rm=T),length=ncols-1), col.regions=col2 )+
  layer(lpoints(
    coords2[, , 1],
    coords2[, , 2],
    pch = "`",
    cex = 1,
    col = "black"
  ))
print(c)
dev.off()


temp = CI[3, ] - CI[1, ]
for (j in 1:dim(X_I_2)[4])
  temp[states.id == j] = temp[j]
temp[states.id == 0] = NA
pdf(
  file = paste0("img/p0_SVC_SPI_IQR.pdf"),
  width = 8,
  height = 5
)

c <- 0
c    <- contourplot( temp~  lonlat[,,1]*lonlat[,,2],
                     ylab="",xlab="", xlim=lon_range, ylim=lat_range,xaxt="n", 
                     
                     scales=list(tck=c(0,0),draw=F),colorkey=list(labels=list(cex=2)),
                     panel=mappanel, aspect=0.625, region=region, 
                     main= list(cex=2,label=expression(paste(p[0], ": 3-month SPI"))),
                     contour=contour, pretty=F,  
                     col.regions=col4 )
print(c)
dev.off()
print("Created p0 SVC estimate plots")





 print("Creating functional boxplots of global p0-PINN estimates")
 
 st = list.files("intermediates/models/p0_global_PINN_fit/")
 st <- st[which(grepl("additivecoeffs", st))]
 
 
 
 load(paste0("intermediates/models/p0_global_PINN_fit/", st[1]))
 
 
 all_gam = array(dim = c(dim(gam_weights), n.boot))
 
 for (it in 1:n.boot) {
   load(paste0("intermediates/models/p0_global_PINN_fit/", st[it]))
   
   all_gam[, , it] = gam_weights
   
   
 }
 
 
 n.knot = 10 # number of knots.
 X_I_basis  <- array(dim = c(dim(X_I), n.knot))
 
 
 temp = c()
 knots = matrix(nrow = dim(X_I)[4], ncol = n.knot)
 for (i in 1:dim(X_I)[4]) {
   #Get knots? Just equally spaced quantiles
   temp = X_I[, , , i]
   knots[i, ] = quantile(temp, probs = seq(0, 1, length = n.knot)) #equally spaced quantiles
 }
 
 # basis function
 rad = function(x, c) {
   out = abs(x - c) ^ 2 * log(abs(x - c))
   out[(x - c) == 0] = 0
   return(out)
 }
 
 
 bases_min <- bases_range <- matrix(nrow = dim(X_I)[4], ncol =
                                      n.knot)
 for (i in 1:dim(X_I)[4]) {
   for (k in 1:n.knot) {
     X_I_basis[, , , i, k] = rad(x = X_I[, , , i], c =
                                   knots[i, k])
     #Scale radial bases to aid training
     
     temp = X_I_basis[, , , i, k]
     bases_min[i, k] = mean(temp)
     bases_range[i, k] = sd(temp)
     
     X_I_basis[, , , i, k] = (temp - bases_min[i, k]) / bases_range[i, k]
     
     
   }
 }
 
 
 
 for (i in 1:dim(knots)[1]) {
   if (i == 1)
     plt.x = seq(from = min(knots[i, ]),
                 to = max(knots[i, ]),
                 length = 1000)
   else
     plt.x = seq(from = min(knots[i, -1]),
                 to = max(knots[i, ]),
                 length = 1000)
   temp = matrix(nrow = length(plt.x), ncol = n.knot)
   for (j in 1:n.knot) {
     temp[, j] = rad(plt.x, knots[i, j])
     temp[, j] = (temp[, j] - bases_min[i, j]) / bases_range[i, j]
     
   }
   
   t0 = matrix(nrow = 1, ncol = n.knot)
   for (j in 1:n.knot) {
     t0[, j] = rad(median(c(X_I[, , , i])), knots[i, j])
     t0[, j] = (t0[, j] - bases_min[i, j]) / bases_range[i, j]
   }
   
   
   plt.y = c()
   for (it in 1:(n.boot)) {
     tmp = temp %*% as.matrix(all_gam[i, , it])
     # tmp=tmp-tmp[length(tmp)/2]
     
     
     y.zero = as.numeric(t0 %*% all_gam[i, , it])
     
     plt.y = cbind(plt.y, tmp - y.zero)
     
     
     
     
   }
   if (i == 1)
     title = expression(paste(p[0], ": 2m temperature (K)"))
   else if (i == 2)
     title = expression(paste(p[0], ": 3-month SPI"))
   pdf(
     file = paste0("img/fbp_cov", i, "_p0.pdf"),
     width = 6,
     height = 4
   )
   fD =  plt.y
   if (sum(is.na(plt.y[1, ])) > 0)
     fD = plt.y[, -which(is.na(plt.y[1, ]))]
   
   
   fda::fbplot(
     fit = fD,
     main = title,
     xlab = 'x',
     ylab = '' ,
     x = plt.x,
     xlim = range(plt.x),
     ylim = range(plt.y, na.rm = T),
     cex.axis = 1.6,
     cex.lab = 1.6,
     cex.main = 1.6
   )
   mtext(expression(m[I](x)), 2, cex = 1.6, line = 2.3)
   
   abline(h = 0, col = "red")
   points(knots[i, ],
          rep(min(plt.y, na.rm = T), n.knot),
          col = "red",
          pch = 2)
   dev.off()
   
 }
 
 
 
 print("Created functional boxplots")




st = list.files("intermediates/predictions/p0_local_PINN_fit/")
load(paste0("intermediates/predictions/p0_local_PINN_fit/", st[1]))
all_p0 = array(dim = c(dim(pred_p0), n.boot))

print("Loading p0 predictions")

for (it in 1:n.boot) {
  load(paste0("intermediates/predictions/p0_local_PINN_fit/", st[it]))
  all_p0[, , , , it] = pred_p0
  
}



print("Creating p0 prediction maps")

for (t in t_inds) {
  qmed = apply(all_p0[t, , , 1, ], c(1, 2), median, na.rm = T)
  
  pdf(
    file = paste0("img/p0_median_t", t, ".pdf"),
    width = 8,
    height = 5
  )
  temp = qmed
  temp[Y[t, , ] < 0] = NA
  
  date <- times[t]
  c <- 0
  c    <- contourplot( temp~  lonlat[,,1]*lonlat[,,2],
                       ylab="",xlab="", xlim=lon_range, ylim=lat_range,xaxt="n",
                       scales=list(tck=c(0,0),draw=F),colorkey=list(labels=list(cex=2)),
                       panel=mappanel, aspect=0.625, region=region, 
                       main= list(cex=2,label=bquote(p[0] : .(paste(times[t])))),
                       contour=contour, pretty=F,   col.regions=col1 ,zlim=c(0,1))
  print(c)
  dev.off()
  
  IQR = apply(all_p0[t, , , 1, ], c(1, 2), IQR, na.rm = T)
  
  pdf(
    file = paste0("img/p0_IQR_t", t, ".pdf"),
    width = 8,
    height = 5
  )
  temp = IQR
  temp[Y[t, , ] < 0] = NA
  c <- 0
  c    <- contourplot( temp~  lonlat[,,1]*lonlat[,,2],
                       ylab="",xlab="", xlim=lon_range, ylim=lat_range,xaxt="n",
                       scales=list(tck=c(0,0),draw=F),colorkey=list(labels=list(cex=2)),
                       panel=mappanel, aspect=0.625, region=region, 
                       main= list(cex=2,label=bquote(p[0] : .(paste(times[t])))),
                       
                       contour=contour, pretty=F,   col.regions=col4, zlim=c(0,1) )
  print(c)
  dev.off()
  
  
  print(times[t])
}


print("Creating p0 temporal trend maps - Takes some time")



months = c("",
           "",
           "March",
           "April",
           "May",
           "June",
           "July",
           "August",
           "September")
 for (m in 3:9) {
   temp = all_p0[seq(m - 2, m - 2 + 22 * 7, by = 7), , , 1, ]
   lm.coeff = array(NA, dim = dim(temp)[-1])
   for (k in 1:dim(temp)[4]) {
     if (!is.na(c(temp[1, 1, 1, k]))) {
       for (i in 1:ncol(temp)) {
         for (j in 1:dim(temp)[3]) {
           fit <- lm(y ~ t, data = data.frame("y" = c(temp[, i, j, k]), "t" = 1:23))
           lm.coeff[i, j, k] = fit$coefficients[2]
         }
       }
     }
   }
   
   qmed = apply(lm.coeff, c(1, 2), median, na.rm = T)
   ql = apply(lm.coeff,
              c(1, 2),
              quantile,
              prob = 0.025,
              na.rm = T)
   qu = apply(lm.coeff,
              c(1, 2),
              quantile,
              prob = 0.975,
              na.rm = T)
   
   qmed[Y[m, , ] < 0] = NA
   qmed[qu > 0 & ql < 0] = NA
   
 
   pdf(
     file = paste0("img/p0_diff_m", m, ".pdf"),
     width = 8,
     height = 5
   )
   
   c <- 0
   c    <- contourplot(
     qmed ~  lonlat[, , 1] * lonlat[, , 2],
     ylab = "",
     xlab = "",
     xlim = lon_range,
     ylim = lat_range,
     xaxt = "n",
     scales = list(tck = c(0, 0), draw = F),
     colorkey = list(labels = list(cex = 2)),
     panel = mappanel,
     aspect = 0.625,
     region = region,
     main = list(cex = 2, label = bquote(p[0]:.(paste(
       months[m]
     )))),
     
     contour = contour,
     pretty = F,
     col.regions = (col2) ,
     at = seq(-max(abs(qmed), na.rm = T), max(abs(qmed), na.rm =
                                                T), length = 11)
   )
   print(c)
   dev.off()
   
   pdf(
     file = paste0("img/p0_diff_m", m, "_IQR.pdf"),
     width = 8,
     height = 5
   )
   qmed = apply(lm.coeff, c(1, 2), IQR, na.rm = T)
   
   
   qmed[Y[m, , ] < 0] = NA
   c <- 0
   c    <- contourplot(
     qmed ~  lonlat[, , 1] * lonlat[, , 2],
     ylab = "",
     xlab = "",
     xlim = lon_range,
     ylim = lat_range,
     xaxt = "n",
     scales = list(tck = c(0, 0), draw = F),
     colorkey = list(labels = list(cex = 2)),
     panel = mappanel,
     aspect = 0.625,
     region = region,
     main = list(cex = 2, label = bquote(p[0]:.(paste(
       months[m]
     )))),
     
     contour = contour,
     pretty = F,
     col.regions = (col4)
   )
   print(c)
   dev.off()
   
   print(paste0(months[m]," finished"))
 }






## bGEV

print("Creating plots for estimates of bGEV models")


st = list.files("intermediates/models/bGEV_local_PINN_fit/")
st <- st[which(grepl("linearcoeffs", st))]


print(paste0("Uncertainty evaluated with ", length(st), " bootstrap samples"))
n.boot = length(st)

load(paste0("intermediates/models/bGEV_local_PINN_fit/", st[1]))


print("Loading SVC estimates")

all_q_coef1 <- all_q_coef2 <- array(dim = c(nrow(linear_coeffs_loc1), n.boot))
all_s_coef1 <- all_s_coef2 <- array(dim = c(nrow(linear_coeffs_spread1), n.boot))


for (it in 1:n.boot) {
  load(paste0("intermediates/models/bGEV_local_PINN_fit/", st[it]))
  
  all_q_coef1[, it] = linear_coeffs_loc1
  all_s_coef1[, it] = linear_coeffs_spread1
  all_q_coef2[, it] = linear_coeffs_loc2
  all_s_coef2[, it] = linear_coeffs_spread2
  
}



CI = apply(all_q_coef1,
           c(1),
           quantile,
           probs = c(0.025, 0.5, 0.975),
           na.rm = T)

zlim = range(CI)

temp = CI[2, ]
insig.inds = which (CI[3, ] > 0 & CI[1, ] < 0)
coords2 = lonlat
for (j in unique(c(states.id))[-(insig.inds + 1)])
  coords2[, , 1][states.id == j] = NA
for (j in unique(c(states.id))[-(insig.inds + 1)])
  coords2[, , 2][states.id == j] = NA

for (j in 1:dim(X_I_1)[4])
  temp[states.id == j] = temp[j]
temp[states.id == 0] = NA

c <- 0
pdf(
  file = paste0("img/bGEV_SVC_temp_loc_median.pdf"),
  width = 8,
  height = 5
)

c    <- contourplot( temp~  lonlat[,,1]*lonlat[,,2],
                     ylab="",xlab="", xlim=lon_range, ylim=lat_range,xaxt="n",
                     scales=list(tck=c(0,0),cex.title=4,draw=F),colorkey=list(labels=list(cex=2)),
                     panel=mappanel, aspect=0.625, region=region, main=list(label=expression(paste(q[alpha], ": 2m temperature (K)")),cex=2),
                     contour=contour, pretty=pretty,  
                     at=seq(-max(abs(zlim),na.rm=T),max(abs(zlim),na.rm=T),length=ncols-1), 
                     col.regions=col2 ) +
  layer(lpoints(
    coords2[, , 1],
    coords2[, , 2],
    pch = "`",
    cex = 1,
    col = "black"
  ))
print(c)
dev.off()


temp = CI[3, ] - CI[1, ]
for (j in 1:dim(X_I_1)[4])
  temp[states.id == j] = temp[j]
temp[states.id == 0] = NA

c <- 0
pdf(
  file = paste0("img/bGEV_SVC_temp_loc_IQR.pdf"),
  width = 8,
  height = 5
)

c    <- contourplot( temp~  lonlat[,,1]*lonlat[,,2],
                     ylab="",xlab="", xlim=lon_range, ylim=lat_range,xaxt="n",
                     scales=list(tck=c(0,0),cex.title=4,draw=F),colorkey=list(labels=list(cex=2)),
                     panel=mappanel, aspect=0.625, region=region, main=list(label=expression(paste(q[alpha], ": 2m temperature (K)")),cex=2),
                     contour=contour, pretty=F, col.regions=col4 )
print(c)
dev.off()


CI = apply(all_s_coef1,
           c(1),
           quantile,
           probs = c(0.025, 0.5, 0.975),
           na.rm = T)

zlim = range(CI)

temp = CI[2, ]
for (j in 1:dim(X_I_1)[4])
  temp[states.id == j] = temp[j]
temp[states.id == 0] = NA

insig.inds = which (CI[3, ] > 0 & CI[1, ] < 0)
coords2 = lonlat
for (j in unique(c(states.id))[-(insig.inds + 1)])
  coords2[, , 1][states.id == j] = NA
for (j in unique(c(states.id))[-(insig.inds + 1)])
  coords2[, , 2][states.id == j] = NA

pdf(
  file = paste0("img/bGEV_SVC_temp_spread_median.pdf"),
  width = 8,
  height = 5
)

c <- 0
c    <- contourplot( temp~  lonlat[,,1]*lonlat[,,2],
                     ylab="",xlab="", xlim=lon_range, ylim=lat_range,xaxt="n",
                     scales=list(tck=c(0,0),cex.title=4,draw=F),colorkey=list(labels=list(cex=2)),
                     panel=mappanel, aspect=0.625, region=region, main=list(label= expression(paste(s[beta], ": 2m temperature (K)")),cex=2),
                     contour=contour, pretty=pretty,  
                     at=seq(-max(abs(zlim),na.rm=T),max(abs(zlim),na.rm=T),length=ncols-1), 
                     col.regions=col2 ) +
  layer(lpoints(
    coords2[, , 1],
    coords2[, , 2],
    pch = "`",
    cex = 1,
    col = "black"
  ))
print(c)
dev.off()



temp = CI[3, ] - CI[1, ]
for (j in 1:dim(X_I_1)[4])
  temp[states.id == j] = temp[j]
temp[states.id == 0] = NA

c <- 0
pdf(
  file = paste0("img/bGEV_SVC_temp_spread_IQR.pdf"),
  width = 8,
  height = 5
)

c    <- contourplot( temp~  lonlat[,,1]*lonlat[,,2],
                     ylab="",xlab="", xlim=lon_range, ylim=lat_range,xaxt="n",
                     scales=list(tck=c(0,0),cex.title=4,draw=F),colorkey=list(labels=list(cex=2)),
                     panel=mappanel, aspect=0.625, region=region, main=list(label=expression(paste(s[beta], ": 2m temperature (K)")),cex=2),
                     contour=contour, pretty=F, col.regions=col4 )
print(c)
dev.off()

CI = apply(all_q_coef2,
           c(1),
           quantile,
           probs = c(0.025, 0.5, 0.975),
           na.rm = T)

zlim = range(CI)

temp = CI[2, ]
for (j in 1:dim(X_I_2)[4])
  temp[states.id == j] = temp[j]
temp[states.id == 0] = NA

insig.inds = which (CI[3, ] > 0 & CI[1, ] < 0)
coords2 = lonlat
for (j in unique(c(states.id))[-(insig.inds + 1)])
  coords2[, , 1][states.id == j] = NA
for (j in unique(c(states.id))[-(insig.inds + 1)])
  coords2[, , 2][states.id == j] = NA


c <- 0
pdf(
  file = paste0("img/bGEV_SVC_SPI_loc_median.pdf"),
  width = 8,
  height = 5
)

c    <- contourplot( temp~  lonlat[,,1]*lonlat[,,2],
                     ylab="",xlab="", xlim=lon_range, ylim=lat_range,xaxt="n",
                     scales=list(tck=c(0,0),cex.title=4,draw=F),colorkey=list(labels=list(cex=2)),
                     panel=mappanel, aspect=0.625, region=region, main=  list(label=expression(paste(q[alpha], ": 3-month SPI")),cex=2),
                     contour=contour, pretty=pretty,  
                     at=seq(-max(abs(zlim),na.rm=T),max(abs(zlim),na.rm=T),length=ncols-1),
                     col.regions=col2 ) +
  layer(lpoints(
    coords2[, , 1],
    coords2[, , 2],
    pch = "`",
    cex = 1,
    col = "black"
  ))
print(c)
dev.off()


temp = CI[3, ] - CI[1, ]
for (j in 1:dim(X_I_1)[4])
  temp[states.id == j] = temp[j]
temp[states.id == 0] = NA

c <- 0
pdf(
  file = paste0("img/bGEV_SVC_SPI_loc_IQR.pdf"),
  width = 8,
  height = 5
)

c    <- contourplot( temp~  lonlat[,,1]*lonlat[,,2],
                     ylab="",xlab="", xlim=lon_range, ylim=lat_range,xaxt="n",
                     scales=list(tck=c(0,0),cex.title=4,draw=F),colorkey=list(labels=list(cex=2)),
                     panel=mappanel, aspect=0.625, region=region, main=list(label=expression(paste(q[alpha], ": 2m temperature (K)")),cex=2),
                     contour=contour, pretty=F, col.regions=col4 )
print(c)
dev.off()

CI = apply(all_s_coef2,
           c(1),
           quantile,
           probs = c(0.025, 0.5, 0.975),
           na.rm = T)

zlim = range(CI)

temp = CI[2, ]
for (j in 1:dim(X_I_2)[4])
  temp[states.id == j] = temp[j]
temp[states.id == 0] = NA

insig.inds = which (CI[3, ] > 0 & CI[1, ] < 0)
coords2 = lonlat
for (j in unique(c(states.id))[-(insig.inds + 1)])
  coords2[, , 1][states.id == j] = NA
for (j in unique(c(states.id))[-(insig.inds + 1)])
  coords2[, , 2][states.id == j] = NA

pdf(
  file = paste0("img/bGEV_SVC_SPI_spread_median.pdf"),
  width = 8,
  height = 5
)

c <- 0
c    <- contourplot( temp~  lonlat[,,1]*lonlat[,,2],
                     ylab="",xlab="", xlim=lon_range, ylim=lat_range,xaxt="n",
                     scales=list(tck=c(0,0),cex.title=4,draw=F),colorkey=list(labels=list(cex=2)),
                     panel=mappanel, aspect=0.625, region=region, main=  list(label=expression(paste(s[beta], ": 3-month SPI")),cex=2),
                     contour=contour, pretty=pretty,  
                     at=seq(-max(abs(zlim),na.rm=T),max(abs(zlim),na.rm=T),length=ncols-1),
                     col.regions=col2 )+
  layer(lpoints(
    coords2[, , 1],
    coords2[, , 2],
    pch = "`",
    cex = 1,
    col = "black"
  ))
print(c)
dev.off()
temp = CI[3, ] - CI[1, ]
for (j in 1:dim(X_I_1)[4])
  temp[states.id == j] = temp[j]
temp[states.id == 0] = NA

c <- 0
pdf(
  file = paste0("img/bGEV_SVC_SPI_spread_IQR.pdf"),
  width = 8,
  height = 5
)

c    <- contourplot( temp~  lonlat[,,1]*lonlat[,,2],
                     ylab="",xlab="", xlim=lon_range, ylim=lat_range,xaxt="n",
                     scales=list(tck=c(0,0),cex.title=4,draw=F),colorkey=list(labels=list(cex=2)),
                     panel=mappanel, aspect=0.625, region=region, main=list(label=expression(paste(s[beta], ": 2m temperature (K)")),cex=2),
                     contour=contour, pretty=F, col.regions=col4 )
print(c)
dev.off()


print("Created bGEV SVC estimate plots")





 print("Creating functional boxplots of global bGEV-PINN estimates")
 
 st = list.files("intermediates/models/bGEV_global_PINN_fit/")
 st <- st[which(grepl("additivecoeffs", st))]
 
 
 load(paste0("intermediates/models/bGEV_global_PINN_fit/", st[1]))
 
 
 all_gam_q = array(dim = c(dim(gam_weights_q), n.boot))
 all_gam_s = array(dim = c(dim(gam_weights_s), n.boot))
 
 
 for (it in 1:n.boot) {
   load(paste0("intermediates/models/bGEV_global_PINN_fit/", st[it]))
   
   all_gam_q[, , it] = gam_weights_q
   all_gam_s[, , it] = gam_weights_s
   
 }
 
 
 
 for (i in 1:dim(knots)[1]) {
   if (i == 1)
     plt.x = seq(from = min(knots[i, ]),
                 to = max(knots[i, ]),
                 length = 1000)
   else
     plt.x = seq(from = min(knots[i, -1]),
                 to = max(knots[i, ]),
                 length = 1000)
   temp = matrix(nrow = length(plt.x), ncol = n.knot)
   for (j in 1:n.knot) {
     temp[, j] = rad(plt.x, knots[i, j])
     temp[, j] = (temp[, j] - bases_min[i, j]) / bases_range[i, j]
     
   }
   
   t0 = matrix(nrow = 1, ncol = n.knot)
   for (j in 1:n.knot) {
     t0[, j] = rad(median(c(X_I[, , , i])), knots[i, j])
     t0[, j] = (t0[, j] - bases_min[i, j]) / bases_range[i, j]
   }
   
   
   plt.y = c()
   for (it in 1:(n.boot)) {
     tmp = temp %*% as.matrix(all_gam_q[i, , it])
     # tmp=tmp-tmp[length(tmp)/2]
     
     
     y.zero = as.numeric(t0 %*% all_gam_q[i, , it])
     
     plt.y = cbind(plt.y, tmp - y.zero)
     
     
     
     
   }
   if (i == 1)
     title = expression(paste(q[alpha], ": 2m temperature (K)"))
   else if (i == 2)
     title = expression(paste(q[alpha], ": 3-month SPI"))
   pdf(
     file = paste0("img/fbp_cov", i, "_q.pdf"),
     width = 6,
     height = 4
   )
   fD =  plt.y
   if (sum(is.na(plt.y[1, ])) > 0)
     fD = plt.y[, -which(is.na(plt.y[1, ]))]
   
   
   fda::fbplot(
     fit = fD,
     main = title,
     xlab = 'x',
     ylab = '' ,
     x = plt.x,
     xlim = range(plt.x),
     ylim = range(plt.y, na.rm = T),
     cex.axis = 1.6,
     cex.lab = 1.6,
     cex.main = 1.6
   )
   mtext(expression(m[I](x)), 2, cex = 1.6, line = 2.3)
   
   abline(h = 0, col = "red")
   points(knots[i, ],
          rep(min(plt.y, na.rm = T), n.knot),
          col = "red",
          pch = 2)
   dev.off()
   
 }
 
 
 
 for (i in 1:dim(knots)[1]) {
   if (i == 1)
     plt.x = seq(from = min(knots[i, ]),
                 to = max(knots[i, ]),
                 length = 1000)
   else
     plt.x = seq(from = min(knots[i, -1]),
                 to = max(knots[i, ]),
                 length = 1000)
   temp = matrix(nrow = length(plt.x), ncol = n.knot)
   for (j in 1:n.knot) {
     temp[, j] = rad(plt.x, knots[i, j])
     temp[, j] = (temp[, j] - bases_min[i, j]) / bases_range[i, j]
     
   }
   
   t0 = matrix(nrow = 1, ncol = n.knot)
   for (j in 1:n.knot) {
     t0[, j] = rad(median(c(X_I[, , , i])), knots[i, j])
     t0[, j] = (t0[, j] - bases_min[i, j]) / bases_range[i, j]
   }
   
   
   plt.y = c()
   for (it in 1:(n.boot)) {
     tmp = temp %*% as.matrix(all_gam_s[i, , it])
     # tmp=tmp-tmp[length(tmp)/2]
     
     
     y.zero = as.numeric(t0 %*% all_gam_s[i, , it])
     
     plt.y = cbind(plt.y, tmp - y.zero)
     
     
     
     
   }
   if (i == 1)
     title = expression(paste(s[beta], ": 2m temperature (K)"))
   else if (i == 2)
     title = expression(paste(s[beta], ": 3-month SPI"))
   pdf(
     file = paste0("img/fbp_cov", i, "_s.pdf"),
     width = 6,
     height = 4
   )
   fD =  plt.y
   if (sum(is.na(plt.y[1, ])) > 0)
     fD = plt.y[, -which(is.na(plt.y[1, ]))]
   
   
   fda::fbplot(
     fit = fD,
     main = title,
     xlab = 'x',
     ylab = '' ,
     x = plt.x,
     xlim = range(plt.x),
     ylim = range(plt.y, na.rm = T),
     cex.axis = 1.6,
     cex.lab = 1.6,
     cex.main = 1.6
   )
   mtext(expression(m[I](x)), 2, cex = 1.6, line = 2.3)
   
   abline(h = 0, col = "red")
   points(knots[i, ],
          rep(min(plt.y, na.rm = T), n.knot),
          col = "red",
          pch = 2)
   dev.off()
   
 }
 
 
 
 print("Created functional boxplots")






st = list.files("intermediates/predictions/bGEV_local_PINN_fit/")
load(paste0("intermediates/predictions/bGEV_local_PINN_fit/", st[1]))
all_preds = array(dim = c(dim(pred_bGEV), n.boot))

print("Loading bGEV predictions")

for (it in 1:n.boot) {
  load(paste0("intermediates/predictions/bGEV_local_PINN_fit/", st[it]))
  all_preds[, , , , it] = pred_bGEV
  
}



print("Creating bGEV prediction maps")

source("src/bGEV_loss.R")

print("Evaluating 90% quantile - Takes some time")

quants = array(dim = c(dim(Y), n.boot))
for (t in 1:nrow(quants)) {
  for (i in 1:ncol(quants)) {
    for (j in 1:dim(quants)[3]) {
      quants[t, i, j, ] = apply(cbind(all_preds[t, i, j, 2, ], all_preds[t, i, j, 3, ], all_preds[t, i, j, 4, ]), c(1), function(x) {
        (
          qbGEV(
            0.9 ^ 12,
            q_a = x[1],
            s_b = x[2],
            xi = x[3],
            alpha = 0.5,
            beta = 0.5,
            p_a = 0.05,
            p_b = 0.2,
            c1 = 5,
            c2 = 5
          )
        )
        
      })
    }
  }
  
}


#Make xi maps

qmed = apply(all_preds[1, , , 4, ], c(1, 2), median, na.rm = T)

pdf(
  file = paste0("img/bGEV_xi_median.pdf"),
  width = 8,
  height = 5
)
temp = qmed
temp[Y[1, , ] < 0] = NA
c <- 0
c    <- contourplot( temp~  lonlat[,,1]*lonlat[,,2],
                     ylab="",xlab="", xlim=lon_range, ylim=lat_range,xaxt="n",
                     scales=list(tck=c(0,0),cex.title=4,draw=F),colorkey=list(labels=list(cex=2)),
                     panel=mappanel, aspect=0.625, region=region,   main= list(cex=2,label=bquote(xi )),
                     contour=contour, pretty=T,   col.regions=col1 )
print(c)
dev.off()

qmed = apply(all_preds[1, , , 4, ], c(1, 2), IQR, na.rm = T)

pdf(
  file = paste0("img/bGEV_xi_IQR.pdf"),
  width = 8,
  height = 5
)
temp = qmed
temp[Y[1, , ] < 0] = NA
c <- 0
c    <- contourplot( temp~  lonlat[,,1]*lonlat[,,2],
                     ylab="",xlab="", xlim=lon_range, ylim=lat_range,xaxt="n",
                     scales=list(tck=c(0,0),cex.title=4,draw=F),colorkey=list(labels=list(cex=2)),
                     panel=mappanel, aspect=0.625, region=region,main= list(cex=2,label=bquote(xi )),
                     contour=contour, pretty=F,   col.regions=col4 )
print(c)
dev.off()

for (t in t_inds) {
  qmed = apply(all_preds[t, , , 2, ], c(1, 2), median, na.rm = T)
  
  pdf(
    file = paste0("img/bGEV_loc_median_t", t, ".pdf"),
    width = 8,
    height = 5
  )
  temp = qmed
  temp[Y[t, , ] < 0] = NA
  c <- 0
  c    <- contourplot( temp~  lonlat[,,1]*lonlat[,,2],
                       ylab="",xlab="", xlim=lon_range, ylim=lat_range,xaxt="n",
                       scales=list(tck=c(0,0),cex.title=4,draw=F),colorkey=list(labels=list(cex=2)),
                       panel=mappanel, aspect=0.625, region=region,                      
                       main= list(cex=2,label=bquote(q[alpha] : .(paste(times[t])))),
                       
                       contour = contour, pretty = F, col.regions = col1 )
  print(c)
  dev.off()
  
  qmed = apply(all_preds[t, , , 2, ], c(1, 2), IQR, na.rm = T)
  
  pdf(
    file = paste0("img/bGEV_loc_IQR_t", t, ".pdf"),
    width = 8,
    height = 5
  )
  temp = qmed
  temp[Y[t, , ] < 0] = NA
  c <- 0
  c    <- contourplot( temp~  lonlat[,,1]*lonlat[,,2],
                       ylab="",xlab="", xlim=lon_range, ylim=lat_range,xaxt="n",
                       scales=list(tck=c(0,0),cex.title=4,draw=F),colorkey=list(labels=list(cex=2)),
                       panel=mappanel, aspect=0.625, region=region,                        main= list(cex=2,label=bquote(q[alpha] : .(paste(times[t])))),
                       
                       contour=contour, pretty=F,   col.regions=col4 )
  print(c)
  dev.off()
  qmed = apply(all_preds[t, , , 3, ], c(1, 2), median, na.rm = T)
  
  pdf(
    file = paste0("img/bGEV_spread_median_t", t, ".pdf"),
    width = 8,
    height = 5
  )
  temp = log(1 + qmed)
  temp[Y[t, , ] < 0] = NA
  c <- 0
  c    <- contourplot( temp~  lonlat[,,1]*lonlat[,,2],
                       ylab="",xlab="", xlim=lon_range, ylim=lat_range,xaxt="n",
                       scales=list(tck=c(0,0),cex.title=4,draw=F),colorkey=list(labels=list(cex=2)),
                       panel=mappanel, aspect=0.625, region=region,                        main= list(cex=2,label=bquote(s[beta] : .(paste(times[t])))),
                       
                       contour=contour, pretty=F,   col.regions=col1 )
  print(c)
  dev.off()
  qmed = apply(all_preds[t, , , 3, ], c(1, 2), IQR, na.rm = T)
  
  pdf(
    file = paste0("img/bGEV_spread_IQR_t", t, ".pdf"),
    width = 8,
    height = 5
  )
  temp = log(1 + qmed)
  temp[Y[t, , ] < 0] = NA
  c <- 0
  c    <- contourplot( temp~  lonlat[,,1]*lonlat[,,2],
                       ylab="",xlab="", xlim=lon_range, ylim=lat_range,xaxt="n",
                       scales=list(tck=c(0,0),cex.title=4,draw=F),colorkey=list(labels=list(cex=2)),
                       panel=mappanel, aspect=0.625, region=region,   main= list(cex=2,label=bquote(s[beta] : .(paste(times[t])))),
                       contour=contour, pretty=F,   col.regions=col4 )
  print(c)
  dev.off()
  
  
  qmed = apply(quants[t, , , ], c(1, 2), median, na.rm = T)
  
  pdf(
    file = paste0("img/bGEV_90q_med_t", t, ".pdf"),
    width = 8,
    height = 5
  )
  temp = log(1 + qmed)
  temp[Y[t, , ] < 0] = NA
  c <- 0
  c    <- contourplot( temp~  lonlat[,,1]*lonlat[,,2],
                       ylab="",xlab="", xlim=lon_range, ylim=lat_range,xaxt="n",
                       scales=list(tck=c(0,0),cex.title=4,draw=F),colorkey=list(labels=list(cex=2)),
                       panel=mappanel, aspect=0.625, region=region,  main= list(cex=2,label=bquote(paste("90% Quantile") :.(paste(times[t])))),
                       contour=contour, pretty=F,   col.regions=col1 )
  print(c)
  dev.off()
  qmed = apply(quants[t, , , ], c(1, 2), IQR, na.rm = T)
  
  pdf(
    file = paste0("img/bGEV_90q_IQR_t", t, ".pdf"),
    width = 8,
    height = 5
  )
  temp = log(1 + qmed)
  temp[Y[t, , ] < 0] = NA
  c <- 0
  c    <- contourplot( temp~  lonlat[,,1]*lonlat[,,2],
                       ylab="",xlab="", xlim=lon_range, ylim=lat_range,xaxt="n",
                       scales=list(tck=c(0,0),cex.title=4,draw=F),colorkey=list(labels=list(cex=2)),
                       panel=mappanel, aspect=0.625, region=region,  main= list(cex=2,label=bquote(paste("90% Quantile") :.(paste(times[t])))),
                       contour=contour, pretty=F,   col.regions=col4 )
  print(c)
  dev.off()
  
  
  print(times[t])
}


# Making pooled QQ plot

print("Creating pooled QQ plot")

#Transform to exp margins



dat = c(Y[Y > 0])
all_exp = array(dim = c(length(dat), n.boot))
for (it in 1:n.boot) {
  pred_tall = matrix(ncol = 3, nrow = length(dat))
  for (i in 1:3)
    pred_tall[, i] = c(all_preds[, , , i + 1, it][Y > 0])
  
  if (!is.na(pred_tall[1])) {
    all_exp[, it] = apply(cbind(dat, pred_tall), 1, function(x) {
      (
        pbGEV(
          x[1],
          q_a = x[2],
          s_b = x[3],
          xi = x[4],
          alpha = 0.5,
          beta = 0.5,
          p_a = 0.05,
          p_b = 0.2,
          c1 = 5,
          c2 = 5
        )
      ) ^ {
        1 / 12
      }
      
    })
  }
  
  
}

all_exp = qexp(all_exp)

p_min = 0.925
n_p = length(dat) * (1 - p_min)
ps = p_min + (1:n_p) / (n_p + 1) * (1 - p_min)
qs = array(dim = c(n.boot, length(ps)))
for (i in 1:n.boot) {
  qs[i, ] = quantile(all_exp[, i], ps, na.rm = T)
  
}
qs_med = apply(qs, 2, median, na.rm = T)
pdf(
  file = paste0("img/bGEV_exp.pdf"),
  width = 8,
  height = 7
)
qupper = apply(qs, 2, quantile, prob = 0.975, na.rm = T)
qlower = apply(qs, 2, quantile, prob = 0.025, na.rm = T)
plot(
  qexp(ps),
  qs_med,
  ylab = "Fitted",
  xlab = "Empirical",
  main = "",
  pch = 20,
  asp = 1,
  ylim = range(qs_med, qupper)
)
abline(a = 0, b = 1, col = "red")

points(
  qupper,
  x = qexp(ps),
  type = "l",
  lty = 2,
  col = "blue",
  lwd = 2
)
points(
  qlower,
  x = qexp(ps),
  type = "l",
  lty = 2,
  col = "blue",
  lwd = 2
)

dev.off()
print("Created pooled QQ plot")

print("Creating bGEV 90% quantile temporal trend maps - Takes some time")



for (m in 3:9) {
  temp = quants[seq(m - 2, m - 2 + 22 * 7, by = 7), , , ]
  lm.coeff = array(NA, dim = dim(temp)[-1])
  for (k in 1:dim(temp)[4]) {
    if (!is.na(c(temp[1, 1, 1, k]))) {
      for (i in 1:ncol(temp)) {
        for (j in 1:dim(temp)[3]) {
          fit <- lm(y ~ t, data = data.frame("y" = c(temp[, i, j, k]), "t" = 1:23))
          lm.coeff[i, j, k] = fit$coefficients[2]
        }
      }
    }
  }
  
  qmed = apply(lm.coeff, c(1, 2), median, na.rm = T)
  ql = apply(lm.coeff,
             c(1, 2),
             quantile,
             prob = 0.025,
             na.rm = T)
  qu = apply(lm.coeff,
             c(1, 2),
             quantile,
             prob = 0.975,
             na.rm = T)
  
  qmed[Y[m, , ] < 0] = NA
  qmed[qu > 0 & ql < 0] = NA
  
  pdf(
    file = paste0("img/q90Map_diff_m", m, ".pdf"),
    width = 8,
    height = 5
  )
  
  c <- 0
  c    <- contourplot( qmed~  lonlat[,,1]*lonlat[,,2],
                       ylab="",xlab="", xlim=lon_range, ylim=lat_range,xaxt="n",
                       scales=list(tck=c(0,0),cex.title=4,draw=F),colorkey=list(labels=list(cex=2)),
                       panel=mappanel, aspect=0.625, region=region,  main= list(cex=2,label=bquote(paste("90% Quantile") :.(paste(months[m])))),
                       contour=contour, pretty=F,   col.regions=(col2) ,
                       at =seq(-max(abs(qmed),na.rm=T), max(abs(qmed),na.rm=T), length=11))
  print(c)
  dev.off()
  
  pdf(
    file = paste0("img/q90Map_diff_m", m, "_IQR.pdf"),
    width = 8,
    height = 5
  )
  qmed = apply(lm.coeff, c(1, 2), IQR, na.rm = T)
  
  
  qmed[Y[m, , ] < 0] = NA
  c <- 0
  c    <- contourplot( qmed~  lonlat[,,1]*lonlat[,,2],
                       ylab="",xlab="", xlim=lon_range, ylim=lat_range,xaxt="n",
                       scales=list(tck=c(0,0),cex.title=4,draw=F),colorkey=list(labels=list(cex=2)),
                       panel=mappanel, aspect=0.625, region=region,  main= list(cex=2,label=bquote(paste("90% Quantile") :.(paste(months[m])))),
                       contour=contour, pretty=F,   col.regions=(col4) )
  print(c)
  dev.off()
  
  print(paste0(months[m]," finished"))

}


print("Creating unconditional quantile plots - Also takes some time...")


for(p in c(0.9,0.95,0.99)){
  scaled_p = (p - (1 - all_p0)) / all_p0
  scaled_p[scaled_p <= 0] = 0
  
  quants = array(dim = c(dim(Y)[2:3], n.boot))
  
  for (t in t_inds) {
    for (i in 1:nrow(quants)) {
      for (j in 1:ncol(quants)) {
        quants[i, j, ] = apply(cbind(scaled_p[t, i, j, 1, ], all_preds[t, i, j, 2, ], all_preds[t, i, j, 3, ], all_preds[t, i, j, 4, ]), c(1), function(x) {
          if (is.na(x[1]) |
              is.na(x[2])) {
            return(NA)
          } else if (x[1] == 0) {
            return(0)
          } else{
            (
              qbGEV(
                x[1] ^ 12,
                q_a = x[2],
                s_b = x[3],
                xi = x[4],
                alpha = 0.5,
                beta = 0.5,
                p_a = 0.05,
                p_b = 0.2,
                c1 = 5,
                c2 = 5
              )
            )
          }
        })
        
        
      }
      
    }
    
    quants[quants < 0] = 0
    
    qmed = apply(quants, c(1, 2), quantile, prob = 0.5, na.rm = T)
    
    pdf(
      file = paste0("img/cr_", p * 1000, "_med_t", t, ".pdf"),
      width = 7,
      height = 5
    )
    temp = log(1 + qmed)
    temp[Y[t, , ] < 0] = NA
    temp[temp == 0] = NA
    c <- 0
    c    <- contourplot( temp~  lonlat[,,1]*lonlat[,,2],
                         ylab="",xlab="", xlim=lon_range, ylim=lat_range,xaxt="n",
                         scales=list(tck=c(0,0),cex.title=4,draw=F),colorkey=list(labels=list(cex=2)),
                         panel=mappanel, aspect=0.625, region=region,
                         main= list(cex=2,label=bquote(.(paste0(p*100,"% Quantile")) :.(paste(times[t])))),
                         contour=contour, pretty=F,   col.regions=col1 )
    print(c)
    dev.off()
    
    qmed = apply(quants, c(1, 2), IQR, na.rm = T)
    
    pdf(
      file = paste0("img/cr_", p * 1000, "_IQR_t", t, ".pdf"),
      width = 7,
      height = 5
    )
    temp = log(1 + qmed)
    temp[Y[t, , ] < 0] = NA
    c <- 0
    c    <- contourplot( temp~  lonlat[,,1]*lonlat[,,2],
                         ylab="",xlab="", xlim=lon_range, ylim=lat_range,xaxt="n",
                         scales=list(tck=c(0,0),cex.title=4,draw=F),colorkey=list(labels=list(cex=2)),
                         panel=mappanel, aspect=0.625, region=region,  main= list(cex=2,label=bquote(.(paste0(p*100,"% Quantile")) :.(paste(times[t])))),
                         contour=contour, pretty=F,   col.regions=col4 )
    print(c)
    dev.off()
  }
}