#code to look at package download stats

library(cranlogs)
library(dlstats)
library(forecast)
library(ggplot2)

packagename = 'DSAIRM'


##using cranlogs package
data1 = cranlogs::cran_downloads(package = packagename, from = "2017-02-01", to = Sys.Date( ))
print(sprintf('Number of downloads %d',sum(data1[,'count'])))
plot(data1$date,data1$count,type='l')

##using dlstats package
data2 <- dlstats::cran_stats(packagename)
print(sprintf('Number of downloads %d',sum(data2[,'downloads'])))
ggplot(data2, aes(end, downloads)) +  geom_line() + geom_point()
dlstats::plot_cran_stats(pkg = packagename)

#doing some more fancy forecasting, adapted from this source:
#http://peter.solymos.org/code/2016/08/23/trends-in-daily-r-package-downloads.html

plot_pkg_trend <- function(pkg)
  {
    op <- par(mar = c(3, 3, 1, 1) + 0.1, las = 1)
    on.exit(par(op))
    ## grab the data
    x <- cran_downloads(pkg, from = "2017-02-01", to = Sys.Date( ))
    x$date <- as.Date(x$date)
    x$std_days <- 1:nrow(x) / 365
    ## past trend
    m <- glm(count ~ std_days, x, family = poisson)
    tr_past <- round(100 * (exp(coef(m)[2L]) - 1), 2)
    ## future trend
    s <- ts(x$count)
    z <- ets(s, model = "AAN")
    f_days = 100 #number of days to forecast
    f <- forecast(z, h=f_days+1)
    f$date <- seq(Sys.Date( ), Sys.Date( ) + f_days, 1)
    tr_future <- round(100 * (f$mean[length(f$mean)] / f$mean[1L] - 1), 2)
    ## plot
    plot(count ~ date, x, type = "l", col = "darkgrey",
         ylab = "", xlab = "",
         ylim = c(0, quantile(x$count, 0.999)),
         xlim = c(x$date[1L], Sys.Date( ) + f_days))
    lines(lowess(x$date, x$count), col = 2, lwd = 2)
    polygon(c(f$date, rev(f$date)),
            c(f$upper[,2L], rev(f$lower[,2L])),
            border = NA, col = "lightblue")
    lines(f$date, f$mean, col = 4, lwd = 2)
    legend("topleft", title = paste("Package:", pkg), bty = "n",
           col = c(2, 4), lwd = 2, cex = 1,
           legend = c(paste0("past: ", tr_past, "%"),
                      paste0("future: ", tr_future, "%")))
    ## return the data
    invisible(x)
  }

plot_pkg_trend(packagename)
