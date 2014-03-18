source("http://www.r-statistics.com/wp-content/uploads/2010/02/Friedman-Test-with-Post-Hoc.r.txt")  # loading the friedman.test.with.post.hoc function from the internet

### Comparison of three Wine ("Wine A", "Wine B", and
###  "Wine C") for rounding first base. 
WineTasting <- data.frame(
  Taste = c(5.40, 5.50, 5.55,
            5.85, 5.70, 5.75,
            5.20, 5.60, 5.50,
            5.55, 5.50, 5.40,
            5.90, 5.85, 5.70,
            5.45, 5.55, 5.60,
            5.40, 5.40, 5.35,
            5.45, 5.50, 5.35,
            5.25, 5.15, 5.00,
            5.85, 5.80, 5.70,
            5.25, 5.20, 5.10,
            5.65, 5.55, 5.45,
            5.60, 5.35, 5.45,
            5.05, 5.00, 4.95,
            5.50, 5.50, 5.40,
            5.45, 5.55, 5.50,
            5.55, 5.55, 5.35,
            5.45, 5.50, 5.55,
            5.50, 5.45, 5.25,
            5.65, 5.60, 5.40,
            5.70, 5.65, 5.55,
            6.30, 6.30, 6.25),
  Wine = factor(rep(c("Wine A", "Wine B", "Wine C"), 22)),
  Taster = factor(rep(1:22, rep(3, 22))))
  #Taster = factor(c(rep(1:11,rep(3,11)),rep(1:11,rep(3,11)))))

with(WineTasting , boxplot( Taste  ~ Wine )) # boxploting 
friedman.test.with.post.hoc(Taste ~ Wine | Taster ,WineTasting)	# the same with our function. With post hoc, and cool plots
