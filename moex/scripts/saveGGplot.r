saveGGplot <- function(sGGplot,branch,name, width = 800, height = 600) {
        
        if (!file.exists(branch)) {
                dir.create(branch)
        }
        
        png(paste0(branch,name,".png"), width = width, height = height)
        print(sGGplot)
        dev.off()
        
}