#' Preprocessing for further analysis
#'
#' @param seg segmentation data
#' @return preprocessed data
preprocess.hrd<-function(seg){
  # Output file that can be used in non-seqz run in the future  # 
  outputdir = getwd()
  run_name <- unique(seg$SampleID)
  out_file <- paste0(outputdir,"/",run_name,"_preprocessed_hrd.txt")
  
  seg <- seg[!seg[,2] %in% c(paste('chr',c('X','Y','x','y',23,24),sep=''),c('X','Y','x','y',23,24)),]
  seg[,1] <- as.character(seg[,1])

  if(! all(seg[,8] <= seg[,7]) ){
    tmp <- seg
    seg[tmp[,8] > tmp[,7],7]  <- tmp[tmp[,8] > tmp[,7],8]
    seg[tmp[,8] > tmp[,7],8]  <- tmp[tmp[,8] > tmp[,7],7]
  }
  seg <- shrink.seg.ai.wrapper(seg)
  write.table(seg,file = out_file, sep="\t",quote = FALSE, row.names = FALSE)
  
  return(seg)
}
