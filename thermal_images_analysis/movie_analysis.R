library(dplyr)
library(ggplot2)
library(tidyverse)
library(scales)
library(ggpubr)
library(gridExtra)

### functions that reads each file from a directory, finds the maximum temperature and corresponding position of it
### saves in a dataframe with frame number, maximum temp, x and y position



therma_ana <- function(data_directory, thresh, every_nth_frame=1, rowlim1=138,rowlim2=200,collim1=13,collim2=104){
  
  frame_seq <- seq(1,length(list.files(data_directory)),every_nth_frame)
  
  path_file_names <- paste(data_directory,"/", "Data_",seq(1,length(list.files(data_directory)),every_nth_frame),".csv", sep="") 
 
  
  frame = integer()
  temp_max =numeric()
  temp_min =numeric()
  time_sec = numeric() 
  max_t_area_list=list()
  thresh_t_area_list = list()
 

          for(i in 1:length(frame_seq)){
                temp_data <- read_csv(path_file_names[i], col_names = c("row", paste("C_", seq(1,321), sep="")), skip=2)
                temp_data <- temp_data[2:241,1:321]
                long_temp_data<-gather(temp_data, key= "column", value="temp", C_1:C_320) %>% separate(column, into = c("col_text", "col_num"), sep = "([\\_])") %>%
                mutate(row=as.numeric(row),col_num=as.numeric(col_num))%>% filter(row%in%c(rowlim1:rowlim2)) %>% filter(col_num%in%c(collim1:collim2))
                  
                max_t <- max(long_temp_data$temp,na.rm=TRUE)
                max_t_area = filter(long_temp_data, temp>=max_t)
               # max_t_pixel = c(mean(max_t_area$row), mean(max_t_area$col_num))
                
                min_t <- min(long_temp_data$temp,na.rm=TRUE)
                min_t_area = filter(long_temp_data, temp<=min_t)
               # min_t_pixel = c(mean(min_t_area$row), mean(min_t_area$col_num))
                
                thresh_t_area = filter(long_temp_data, temp < max_t & temp > thresh)
                
                frame[i] = frame_seq[i]
                temp_max[i]= max_t
                temp_min [i] = min_t
                time_sec[i]= frame_seq[i]*0.11
                max_t_area_list[[i]]=   max_t_area 
                thresh_t_area_list[[i]]=  thresh_t_area 
                }
      
      tmax_pos = data.frame(frame, time_sec, temp_max,  temp_min)
      output = list(tmax_pos, max_t_area_list, thresh_t_area_list)
      return(output)
      
}


therma_frame_plot <- function(data_list,frame_length_out,breaks_plot= seq(28,35, by=0.3),x_lim,y_lim,save_file_path,col_plot=3)  {
  
  
  plot_frame =as.integer(seq(1,length(data_list), length.out = frame_length_out))
  plot_list = list()
  
    for (i in 1:frame_length_out) {
        p = ggplot(thresh_frame[[plot_frame[i]]], aes(x=col_num, y=row, colour=temp)) + geom_point(shape=15,size=2)+
                ggtitle(paste("frame at ", round(plot_frame[i]*0.11*(20/60),digits=1), " min" , sep= " ")) + scale_color_gradient(high="darkred", low="green", breaks=breaks_plot) + coord_fixed(ratio=1,xlim=x_lim , ylim=y_lim) + ylab("rows")+ xlab("columns")
        plot_list[[i]] = p
      }
    ggsave(save_file_path, arrangeGrob(grobs = plot_list, ncol = col_plot))
    
    
}

therma_temp_time_plot  <- function(data,breaks_temp= seq(28,35, by=0.3),break_time = seq(0,600, by=50), x_lim= c(0,600), y_lim =c(28,35), save_file_path )  {
  
p <- ggplot(data, aes(x=time_sec)) + geom_line(aes(y=temp_max), colour="red") +  ggtitle("Temperature distrubution in ALPIDE") 

P1 <- p + geom_line(aes(y=temp_min), color= "green")+ scale_y_continuous(name = "Maximum_temp(C)", breaks= breaks_temp, limits= y_lim) + scale_x_continuous(name = "Time(sec)", breaks = break_time, limits= x_lim)

ggsave(save_file_path)

}



  

  

### analysis of IR_088 movie

  data_088_20 <- therma_ana("\\\\eir.uib.no/home7/sme028/PCT/Thermal_Images_movie/270819_88/IR_088",28.3,20)
  
  
  
  therma_plot(data_list=data_088_20,frame_length_out=8, breaks = seq(28,35, by=0.3), x_lim= c(10,120), y_lim=c(120,220),
              save_file_path = "\\\\eir.uib.no/home7/sme028/PCT/Thermal_Images_movie/IR_287_charge_injection_test_1.pdf", col_plot=2)
  
  therma_temp_time_plot(data=data_287_5[[1]],breaks_temp = seq(28,35, by=0.3),break_time =100, y_lim =c(28,35), save_file_path="\\\\eir.uib.no/home7/sme028/PCT/Thermal_Images_movie/IR_287_time_temp_test_2.pdf" ) 
  
  
  
  
  data_088_20_tmax_pos <-  data_088_20[[1]]
 
   thresh_frame <- data_just_active_every_20[[3]]
  plot_frame =as.integer(seq(1,length(thresh_frame), length.out = 6))
 
    plot_1 <- ggplot(thresh_frame[[plot_frame[1]]], aes(x=col_num, y=row, colour=temp)) + geom_point(shape=15,size=2) + ggtitle(paste("frame at ", round(plot_frame[1]*0.11*(20/60), digits=1), " min" , sep= " ")) + scale_color_gradient(high="darkred", low="green", breaks=seq(28,35, by=0.3))+ coord_fixed(ratio=1,xlim=c(10,120),ylim=c(120,220))+ylab("rows")+xlab("columns")
    plot_2 <-  ggplot(thresh_frame[[plot_frame[2]]], aes(x=col_num, y=row, colour=temp)) + geom_point(shape=15,size=2) + ggtitle(paste("frame at ", round(plot_frame[2]*0.11*(20/60), digits=1), " min" , sep= " ")) + scale_color_gradient(high="darkred", low="green", breaks=seq(28,35, by=0.3))+ coord_fixed(ratio=1,xlim=c(10,120),ylim=c(120,220))+ylab("rows")+xlab("columns")
    plot_3 <-  ggplot(thresh_frame[[plot_frame[3]]], aes(x=col_num, y=row, colour=temp)) + geom_point(shape=15,size=2) + ggtitle(paste("frame at ", round(plot_frame[3]*0.11*(20/60), digits=1), "  min" , sep= " ")) + scale_color_gradient(high="darkred", low="green", breaks=seq(28,35, by=0.3))+ coord_fixed(ratio=1,xlim=c(10,120),ylim=c(120,220))+ylab("rows")+xlab("columns")
    plot_4 <-  ggplot(thresh_frame[[plot_frame[4]]], aes(x=col_num, y=row, colour=temp)) + geom_point(shape=15,size=2) + ggtitle(paste("frame at ", round(plot_frame[4]*0.11*(20/60), digits=1), "  min" , sep= " ")) + scale_color_gradient(high="darkred", low="green", breaks=seq(28,35, by=0.3))+ coord_fixed(ratio=1,xlim=c(10,120),ylim=c(120,220))+ylab("rows")+xlab("columns")
    plot_5 <- ggplot(thresh_frame[[plot_frame[5]]], aes(x=col_num, y=row, colour=temp)) + geom_point(shape=15,size=2) + ggtitle(paste("frame at ", round(plot_frame[5]*0.11*(20/60), digits=1), "  min" , sep= " ")) + scale_color_gradient(high="darkred", low="green", breaks=seq(28,35, by=0.3))+ coord_fixed(ratio=1,xlim=c(10,120),ylim=c(120,220))+ylab("rows")+xlab("columns")
    plot_6 <- ggplot(thresh_frame[[plot_frame[6]]], aes(x=col_num, y=row, colour=temp)) + geom_point(shape=15,size=2) + ggtitle(paste("frame at ", round(plot_frame[6]*0.11*(20/60), digits=1), "  min" , sep= " ")) + scale_color_gradient(high="darkred", low="green", breaks=seq(28,35, by=0.3))+ coord_fixed(ratio=1,xlim=c(10,120),ylim=c(120,220))+ylab("rows")+xlab("columns")
    
    final_plot <- ggarrange(plot_1, plot_2,plot_3, plot_4, plot_5, plot_6, ncol=3,nrow = 2)
 
    ggsave("IR_88_EVERY_20_FRAMES_6.pdf", final_plot, device="pdf", path="\\\\eir.uib.no/home7/sme028/PCT/Thermal_Images_movie")
  
  
  time_min_max_temp_88 <- ggplot(data_just_active_every_20_tmax_pos, aes(x=time_sec)) + geom_line(aes(y=temp_max), colour="red") +  ggtitle("Temperature distrubution in ALPIDE") 
  
  time_min_max_temp_88 <- time_min_max_temp_88 + geom_line(aes(y=temp_min), color= "green")+ scale_y_continuous(name = "Maximum_temp(C)", breaks=seq(27,33,0.25), limits=c(27,33)) + scale_x_continuous(name = "Time(sec)", breaks =seq(0,1850,200), limits=c(0,1850))

  ggsave("IR_088_EVERY_20.pdf", time_min_max_temp_88, device="pdf", path="\\\\eir.uib.no/home7/sme028/PCT/Thermal_Images_movie")
  
 
  
  
########## Analysis of IR_287.IR3 IMAGES ...... CHECK FOR TEMPERATURE SPIKES.
  
  data_287 <- therma_ana("\\\\eir.uib.no/home7/sme028/PCT/Thermal_Images_movie/IR_0287",28.3, 1, 168,205,170,232)
 
  therma_plot(data_list=data_287,frame_length_out=8, breaks = seq(28,35, by=0.3), x_lim= c(160,240), y_lim=c(160,210),
              save_file_path = "\\\\eir.uib.no/home7/sme028/PCT/Thermal_Images_movie/IR_287_charge_injection_test_1.pdf", col_plot=2)
  
  therma_temp_time_plot(data=data_287_5[[1]],breaks_temp = seq(28,35, by=0.3),break_time = seq(0,400, by=50), y_lim =c(28,35), x_lim=c(0,400),save_file_path="\\\\eir.uib.no/home7/sme028/PCT/Thermal_Images_movie/IR_287_time_temp_test_5.pdf" ) 
 