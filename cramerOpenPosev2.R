cramerOpenPosev2 <- function(df,set.NAs, type.point = "pose_keypoints", fixed.point.x = 1, fixed.point.y = 1, v.i=5, save.parquet, save.by.video=TRUE,path.to.save, save.csv=FALSE) {
  
  
  ##### transform 0 values to NAs values ###############
  
  if ( set.NAs == TRUE ) {
    
    df$x[ df$x == 0 ] <- NA
    
    df$y[ df$y == 0 ] <- NA
  }

  
  # set automatic fix parameters ######  
  
  total_frames<- ( nrow( df ) / 137 ) -1 ### number of frames in data frame :P
  
  total_matrix = NULL  
  
  
  ############## the process frame by frame ##############
  
  for ( frame_number in 0:total_frames ) {
    
    first_element<- 137 * frame_number + 1 
    # frame number is refered inside the data set 
    # and it is independent of the number frame in the video
    
    frame_df<- ( df[ first_element:( first_element + 136 ) , ] )
    
    new_x_values<- frame_df$x - frame_df[ frame_df$point == fixed.point.x & frame_df$typePoint == type.point , "x" ]
    new_y_values<- ( frame_df$y - frame_df[ frame_df$point == fixed.point.y & frame_df$typePoint == type.point , "y" ]) * (-1)
    
    
    ##### preapring cramer #########
    
    m<- matrix( cbind( new_x_values , new_y_values ) , ncol = 2 ) # matrix
    i<- m[ v.i + 1 , ]
    j<- i[ 2:1 ] # orthonormal
    
    
    ##### here is cramer #######
    
    newm = NULL
    a<- matrix( data = c( i,j ) , nrow = 2 )
    
    for ( k in 1:nrow( m ) ) {
      
      b<- as.matrix( c( m[ k , ] ) , ncol = 1 )
      
      newx<- ( a[ 2 , 2 ] * b[ 1 ] - b[ 2 ] * a[ 1 , 2 ] ) / ( a[ 1 , 1 ] * a[ 2 , 2 ] - a[ 2 , 1 ] * a[ 2 , 1 ] )
      newy<- ( a[ 1 , 1 ] * b[ 2 ] - b[ 1 ] * a[ 2 , 1 ] ) / ( a[ 1 , 1 ] * a[ 2 , 2 ] - a[ 2 , 1 ] * a[ 2 , 1 ] )
      
      newPoint<- c( newx , newy )
      newm<- rbind( newm , newPoint )
    }
    
    total_matrix<- rbind( total_matrix , newm )
  }
    
    
  ##### join all #######
  colnames( total_matrix )<-c( "nx" , "ny" )
  row.names( total_matrix )<- NULL
  df<- cbind( df[ 1:3 ] , total_matrix , df[ 4:length( df ) ] )
  
  ## wirte files ######
    
if ( save.parquet == TRUE & save.by.video == TRUE ) {
  
  require( arrow )
  
  file_names<- unique( df$name )
  
  for ( f in 1:length( file_names ) ) {
    
    bit_df<- df[ df$name == file_names[ f ] , ]
  
  write_parquet( x = bit_df , paste( path.to.save , "/" , file_name[f] , ".parquet" , sep = "" ) )
  
  } 
} 
  
  
  if ( save.parquet == TRUE & save.by.video == FALSE ) {
    
    require( arrow )
    
    write_parquet( x = df , paste(path.to.save , "/" ,
                                  format(Sys.time() ) , "_" , 
                                  system( "whoami" , intern = TRUE ) ,
                                  ".parquet" , sep = "" ) )
    
  } 
  
  
  if ( save.csv == TRUE & save.by.video == TRUE ) {
    

    file_names<- unique( df$name )
    
    for ( f in 1:length( file_names ) ) {
      
      bit_df<- df[ df$name == file_names[ f ] , ]
      
      write.csv( x = bit_df, paste(path.to.save, "/" , file_names[f] , ".csv" , sep = "" ) , row.names = FALSE )
      
    }
  }  
  
  
  
  if ( save.csv == TRUE & save.by.video == FALSE ) {
    
    

    print( "Please write it as regular csv file, write.csv(filename) " )
    
  }  
    
return( df )    
  
}

df<-read.csv("/home/agora/daedalusDataBank/openPoseData/rawData/tonight/2016-01-01_0735_US_KCBS_Late_Show_with_Stephen_Colbert_1733.53-1737.98_tonight.csv")


new_df<-cramerOpenPosev2(df = df, set.NAs = TRUE,save.parquet = FALSE,path.to.save = "data/",save.by.video = TRUE,save.csv = FALSE)


save(cramerOpenPosev2,file = "/home/agora/functions/cramerOpenPoseV2.rda")

load(file = "/home/agora/functions/cramerOpenPoseV2.rda")
