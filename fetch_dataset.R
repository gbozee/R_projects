require(EIAdata)
require(Quandl)
update_csv<-function(){
  key<-("95365B2462BFD45A96D4EB0DBC60E59A")
  key2<-("5DzySRnssU4_uNysxVU6")
  eia_d_oil<-getEIA("PET.RBRTE.D",key)
  eia_w_oil<-getEIA("PET.RBRTE.W",key)
  eia_m_oil<-getEIA("PET.RBRTE.M",key)
  brent_d_oil<-Quandl("FRED/DCOILBRENTEU", api_key=key2, start_date="1987-05-20",order=c("asc"))
  brent_w_oil<-Quandl("FRED/WCOILBRENTEU", api_key=key2, start_date="1987-05-15",order=c("asc"))
  brent_m_oil<-Quandl("FRED/MCOILBRENTEU", api_key=key2, start_date="1987-05-01",order=c("asc"))
  wt_d_oil<-Quandl("FRED/DCOILWTICO", api_key=key2, start_date="1987-05-20",order=c("asc"))
  wt_w_oil<-Quandl("FRED/WCOILWTICO", api_key=key2, start_date="1987-05-15",order=c("asc"))
  wt_m_oil<-Quandl("FRED/MCOILWTICO", api_key=key2, start_date="1987-05-01",order=c("asc"))
  
 
  eia_monthly <- data.frame(date=index(eia_m_oil),coredata(eia_m_oil))
  eia_weekly <- data.frame(date=index(eia_w_oil),coredata(eia_w_oil))
  eia_daily <- data.frame(date=index(eia_d_oil),coredata(eia_d_oil))
  
  col_vector <- c("date","price")
  colnames(eia_monthly)<-  col_vector
  colnames(eia_weekly)<-col_vector 
  colnames(eia_daily)<- col_vector 
  colnames(brent_d_oil)<-col_vector
  colnames(brent_w_oil)<-col_vector
  colnames(brent_m_oil)<-col_vector
  colnames(wt_d_oil)<-col_vector
  colnames(wt_w_oil)<-col_vector
  colnames(wt_m_oil)<-col_vector
  
  write.csv(eia_monthly,"eia_monthly.csv")
  write.csv(eia_weekly,"eia_weekly.csv")
  write.csv(eia_daily,"eia_daily.csv")
  write.csv(brent_d_oil,"brent_daily.csv")
  write.csv(brent_w_oil,"brent_weekly.csv")
  write.csv(brent_m_oil,"brent_monthly.csv")
  write.csv(wt_d_oil,"wt_daily.csv")
  write.csv(wt_w_oil,"wt_weekly.csv")
  write.csv(wt_m_oil,"wt_monthly.csv")
  
}