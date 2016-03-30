require(EIAdata)

update_csv<-function(){
  key<-("95365B2462BFD45A96D4EB0DBC60E59A")
  d_oil<-getEIA("PET.RBRTE.D",key)
  w_oil<-getEIA("PET.RBRTE.W",key)
  m_oil<-getEIA("PET.RBRTE.M",key)
 
  monthly <- data.frame(date=index(m_oil),coredata(m_oil))
  weekly <- data.frame(date=index(w_oil),coredata(w_oil))
  daily <- data.frame(date=index(d_oil),coredata(d_oil))
  colnames(monthly)<- c("date","price") 
  colnames(weekly)<- c("date","price") 
  colnames(daily)<- c("date","price") 
  write.csv(monthly,"monthly.csv")
  write.csv(weekly,"weekly.csv")
  write.csv(daily,"daily.csv")
}