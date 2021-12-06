library(readxl)
library(writexl)
library(tidyverse)
library(rgdal)
library(lmtest)
library(sf)
library(sandwich)

########

data <- readRDS("stolypin.RDS")

########

m1<-lm(factories_workers_diff~log(consolidate_ind+1) +railroad +
         log(migrants_sum)+rwtarif_1907+
         print_num_1904+log(city_pop_1904)+
         log(rwage_1907+1)+log(uwage_1907+1)+livestock_1907+yield_1907+community_space_share+
         russian_per+
         log(rural_pop_1907)+#delta_rural_pop+
         log(factories_workers_1904+1), #log rural pop
       data = data )


m2<-lm(factories_num_diff~log(consolidate_ind+1) +railroad +log(migrants_sum) +
         rwtarif_1907 +
         print_num_1904+log(city_pop_1904)+log(rural_pop_1907)+
         log(rwage_1907+1)+log(uwage_1907+1)+livestock_1907+yield_1907+community_space_share+
         russian_per+
         #delta_rural_pop+
         log(factories_num_1904+1),
       data = data )

m3<-lm(factory_prod_diff~log(consolidate_ind+1) + railroad +log(migrants_sum) +
         #log(factories_workers_1904+1)+  
         rwtarif_1907+
         log(rwage_1907+1)+log(uwage_1907+1)+livestock_1907+yield_1907+community_space_share+
         russian_per+
         print_num_1904+log(city_pop_1904)+log(rural_pop_1907)+
         #delta_rural_pop+
         log(factory_prod_1904+1),
       data = data )

m4<-lm(factory_prod_pw_diff~log(consolidate_ind+1)+railroad +log(migrants_sum)+ rwtarif_1907+
         print_num_1904+log(city_pop_1904)+log(rural_pop_1907)+
         log(rwage_1907+1)+log(uwage_1907+1)+livestock_1907+yield_1907+community_space_share+
         russian_per+
         #delta_rural_pop+
         log(factory_prod_pw_1904+1),
       data = data )


m5<-lm(worker_pf_diff~log(consolidate_ind+1)+railroad +log(migrants_sum)+rwtarif_1907+
         print_num_1904+log(city_pop_1904)+log(rural_pop_1907)+
         log(rwage_1907+1)+log(uwage_1907+1)+livestock_1907+yield_1907+community_space_share+
         russian_per+
         #delta_rural_pop+
         log(workers_pf1904+1),
       data = data )

m6<-lm(prod_pf_diff~log(consolidate_ind+1)+railroad +log(migrants_sum)+rwtarif_1907+
         print_num_1904+log(city_pop_1904)+log(rural_pop_1907)+#delta_rural_pop+
         log(rwage_1907+1)+log(uwage_1907+1)+livestock_1907+yield_1907+community_space_share+
         russian_per+
         log(prod_pf_1904+1),
       data = data )

se1 <- c(list(sqrt(diag(vcovHC(m1, type = "HC0"))),
              sqrt(diag(vcovHC(m2, type = "HC0"))), 
              sqrt(diag(vcovHC(m3, type = "HC0"))),
              sqrt(diag(vcovHC(m4, type = "HC0"))),
              sqrt(diag(vcovHC(m5, type = "HC0"))),
              sqrt(diag(vcovHC(m6, type = "HC0")))
))


stargazer::stargazer(m1,m2,m3,m4,m5, m6, se=se1)



m12<-lm(factories_workers_diff~log(exits_sum+1) +railroad +
          log(migrants_sum)+rwtarif_1907+
          print_num_1904+log(city_pop_1904)+
          log(rwage_1907+1)+log(uwage_1907+1)+livestock_1907+yield_1907+community_space_share+
          russian_per+
          log(rural_pop_1907)+#delta_rural_pop+
          log(factories_workers_1904+1), #log rural pop
        data = data )


m22<-lm(factories_num_diff~log(exits_sum+1) +railroad +log(migrants_sum) +
          rwtarif_1907 +
          print_num_1904+log(city_pop_1904)+log(rural_pop_1907)+
          log(rwage_1907+1)+log(uwage_1907+1)+livestock_1907+yield_1907+community_space_share+
          russian_per+
          #delta_rural_pop+
          log(factories_num_1904+1),
        data = data )


m32<-lm(factory_prod_diff~log(exits_sum+1) + railroad +log(migrants_sum) +
          #log(factories_workers_1904+1)+  
          rwtarif_1907+
          log(rwage_1907+1)+log(uwage_1907+1)+livestock_1907+yield_1907+community_space_share+
          russian_per+
          print_num_1904+log(city_pop_1904)+log(rural_pop_1907)+
          #delta_rural_pop+
          log(factory_prod_1904+1),
        data = data )


m42<-lm(factory_prod_pw_diff~log(exits_sum+1)+railroad +log(migrants_sum)+ rwtarif_1907+
          print_num_1904+log(city_pop_1904)+log(rural_pop_1907)+
          log(rwage_1907+1)+log(uwage_1907+1)+livestock_1907+yield_1907+community_space_share+
          russian_per+
          #delta_rural_pop+
          log(factory_prod_pw_1904+1),
        data = data )


m52<-lm(worker_pf_diff~log(exits_sum+1)+railroad +log(migrants_sum)+rwtarif_1907+
          print_num_1904+log(city_pop_1904)+log(rural_pop_1907)+
          log(rwage_1907+1)+log(uwage_1907+1)+livestock_1907+yield_1907+community_space_share+
          russian_per+
          #delta_rural_pop+
          log(workers_pf1904+1),
        data = data )

m62<-lm(prod_pf_diff~log(exits_sum+1)+railroad +log(migrants_sum)+rwtarif_1907+
          print_num_1904+log(city_pop_1904)+log(rural_pop_1907)+#delta_rural_pop+
          log(rwage_1907+1)+log(uwage_1907+1)+livestock_1907+yield_1907+community_space_share+
          russian_per+
          log(prod_pf_1904+1),
        data = data )

se12 <- c(list(sqrt(diag(vcovHC(m12, type = "HC0"))),
               sqrt(diag(vcovHC(m22, type = "HC0"))), 
               sqrt(diag(vcovHC(m32, type = "HC0"))),
               sqrt(diag(vcovHC(m42, type = "HC0"))),
               sqrt(diag(vcovHC(m52, type = "HC0"))),
               sqrt(diag(vcovHC(m62, type = "HC0")))
))


stargazer::stargazer(m12,m22,m32,m42,m52,m62, se=se12)


####sensitivity####

m21<-lm(factories_workers_diff~log(consolidate_ind_coll_sum+1) +railroad +
          log(migrants_sum)+rwtarif_1907+
          print_num_1904+log(city_pop_1904)+
          log(rwage_1907+1)+log(uwage_1907+1)+livestock_1907+yield_1907+community_space_share+
          russian_per+
          log(rural_pop_1907)+#delta_rural_pop+
          log(factories_workers_1904+1), #log rural pop
        data = data )


m2<-lm(factories_num_diff~log(consolidate_ind_coll_sum+1) +railroad +log(migrants_sum) +
         rwtarif_1907 +
         print_num_1904+log(city_pop_1904)+log(rural_pop_1907)+
         log(rwage_1907+1)+log(uwage_1907+1)+livestock_1907+yield_1907+community_space_share+
         russian_per+
         #delta_rural_pop+
         log(factories_num_1904+1),
       data = data )


m23<-lm(factory_prod_diff~log(consolidate_ind_coll_sum+1) + railroad +log(migrants_sum) +
          #log(factories_workers_1904+1)+  
          rwtarif_1907+
          log(rwage_1907+1)+log(uwage_1907+1)+livestock_1907+yield_1907+community_space_share+
          russian_per+
          print_num_1904+log(city_pop_1904)+log(rural_pop_1907)+
          #delta_rural_pop+
          log(factory_prod_1904+1),
        data = data )


m24<-lm(factory_prod_pw_diff~log(consolidate_ind_coll_sum+1)+railroad +log(migrants_sum)+
          rwtarif_1907+
          print_num_1904+log(city_pop_1904)+log(rural_pop_1907)+
          log(rwage_1907+1)+log(uwage_1907+1)+livestock_1907+yield_1907+community_space_share+
          russian_per+
          #delta_rural_pop+
          log(factory_prod_pw_1904+1),
        data = data )


m25<-lm(worker_pf_diff~log(consolidate_ind_coll_sum+1)+railroad +log(migrants_sum)+rwtarif_1907+
          print_num_1904+log(city_pop_1904)+log(rural_pop_1907)+
          log(rwage_1907+1)+log(uwage_1907+1)+livestock_1907+yield_1907+community_space_share+
          russian_per+
          #delta_rural_pop+
          log(workers_pf1904+1),
        data = data )

m26<-lm(prod_pf_diff~log(consolidate_ind_coll_sum+1)+railroad +log(migrants_sum)+rwtarif_1907+
          print_num_1904+log(city_pop_1904)+log(rural_pop_1907)+#delta_rural_pop+
          log(rwage_1907+1)+log(uwage_1907+1)+livestock_1907+yield_1907+community_space_share+
          russian_per+
          log(prod_pf_1904+1),
        data = data )

se21 <- c(list(sqrt(diag(vcovHC(m21, type = "HC0"))),
               sqrt(diag(vcovHC(m2, type = "HC0"))), 
               sqrt(diag(vcovHC(m23, type = "HC0"))),
               sqrt(diag(vcovHC(m24, type = "HC0"))),
               sqrt(diag(vcovHC(m25, type = "HC0"))),
               sqrt(diag(vcovHC(m26, type = "HC0")))
))


stargazer::stargazer(m21,m2,m23,m24,m25,m26, se=se21)


m212<-lm(factories_workers_diff~log(consolidate_ind_sing+1) +railroad +
           log(migrants_sum)+rwtarif_1907+
           print_num_1904+log(city_pop_1904)+
           log(rwage_1907+1)+log(uwage_1907+1)+livestock_1907+yield_1907+community_space_share+
           russian_per+
           log(rural_pop_1907)+#delta_rural_pop+
           log(factories_workers_1904+1), #log rural pop
         data = data )


m222<-lm(factories_num_diff~log(consolidate_ind_sing+1) +railroad +log(migrants_sum) +
           rwtarif_1907 +
           print_num_1904+log(city_pop_1904)+log(rural_pop_1907)+
           log(rwage_1907+1)+log(uwage_1907+1)+livestock_1907+yield_1907+community_space_share+
           russian_per+
           #delta_rural_pop+
           log(factories_num_1904+1),
         data = data )


m232<-lm(factory_prod_diff~log(consolidate_ind_sing+1) + railroad +log(migrants_sum) +
           #log(factories_workers_1904+1)+  
           rwtarif_1907+
           log(rwage_1907+1)+log(uwage_1907+1)+livestock_1907+yield_1907+community_space_share+
           russian_per+
           print_num_1904+log(city_pop_1904)+log(rural_pop_1907)+
           #delta_rural_pop+
           log(factory_prod_1904+1),
         data = data )


m242<-lm(factory_prod_pw_diff~log(consolidate_ind_sing+1)+railroad +log(migrants_sum)+ 
           rwtarif_1907+
           print_num_1904+log(city_pop_1904)+log(rural_pop_1907)+
           log(rwage_1907+1)+log(uwage_1907+1)+livestock_1907+yield_1907+community_space_share+
           russian_per+
           #delta_rural_pop+
           log(factory_prod_pw_1904+1),
         data = data )


m252<-lm(worker_pf_diff~log(consolidate_ind_sing+1)+railroad +log(migrants_sum)+rwtarif_1907+
           print_num_1904+log(city_pop_1904)+log(rural_pop_1907)+
           log(rwage_1907+1)+log(uwage_1907+1)+livestock_1907+yield_1907+community_space_share+
           russian_per+
           #delta_rural_pop+
           log(workers_pf1904+1),
         data = data )

m262<-lm(prod_pf_diff~log(consolidate_ind_sing+1)+railroad +log(migrants_sum)+rwtarif_1907+
           print_num_1904+log(city_pop_1904)+log(rural_pop_1907)+#delta_rural_pop+
           log(rwage_1907+1)+log(uwage_1907+1)+livestock_1907+yield_1907+community_space_share+
           russian_per+
           log(prod_pf_1904+1),
         data = data )

se22 <- c(list(sqrt(diag(vcovHC(m212, type = "HC0"))),
               sqrt(diag(vcovHC(m222, type = "HC0"))), 
               sqrt(diag(vcovHC(m232, type = "HC0"))),
               sqrt(diag(vcovHC(m242, type = "HC0"))),
               sqrt(diag(vcovHC(m252, type = "HC0"))),
               sqrt(diag(vcovHC(m262, type = "HC0")))
))


stargazer::stargazer(m212,m222,m232,m242,m252,m262, se=se22)

####communes####

m31<-lm(factories_workers_diff~log(consolidate_ind_coll_sum+1)+repartition_province_1907+
          I(log(consolidate_ind_coll_sum+1)*repartition_province_1907)+
          +railroad +
          log(migrants_sum)+rwtarif_1907+
          print_num_1904+log(city_pop_1904)+
          log(rwage_1907+1)+log(uwage_1907+1)+livestock_1907+yield_1907+#community_space_share+
          russian_per+
          log(rural_pop_1907)+#delta_rural_pop+
          log(factories_workers_1904+1), #log rural pop
        data = data )


m32<-lm(factories_num_diff~log(consolidate_ind_coll_sum+1) +repartition_province_1907+
          I(log(consolidate_ind_coll_sum+1)*repartition_province_1907)+
          +railroad +log(migrants_sum) +
          rwtarif_1907 +
          print_num_1904+log(city_pop_1904)+log(rural_pop_1907)+
          log(rwage_1907+1)+log(uwage_1907+1)+livestock_1907+yield_1907+#community_space_share+
          russian_per+
          #delta_rural_pop+
          log(factories_num_1904+1),
        data = data )


m33<-lm(factory_prod_diff~log(consolidate_ind_coll_sum+1) +repartition_province_1907+
          I(log(consolidate_ind_coll_sum+1)*repartition_province_1907)+
          + railroad +log(migrants_sum) +
          #log(factories_workers_1904+1)+  
          rwtarif_1907+
          log(rwage_1907+1)+log(uwage_1907+1)+livestock_1907+yield_1907+#community_space_share+
          russian_per+
          print_num_1904+log(city_pop_1904)+log(rural_pop_1907)+
          #delta_rural_pop+
          log(factory_prod_1904+1),
        data = data )


m34<-lm(factory_prod_pw_diff~log(consolidate_ind_coll_sum+1)+repartition_province_1907+
          I(log(consolidate_ind_coll_sum+1)*repartition_province_1907)+
          +railroad +log(migrants_sum)+ rwtarif_1907+
          print_num_1904+log(city_pop_1904)+log(rural_pop_1907)+
          log(rwage_1907+1)+log(uwage_1907+1)+livestock_1907+yield_1907+#community_space_share+
          russian_per+
          #delta_rural_pop+
          log(factory_prod_pw_1904+1),
        data = data )


m35<-lm(worker_pf_diff~log(consolidate_ind_coll_sum+1)+repartition_province_1907+
          I(log(consolidate_ind_coll_sum+1)*repartition_province_1907)+
          +railroad +log(migrants_sum)+rwtarif_1907+
          print_num_1904+log(city_pop_1904)+log(rural_pop_1907)+
          log(rwage_1907+1)+log(uwage_1907+1)+livestock_1907+yield_1907+#community_space_share+
          russian_per+
          #delta_rural_pop+
          log(workers_pf1904+1),
        data = data )

m36<-lm(prod_pf_diff~log(consolidate_ind_coll_sum+1)+repartition_province_1907+
          I(log(consolidate_ind_coll_sum+1)*repartition_province_1907)+
          +railroad +log(migrants_sum)+rwtarif_1907+
          print_num_1904+log(city_pop_1904)+log(rural_pop_1907)+#delta_rural_pop+
          log(rwage_1907+1)+log(uwage_1907+1)+livestock_1907+yield_1907+#community_space_share+
          russian_per+
          log(prod_pf_1904+1),
        data = data )

se31 <- c(list(sqrt(diag(vcovHC(m31, type = "HC0"))),
               sqrt(diag(vcovHC(m32, type = "HC0"))), 
               sqrt(diag(vcovHC(m33, type = "HC0"))),
               sqrt(diag(vcovHC(m34, type = "HC0"))),
               sqrt(diag(vcovHC(m35, type = "HC0"))),
               sqrt(diag(vcovHC(m36, type = "HC0")))
))


stargazer::stargazer(m31,m32,m33,m34,m35,m36, se=se31)


m312<-lm(factories_workers_diff~log(consolidate_ind+1) +repartition_province_1907+
           I(log(consolidate_ind+1)*repartition_province_1907)+
           +railroad +
           log(migrants_sum)+rwtarif_1907+
           print_num_1904+log(city_pop_1904)+
           log(rwage_1907+1)+log(uwage_1907+1)+livestock_1907+yield_1907+#community_space_share+
           russian_per+
           log(rural_pop_1907)+#delta_rural_pop+
           log(factories_workers_1904+1), #log rural pop
         data = data )


m322<-lm(factories_num_diff~log(consolidate_ind+1)+repartition_province_1907+
           I(log(consolidate_ind+1)*repartition_province_1907)+
           +railroad +log(migrants_sum) +
           rwtarif_1907 +
           print_num_1904+log(city_pop_1904)+log(rural_pop_1907)+
           log(rwage_1907+1)+log(uwage_1907+1)+livestock_1907+yield_1907+#community_space_share+
           russian_per+
           #delta_rural_pop+
           log(factories_num_1904+1),
         data = data )


m332<-lm(factory_prod_diff~log(consolidate_ind+1) +repartition_province_1907+
           I(log(consolidate_ind+1)*repartition_province_1907)+
           + railroad +log(migrants_sum) +
           #log(factories_workers_1904+1)+  
           rwtarif_1907+
           log(rwage_1907+1)+log(uwage_1907+1)+livestock_1907+yield_1907+#community_space_share+
           russian_per+
           print_num_1904+log(city_pop_1904)+log(rural_pop_1907)+
           #delta_rural_pop+
           log(factory_prod_1904+1),
         data = data )


m342<-lm(factory_prod_pw_diff~log(consolidate_ind+1)+repartition_province_1907+
           I(log(consolidate_ind+1)*repartition_province_1907)+
           +railroad +log(migrants_sum)+ rwtarif_1907+
           print_num_1904+log(city_pop_1904)+log(rural_pop_1907)+
           log(rwage_1907+1)+log(uwage_1907+1)+livestock_1907+yield_1907+#community_space_share+
           russian_per+
           #delta_rural_pop+
           log(factory_prod_pw_1904+1),
         data = data )


m352<-lm(worker_pf_diff~log(consolidate_ind+1)+repartition_province_1907+
           I(log(consolidate_ind+1)*repartition_province_1907)+
           +railroad +log(migrants_sum)+rwtarif_1907+
           print_num_1904+log(city_pop_1904)+log(rural_pop_1907)+
           log(rwage_1907+1)+log(uwage_1907+1)+livestock_1907+yield_1907+#community_space_share+
           russian_per+
           #delta_rural_pop+
           log(workers_pf1904+1),
         data = data )

m362<-lm(prod_pf_diff~log(consolidate_ind+1)+repartition_province_1907+
           I(log(consolidate_ind+1)*repartition_province_1907)+
           +railroad +log(migrants_sum)+rwtarif_1907+
           print_num_1904+log(city_pop_1904)+log(rural_pop_1907)+#delta_rural_pop+
           log(rwage_1907+1)+log(uwage_1907+1)+livestock_1907+yield_1907+#community_space_share+
           russian_per+
           log(prod_pf_1904+1),
         data = data )

se32 <- c(list(sqrt(diag(vcovHC(m312, type = "HC0"))),
               sqrt(diag(vcovHC(m322, type = "HC0"))), 
               sqrt(diag(vcovHC(m332, type = "HC0"))),
               sqrt(diag(vcovHC(m342, type = "HC0"))),
               sqrt(diag(vcovHC(m352, type = "HC0"))),
               sqrt(diag(vcovHC(m362, type = "HC0")))
))


stargazer::stargazer(m312,m322,m332,m342,m352,m362, se=se32)

data$log_consolidate_ind <- log(data$consolidate_ind+1)

m312a<-lm(factories_workers_diff~log_consolidate_ind +repartition_province_1907+
            I(log_consolidate_ind*repartition_province_1907)+
            +railroad +
            log(migrants_sum)+rwtarif_1907+
            print_num_1904+log(city_pop_1904)+
            log(rwage_1907+1)+log(uwage_1907+1)+livestock_1907+yield_1907+#community_space_share+
            russian_per+
            log(rural_pop_1907)+#delta_rural_pop+
            log(factories_workers_1904+1), #log rural pop
          data = data )


m322a<-lm(factories_num_diff~log_consolidate_ind+repartition_province_1907+
            I(log_consolidate_ind*repartition_province_1907)+
            +railroad +log(migrants_sum) +
            rwtarif_1907 +
            print_num_1904+log(city_pop_1904)+log(rural_pop_1907)+
            log(rwage_1907+1)+log(uwage_1907+1)+livestock_1907+yield_1907+#community_space_share+
            russian_per+
            #delta_rural_pop+
            log(factories_num_1904+1),
          data = data )


m332a<-lm(factory_prod_diff~log_consolidate_ind +repartition_province_1907+
            I(log_consolidate_ind*repartition_province_1907)+
            + railroad +log(migrants_sum) +
            #log(factories_workers_1904+1)+  
            rwtarif_1907+
            log(rwage_1907+1)+log(uwage_1907+1)+livestock_1907+yield_1907+#community_space_share+
            russian_per+
            print_num_1904+log(city_pop_1904)+log(rural_pop_1907)+
            #delta_rural_pop+
            log(factory_prod_1904+1),
          data = data )

m342a<-lm(factory_prod_pw_diff~log_consolidate_ind+repartition_province_1907+
            I(log_consolidate_ind*repartition_province_1907)+
            +railroad +log(migrants_sum)+ rwtarif_1907+
            print_num_1904+log(city_pop_1904)+log(rural_pop_1907)+
            log(rwage_1907+1)+log(uwage_1907+1)+livestock_1907+yield_1907+#community_space_share+
            russian_per+
            #delta_rural_pop+
            log(factory_prod_pw_1904+1),
          data = data )


m352a<-lm(worker_pf_diff~log_consolidate_ind+repartition_province_1907+
            I(log_consolidate_ind*repartition_province_1907)+
            +railroad +log(migrants_sum)+rwtarif_1907+
            print_num_1904+log(city_pop_1904)+log(rural_pop_1907)+
            log(rwage_1907+1)+log(uwage_1907+1)+livestock_1907+yield_1907+#community_space_share+
            russian_per+
            #delta_rural_pop+
            log(workers_pf1904+1),
          data = data )

m362a<-lm(prod_pf_diff~log_consolidate_ind+repartition_province_1907+
            I(log_consolidate_ind*repartition_province_1907)+
            +railroad +log(migrants_sum)+rwtarif_1907+
            print_num_1904+log(city_pop_1904)+log(rural_pop_1907)+#delta_rural_pop+
            log(rwage_1907+1)+log(uwage_1907+1)+livestock_1907+yield_1907+#community_space_share+
            russian_per+
            log(prod_pf_1904+1),
          data = data )

pm1 <- ggplot(ggeffects::ggpredict(m312a, terms = c("log_consolidate_ind", 
                                                    "repartition_province_1907")),
              aes(x = x, y = predicted, colour = group)) +
  geom_line()+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, color=group), alpha = .1)+
  labs(color = "Repartition province")+
  scale_color_brewer(labels = c("No", "Yes"), palette = "Set1")+
  #xlab("Consolidations (log)")+
  #ylab("Factory workers growth")+
  xlab("")+ylab("")+
  ggtitle("Factory workers growth")+
  theme_bw()

pm2 <- ggplot(ggeffects::ggpredict(m322a, terms = c("log_consolidate_ind", 
                                                    "repartition_province_1907")),
              aes(x = x, y = predicted, colour = group)) +
  geom_line()+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, color=group), alpha = .1)+
  labs(color = "Repartition province")+
  scale_color_brewer(labels = c("No", "Yes"), palette = "Set1")+
  #xlab("Consolidations (log)")+
  #ylab("Factories number growth")+
  xlab("")+ylab("")+
  ggtitle("Factories number growth")+
  theme_bw()

pm3 <- ggplot(ggeffects::ggpredict(m332a, terms = c("log_consolidate_ind", 
                                                    "repartition_province_1907")),
              aes(x = x, y = predicted, colour = group)) +
  geom_line()+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, color=group), alpha = .1)+
  labs(color = "Repartition province")+
  scale_color_brewer(labels = c("No", "Yes"), palette = "Set1")+
  #xlab("Consolidations (log)")+
  #ylab("Factories production growth")+
  xlab("")+ylab("")+
  ggtitle("Factories production growth")+
  theme_bw()

pm4 <- ggplot(ggeffects::ggpredict(m342a, terms = c("log_consolidate_ind", 
                                                    "repartition_province_1907")),
              aes(x = x, y = predicted, colour = group)) +
  geom_line()+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, color=group), alpha = .1)+
  labs(color = "Repartition province")+
  scale_color_brewer(labels = c("No", "Yes"), palette = "Set1")+
  #xlab("Consolidations (log)")+
  #ylab("Production per worker growth")+
  xlab("")+ylab("")+
  ggtitle("Production per worker growth")+
  theme_bw()

pm5 <- ggplot(ggeffects::ggpredict(m352a, terms = c("log_consolidate_ind", 
                                                    "repartition_province_1907")),
              aes(x = x, y = predicted, colour = group)) +
  geom_line()+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, color=group), alpha = .1)+
  labs(color = "Repartition province")+
  scale_color_brewer(labels = c("No", "Yes"), palette = "Set1")+
  #xlab("Consolidations (log)")+
  #ylab("Workers per fabric growth")+
  xlab("")+ylab("")+
  ggtitle("Workers per fabric growth")+
  theme_bw()

pm6 <- ggplot(ggeffects::ggpredict(m362a, terms = c("log_consolidate_ind", 
                                                    "repartition_province_1907")),
              aes(x = x, y = predicted, colour = group)) +
  geom_line()+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, color=group), alpha = .1)+
  labs(color = "Repartition province")+
  scale_color_brewer(labels = c("No", "Yes"), palette = "Set1")+
  #xlab("Consolidations (log)")+
  #ylab("Production per fabric growth")+
  xlab("")+ylab("")+
  ggtitle("Production per fabric growth")+
  theme_bw()


pms <- ggpubr::ggarrange(pm1, pm2, pm3, pm4, pm5, pm6,
                         ncol=3, nrow=2,
                         common.legend=T#, legend = "bottom"
)

ggpubr::annotate_figure(pms, 
                        top = ggpubr::text_grob("Marginal effects",
                                                size=18),
                        bottom = ggpubr::text_grob("Consolidations (log)",
                                                   size=14))

####spillovers#####


m41<-lm(factories_workers_diff~log(exits_sum_near+1) +railroad +
          log(migrants_sum)+rwtarif_1907+
          print_num_1904+log(city_pop_1904)+
          log(rwage_1907+1)+log(uwage_1907+1)+livestock_1907+yield_1907+community_space_share+
          russian_per+
          log(rural_pop_1907)+#delta_rural_pop+
          log(factories_workers_1904+1), #log rural pop
        data = data )


m42<-lm(factories_num_diff~log(exits_sum_near+1) +railroad +log(migrants_sum) +
          rwtarif_1907 +
          print_num_1904+log(city_pop_1904)+log(rural_pop_1907)+
          log(rwage_1907+1)+log(uwage_1907+1)+livestock_1907+yield_1907+community_space_share+
          russian_per+
          #delta_rural_pop+
          log(factories_num_1904+1),
        data = data )


m43<-lm(factory_prod_diff~log(exits_sum_near+1) + railroad +log(migrants_sum) +
          #log(factories_workers_1904+1)+  
          rwtarif_1907+
          log(rwage_1907+1)+log(uwage_1907+1)+livestock_1907+yield_1907+community_space_share+
          russian_per+
          print_num_1904+log(city_pop_1904)+log(rural_pop_1907)+
          #delta_rural_pop+
          log(factory_prod_1904+1),
        data = data )


m44<-lm(factory_prod_pw_diff~log(exits_sum_near+1)+railroad +log(migrants_sum)+ rwtarif_1907+
          print_num_1904+log(city_pop_1904)+log(rural_pop_1907)+
          log(rwage_1907+1)+log(uwage_1907+1)+livestock_1907+yield_1907+community_space_share+
          russian_per+
          #delta_rural_pop+
          log(factory_prod_pw_1904+1),
        data = data )


m45<-lm(worker_pf_diff~log(exits_sum_near+1)+railroad +log(migrants_sum)+rwtarif_1907+
          print_num_1904+log(city_pop_1904)+log(rural_pop_1907)+
          log(rwage_1907+1)+log(uwage_1907+1)+livestock_1907+yield_1907+community_space_share+
          russian_per+
          #delta_rural_pop+
          log(workers_pf1904+1),
        data = data )

m46<-lm(prod_pf_diff~log(exits_sum_near+1)+railroad +log(migrants_sum)+rwtarif_1907+
          print_num_1904+log(city_pop_1904)+log(rural_pop_1907)+#delta_rural_pop+
          log(rwage_1907+1)+log(uwage_1907+1)+livestock_1907+yield_1907+community_space_share+
          russian_per+
          log(prod_pf_1904+1),
        data = data )

se41 <- c(list(sqrt(diag(vcovHC(m41, type = "HC0"))),
               sqrt(diag(vcovHC(m42, type = "HC0"))), 
               sqrt(diag(vcovHC(m43, type = "HC0"))),
               sqrt(diag(vcovHC(m44, type = "HC0"))),
               sqrt(diag(vcovHC(m45, type = "HC0"))),
               sqrt(diag(vcovHC(m46, type = "HC0")))
))


stargazer::stargazer(m41,m42,m43,m44,m45,m46, se=se41)


m41c<-lm(factories_workers_diff~log(consolidate_ind_near+1) +railroad +
           log(migrants_sum)+rwtarif_1907+
           print_num_1904+log(city_pop_1904)+
           log(rwage_1907+1)+log(uwage_1907+1)+livestock_1907+yield_1907+community_space_share+
           russian_per+
           log(rural_pop_1907)+#delta_rural_pop+
           log(factories_workers_1904+1), #log rural pop
         data = data )


m42c<-lm(factories_num_diff~log(consolidate_ind_near+1) +railroad +log(migrants_sum) +
           rwtarif_1907 +
           print_num_1904+log(city_pop_1904)+log(rural_pop_1907)+
           log(rwage_1907+1)+log(uwage_1907+1)+livestock_1907+yield_1907+community_space_share+
           russian_per+
           #delta_rural_pop+
           log(factories_num_1904+1),
         data = data )


m43c<-lm(factory_prod_diff~log(consolidate_ind_near+1) + railroad +log(migrants_sum) +
           #log(factories_workers_1904+1)+  
           rwtarif_1907+
           log(rwage_1907+1)+log(uwage_1907+1)+livestock_1907+yield_1907+community_space_share+
           russian_per+
           print_num_1904+log(city_pop_1904)+log(rural_pop_1907)+
           #delta_rural_pop+
           log(factory_prod_1904+1),
         data = data )


m44c<-lm(factory_prod_pw_diff~log(consolidate_ind_near+1)+railroad +log(migrants_sum)+ 
           rwtarif_1907+
           print_num_1904+log(city_pop_1904)+log(rural_pop_1907)+
           log(rwage_1907+1)+log(uwage_1907+1)+livestock_1907+yield_1907+community_space_share+
           russian_per+
           #delta_rural_pop+
           log(factory_prod_pw_1904+1),
         data = data )


m45c<-lm(worker_pf_diff~log(consolidate_ind_near+1)+railroad +log(migrants_sum)+rwtarif_1907+
           print_num_1904+log(city_pop_1904)+log(rural_pop_1907)+
           log(rwage_1907+1)+log(uwage_1907+1)+livestock_1907+yield_1907+community_space_share+
           russian_per+
           #delta_rural_pop+
           log(workers_pf1904+1),
         data = data )

m46c<-lm(prod_pf_diff~log(consolidate_ind_near+1)+railroad +log(migrants_sum)+rwtarif_1907+
           print_num_1904+log(city_pop_1904)+log(rural_pop_1907)+#delta_rural_pop+
           log(rwage_1907+1)+log(uwage_1907+1)+livestock_1907+yield_1907+community_space_share+
           russian_per+
           log(prod_pf_1904+1),
         data = data )

se41c <- c(list(sqrt(diag(vcovHC(m41c, type = "HC0"))),
                sqrt(diag(vcovHC(m42c, type = "HC0"))), 
                sqrt(diag(vcovHC(m43c, type = "HC0"))),
                sqrt(diag(vcovHC(m44c, type = "HC0"))),
                sqrt(diag(vcovHC(m45c, type = "HC0"))),
                sqrt(diag(vcovHC(m46c, type = "HC0")))
))


stargazer::stargazer(m41c,m42c,m43c,m44c,m45c,m46c, se=se41c)

####inflation####

data$factory_prod_inf <- data$factory_prod/0.896226779
data$factory_prod_1904_inf <- data$factory_prod_1904/0.767646501


data$prod_pf_inf <- data$factory_prod_inf/data$factories_num
data$prod_pf_1904_inf <- data$factory_prod_1904_inf/data$factories_num_1904
data$prod_pf_inf <- na_if(data$prod_pf_inf, "Inf")
data$prod_pf_inf <- na_if(data$prod_pf_inf, "NaN")
data$prod_pf_1904_inf <- na_if(data$prod_pf_1904_inf, "Inf")
data$prod_pf_1904_inf <- na_if(data$prod_pf_1904_inf, "NaN")

data$factory_prod_pw_inf <- data$factory_prod_inf/data$factories_workers
data$factory_prod_pw_1904_inf <- data$factory_prod_1904_inf/data$factories_workers_1904
data$factory_prod_pw_inf <- na_if(data$prod_pw_inf, "Inf")
data$factory_prod_pw_inf <- na_if(data$prod_pw_inf, "NaN")
data$factory_prod_pw_1904_inf <- na_if(data$factory_prod_pw_1904_inf, "Inf")
data$factory_prod_pw_1904_inf <- na_if(data$factory_prod_pw_1904_inf, "NaN")

data$factory_prod_diff_inf <- log(data$factory_prod_inf+1)-log(data$factory_prod_1904_inf+1)
data$factory_prod_pw_diff_inf <- log(data$factory_prod_pw_inf+1)-
  log(data$factory_prod_pw_1904_inf+1)
data$prod_pf_diff_inf <- log(data$prod_pf_inf+1)-log(data$prod_pf_1904_inf+1)
data$factory_prod_pw_diff_inf <- na_if(data$factory_prod_pw_diff_inf, "Inf")
data$factory_prod_pw_diff_inf <- na_if(data$factory_prod_pw_diff_inf, "NaN")


m3_inf<-lm(factory_prod_diff_inf~log(consolidate_ind+1) + railroad +log(migrants_sum) +
             #log(factories_workers_1904+1)+  
             rwtarif_1907+
             log(rwage_1907+1)+log(uwage_1907+1)+livestock_1907+yield_1907+community_space_share+
             russian_per+
             print_num_1904+log(city_pop_1904)+log(rural_pop_1907)+
             #delta_rural_pop+
             log(factory_prod_1904_inf+1),
           data = data )


m4_inf<-lm(factory_prod_pw_diff_inf~log(consolidate_ind+1)+railroad +log(migrants_sum)+ 
             rwtarif_1907+
             print_num_1904+log(city_pop_1904)+log(rural_pop_1907)+
             log(rwage_1907+1)+log(uwage_1907+1)+livestock_1907+yield_1907+community_space_share+
             russian_per+
             #delta_rural_pop+
             log(factory_prod_pw_1904_inf+1),
           data = data )


m6_inf<-lm(prod_pf_diff_inf~log(consolidate_ind+1)+railroad +log(migrants_sum)+rwtarif_1907+
             print_num_1904+log(city_pop_1904)+log(rural_pop_1907)+#delta_rural_pop+
             log(rwage_1907+1)+log(uwage_1907+1)+livestock_1907+yield_1907+community_space_share+
             russian_per+
             log(prod_pf_1904_inf+1),
           data = data )

m32_inf<-lm(factory_prod_diff_inf~log(exits_sum+1) + railroad +log(migrants_sum) +
              #log(factories_workers_1904+1)+  
              rwtarif_1907+
              log(rwage_1907+1)+log(uwage_1907+1)+livestock_1907+yield_1907+community_space_share+
              russian_per+
              print_num_1904+log(city_pop_1904)+log(rural_pop_1907)+
              #delta_rural_pop+
              log(factory_prod_1904_inf+1),
            data = data )


m42_inf<-lm(factory_prod_pw_diff_inf~log(exits_sum+1)+railroad +log(migrants_sum)+ rwtarif_1907+
              print_num_1904+log(city_pop_1904)+log(rural_pop_1907)+
              log(rwage_1907+1)+log(uwage_1907+1)+livestock_1907+yield_1907+community_space_share+
              russian_per+
              #delta_rural_pop+
              log(factory_prod_pw_1904_inf+1),
            data = data )

m62_inf<-lm(prod_pf_diff_inf~log(exits_sum+1)+railroad +log(migrants_sum)+rwtarif_1907+
              print_num_1904+log(city_pop_1904)+log(rural_pop_1907)+#delta_rural_pop+
              log(rwage_1907+1)+log(uwage_1907+1)+livestock_1907+yield_1907+community_space_share+
              russian_per+
              log(prod_pf_1904_inf+1),
            data = data )

se12_inf <- c(list(sqrt(diag(vcovHC(m3_inf, type = "HC0"))),
                   sqrt(diag(vcovHC(m4_inf, type = "HC0"))), 
                   sqrt(diag(vcovHC(m6_inf, type = "HC0"))),
                   sqrt(diag(vcovHC(m32_inf, type = "HC0"))),
                   sqrt(diag(vcovHC(m42_inf, type = "HC0"))),
                   sqrt(diag(vcovHC(m62_inf, type = "HC0")))
))


stargazer::stargazer(m3_inf,m4_inf,m6_inf,m32_inf,m42_inf,m62_inf, se=se12_inf)

####data description####

stargazer::stargazer(data %>% 
                       select(factories_workers, factories_workers_1904, factories_num, 
                              factories_num_1904,
                              factory_prod, factory_prod_1904, factory_prod_pw, 
                              factory_prod_pw_1904,
                              workers_pf1910, workers_pf1904, prod_pf, prod_pf_1904,
                              factories_workers_diff, factories_num_diff, factory_prod_diff, 
                              factory_prod_pw_diff, worker_pf_diff, prod_pf_diff,
                              exits_sum, consolidate_ind, consolidate_ind_coll_sum,
                              consolidate_ind_sing,
                              exits_sum_near, consolidate_ind_near, consolidate_ind_coll_sum_near,
                              consolidate_ind_sing_near,
                              city_pop, city_pop_1904, print_num, print_num_1904, railroad,
                              rural_pop_1907,migrants_sum, rwtarif_1907, rwage_1907, uwage_1907,
                              livestock_1907,
                              yield_1907, community_space_share, repartition_province_1907,
                              russian_per),
                     digits = 2
                     
)


####more controls####

m1ac<-lm(factories_workers_diff~log(consolidate_ind+1) +railroad +
           log(migrants_sum)+rwtarif_1907+
           print_num_1904+log(city_pop_1904)+
           log(rwage_1907+1)+log(uwage_1907+1)+livestock_1907+yield_1907+community_space_share+
           russian_per+literacy+
           mean_space+rdensity_1907+malorus_per+belorus_per+
           log(rural_pop_1907)+#delta_rural_pop+
           log(factories_workers_1904+1), #log rural pop
         data = data )


m2ac<-lm(factories_num_diff~log(consolidate_ind+1) +railroad +log(migrants_sum) +
           rwtarif_1907 +
           print_num_1904+log(city_pop_1904)+log(rural_pop_1907)+
           log(rwage_1907+1)+log(uwage_1907+1)+livestock_1907+yield_1907+community_space_share+
           russian_per+literacy+
           mean_space+rdensity_1907+malorus_per+belorus_per+
           #delta_rural_pop+
           log(factories_num_1904+1),
         data = data )


m3ac<-lm(factory_prod_diff~log(consolidate_ind+1) + railroad +log(migrants_sum) +
           #log(factories_workers_1904+1)+  
           rwtarif_1907+
           log(rwage_1907+1)+log(uwage_1907+1)+livestock_1907+yield_1907+community_space_share+
           russian_per+literacy+
           mean_space+rdensity_1907+malorus_per+belorus_per+
           print_num_1904+log(city_pop_1904)+log(rural_pop_1907)+
           #delta_rural_pop+
           log(factory_prod_1904+1),
         data = data )



m4ac<-lm(factory_prod_pw_diff~log(consolidate_ind+1)+railroad +log(migrants_sum)+ rwtarif_1907+
           print_num_1904+log(city_pop_1904)+log(rural_pop_1907)+
           log(rwage_1907+1)+log(uwage_1907+1)+livestock_1907+yield_1907+community_space_share+
           russian_per+literacy+
           mean_space+rdensity_1907+malorus_per+belorus_per+
           #delta_rural_pop+
           log(factory_prod_pw_1904+1),
         data = data )


m5ac<-lm(worker_pf_diff~log(consolidate_ind+1)+railroad +log(migrants_sum)+rwtarif_1907+
           print_num_1904+log(city_pop_1904)+log(rural_pop_1907)+
           log(rwage_1907+1)+log(uwage_1907+1)+livestock_1907+yield_1907+community_space_share+
           russian_per+literacy+
           mean_space+rdensity_1907+malorus_per+belorus_per+
           #delta_rural_pop+
           log(workers_pf1904+1),
         data = data )


m6ac<-lm(prod_pf_diff~log(consolidate_ind+1)+railroad +log(migrants_sum)+rwtarif_1907+
           print_num_1904+log(city_pop_1904)+log(rural_pop_1907)+#delta_rural_pop+
           log(rwage_1907+1)+log(uwage_1907+1)+livestock_1907+yield_1907+community_space_share+
           russian_per+literacy+
           mean_space+rdensity_1907+malorus_per+belorus_per+
           log(prod_pf_1904+1),
         data = data )



se1ac <- c(list(sqrt(diag(vcovHC(m1ac, type = "HC0"))),
                sqrt(diag(vcovHC(m2ac, type = "HC0"))), 
                sqrt(diag(vcovHC(m3ac, type = "HC0"))),
                sqrt(diag(vcovHC(m4ac, type = "HC0"))),
                sqrt(diag(vcovHC(m5ac, type = "HC0"))),
                sqrt(diag(vcovHC(m6ac, type = "HC0")))
))


stargazer::stargazer(m1ac,m2ac,m3ac,m4ac,m5ac, m6ac, se=se1ac)



m12ac<-lm(factories_workers_diff~log(exits_sum+1) +railroad +
            log(migrants_sum)+rwtarif_1907+
            print_num_1904+log(city_pop_1904)+
            log(rwage_1907+1)+log(uwage_1907+1)+livestock_1907+yield_1907+community_space_share+
            russian_per+literacy+
            mean_space+rdensity_1907+malorus_per+belorus_per+
            log(rural_pop_1907)+#delta_rural_pop+
            log(factories_workers_1904+1), #log rural pop
          data = data )


m22ac<-lm(factories_num_diff~log(exits_sum+1) +railroad +log(migrants_sum) +
            rwtarif_1907 +
            print_num_1904+log(city_pop_1904)+log(rural_pop_1907)+
            log(rwage_1907+1)+log(uwage_1907+1)+livestock_1907+yield_1907+community_space_share+
            russian_per+literacy+
            mean_space+rdensity_1907+malorus_per+belorus_per+
            #delta_rural_pop+
            log(factories_num_1904+1),
          data = data )


m32ac<-lm(factory_prod_diff~log(exits_sum+1) + railroad +log(migrants_sum) +
            #log(factories_workers_1904+1)+  
            rwtarif_1907+
            log(rwage_1907+1)+log(uwage_1907+1)+livestock_1907+yield_1907+community_space_share+
            russian_per+literacy+
            mean_space+rdensity_1907+malorus_per+belorus_per+
            print_num_1904+log(city_pop_1904)+log(rural_pop_1907)+
            #delta_rural_pop+
            log(factory_prod_1904+1),
          data = data )


m42ac<-lm(factory_prod_pw_diff~log(exits_sum+1)+railroad +log(migrants_sum)+ rwtarif_1907+
            print_num_1904+log(city_pop_1904)+log(rural_pop_1907)+
            log(rwage_1907+1)+log(uwage_1907+1)+livestock_1907+yield_1907+community_space_share+
            russian_per+literacy+
            mean_space+rdensity_1907+malorus_per+belorus_per+
            #delta_rural_pop+
            log(factory_prod_pw_1904+1),
          data = data )


m52ac<-lm(worker_pf_diff~log(exits_sum+1)+railroad +log(migrants_sum)+rwtarif_1907+
            print_num_1904+log(city_pop_1904)+log(rural_pop_1907)+
            log(rwage_1907+1)+log(uwage_1907+1)+livestock_1907+yield_1907+community_space_share+
            russian_per+literacy+
            mean_space+rdensity_1907+malorus_per+belorus_per+
            #delta_rural_pop+
            log(workers_pf1904+1),
          data = data )

m62ac<-lm(prod_pf_diff~log(exits_sum+1)+railroad +log(migrants_sum)+rwtarif_1907+
            print_num_1904+log(city_pop_1904)+log(rural_pop_1907)+#delta_rural_pop+
            log(rwage_1907+1)+log(uwage_1907+1)+livestock_1907+yield_1907+community_space_share+
            russian_per+literacy+
            mean_space+rdensity_1907+malorus_per+belorus_per+
            log(prod_pf_1904+1),
          data = data )

se12ac <- c(list(sqrt(diag(vcovHC(m12ac, type = "HC0"))),
                 sqrt(diag(vcovHC(m22ac, type = "HC0"))), 
                 sqrt(diag(vcovHC(m32ac, type = "HC0"))),
                 sqrt(diag(vcovHC(m42ac, type = "HC0"))),
                 sqrt(diag(vcovHC(m52ac, type = "HC0"))),
                 sqrt(diag(vcovHC(m62ac, type = "HC0")))
))


stargazer::stargazer(m12ac,m22ac,m32ac,m42ac,m52ac,m62ac, se=se12ac)

####maps####

reform2_sum <- unique(data %>% group_by(province) %>% filter(!is.na(consolidate_ind)) %>% 
  summarise(exits_sum = exits_sum,
            consolidate_ind_coll_sum=consolidate_ind_coll_sum,
            consolidate_ind_sing=consolidate_ind_sing,
            consolidate_ind=consolidate_ind,
            regname = regname_1907))

nc0 <- read_sf(dsn = "1897RussianEmpire.shp")
provinces_map <- nc0 %>% select(NameRUS)
#writexl::write_xlsx(as.data.frame(provinces_map$NameRUS), "map_provinces.xlsx")
provinces_map <- readxl::read_xlsx("map_provinces.xlsx")
provinces_map$check <- provinces_map$province %in% reform2_sum$province
table(provinces_map$check)

nc00 <- nc0 %>% select(NameRUS)
nc01 <- tigris::geo_join(nc00, provinces_map, by = "NameRUS")
nc01 <- nc01 %>% filter(province %in% reform2_sum$province)

nc01 <- tigris::geo_join(nc01, reform2_sum, by = "province")
names(nc01)[6] <- "exits"
names(nc01)[7] <- "village-wide"
names(nc01)[8] <- "singular"
names(nc01)[9] <- "consolidations"
names(nc01)[10] <- "region"

p1 <- ggplot(data = nc01)+
  geom_sf(aes(fill = exits))+
  coord_sf(xlim = c(20.5, 66.5),
           ylim = c(42, 71),
           expand = FALSE)+
  ggtitle("Cumulative number of exits", subtitle = "from 1907 to 1910, gray stands for NA")+
  scale_fill_distiller(palette="Greens", direction = 1)+
  theme_bw()

p2 <- ggplot(data=nc01)+
  geom_sf(aes(fill = consolidations), color = "black")+
  coord_sf(xlim = c(20.5, 66.5),
           ylim = c(42, 71),
           expand = FALSE)+
  ggtitle("Cumulative number of consolidations", subtitle = "from 1907 to 1910")+
  scale_fill_distiller(palette="Blues", direction = 1)+
  theme_bw()
ggpubr::ggarrange(p1,p2,ncol=2, widths = c(5,5.275))

nc01$region <- gsub("СПБ округ", "Saint-Petersburg", nc01$region)
nc01$region <- gsub("Киевский округ", "Kyiv", nc01$region)
nc01$region <- gsub("Мос округ", "Moscow", nc01$region)
nc01$region <- gsub("Поволжский округ", "Volga", nc01$region)
nc01$region <- gsub("Варшавский округ", "Warsaw", nc01$region)
nc01$region <- gsub("Харьковский округ", "Kharkov", nc01$region)

ggplot(data=nc01)+
  geom_sf(aes(fill = region), color = "black")+
  coord_sf(xlim = c(20.5, 66.5),
           ylim = c(42, 71),
           expand = FALSE)+
  ggtitle("Territories by region")+
  scale_fill_brewer(palette = "RdBu")+
  theme_bw()

nc01 <- tigris::geo_join(nc01, data %>% select(province, repartition_province_1907),
                         by = "province")
names(nc01)[12] <- "repartition commune"
nc01$`repartition commune` <- recode_factor(nc01$`repartition commune`, '1' = "yes", '0' = "no")

ggplot(data=nc01)+
  geom_sf(aes(fill = `repartition commune`))+
  coord_sf(xlim = c(20.5, 66.5),
           ylim = c(42, 71),
           expand = FALSE)+
  ggtitle("Territories with repartition commune")+
  scale_fill_brewer(palette = "Set1")+
  theme_bw()

p3 <- ggplot(data=nc01)+
  geom_sf(aes(fill = singular), color = "black")+
  coord_sf(xlim = c(20.5, 66.5),
           ylim = c(42, 71),
           expand = FALSE)+
  ggtitle("Singular", subtitle = "from 1907 to 1910, gray stands for NA")+
  scale_fill_distiller(palette="BuGn", direction = 1)+
  theme_bw()

p4 <- ggplot(data=nc01)+
  geom_sf(aes(fill = `village-wide`), color = "black")+
  coord_sf(xlim = c(20.5, 66.5),
           ylim = c(42, 71),
           expand = FALSE)+
  ggtitle("Village-wide", subtitle = "from 1907 to 1910, gray stands for NA")+
  scale_fill_distiller(palette="BuPu", direction = 1)+
  theme_bw()

p34 <- ggpubr::ggarrange(p3,p4,ncol=2, widths = c(5,5.265))
ggpubr::annotate_figure(p34, 
                        top = ggpubr::text_grob("Cumulative number of consolidations",
                                                size=18))



shp4 <- readOGR(dsn = "1897RussianEmpire.shp", stringsAsFactors = F)
provinces_map22 <- readxl::read_xlsx("map_provinces2.xlsx")
#shp4 <- sp::merge(shp4, provinces_map22, by = "NameENG")
provinces_map22$check <- provinces_map22$province %in% reform2_sum$province
provinces_map22 <- provinces_map22 %>% filter(check == T)

ggplot() + 
  geom_polygon(data = shp4[shp4$NameENG %in% provinces_map22$NameENG,], 
               #data = shp[shp$Gub_ID <= 84|shp$Gub_ID==107 & shp$Gub_ID!=64 ,],
               aes(x = long, y = lat, group = group), 
               colour = "black", fill = NA)+
  coord_cartesian(xlim = c(20.5, 66.5),
                  ylim = c(42, 71)) +
  theme_bw()+
  geom_point(data = data %>% filter(province %in% reform2_sum$province),
             aes(x=lon, y=lat, colour = `City administrative status`))+
  theme(legend.position="bottom")+
  scale_color_brewer(palette="RdBu", direction=1)








