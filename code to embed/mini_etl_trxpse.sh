sed -z 's/\n\s\s/  /g' dt_trxpse_personas_2016_2018_muestra_adjt.csv > dt_trxpse_personas_2016_2018_muestra_adjt_mod_1.csv
sed -z 's/\n,/,/g' dt_trxpse_personas_2016_2018_muestra_adjt_mod_1.csv > dt_trxpse_personas_2016_2018_muestra_adjt_mod_2.csv 
sed -z 's/\n[^0-9]/ /g' dt_trxpse_personas_2016_2018_muestra_adjt_mod_2.csv > dt_trxpse_personas_2016_2018_muestra_adjt_mod_final.csv 
rm dt_trxpse_personas_2016_2018_muestra_adjt_mod_1.csv dt_trxpse_personas_2016_2018_muestra_adjt_mod_2.csv
cut -d',' -f1-11 dt_trxpse_personas_2016_2018_muestra_adjt_mod_final.csv > cut_1_11.out
cut -d',' -f12-13 dt_trxpse_personas_2016_2018_muestra_adjt_mod_final.csv > cut_12_13.out
sed -e "s/,/;/g" cut_1_11.out > cut_1_11_semicolon.out
paste -d' ' cut_1_11_semicolon.out cut_12_13.out > dt_trxpse.csv
rm dt_trxpse_personas_2016_2018_muestra_adjt_mod_final.csv cut_1_11.out cut_1_11_semicolon.out cut_12_13.out
