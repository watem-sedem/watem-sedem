set -e
rm -r testfiles/molenbeek/modeloutput
rm -r testfiles/molenbeek/modeloutput_sdat
cn_ws/cn_ws testfiles/molenbeek/modelinput_sdat/ini_molenbeek_scenario_1_sdat.ini
cn_ws/cn_ws testfiles/molenbeek/modelinput/ini_molenbeek_scenario_1.ini
