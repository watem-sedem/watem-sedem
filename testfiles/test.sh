set -e
rm -rf testfiles/molenbeek/modeloutput
rm -rf testfiles/molenbeek/modeloutput_sdat
cn_ws/cn_ws testfiles/molenbeek/modelinput_sdat/ini_molenbeek_scenario_1_sdat.ini
cn_ws/cn_ws testfiles/molenbeek/modelinput/ini_molenbeek_scenario_1.ini
