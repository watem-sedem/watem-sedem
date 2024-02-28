set -e
rm -rf testfiles/molenbeek/modeloutput
rm -rf testfiles/molenbeek/modeloutput_sdat
watem_sedem/watem_sedem testfiles/molenbeek/modelinput_sdat/ini_molenbeek_scenario_1_sdat.ini
watem_sedem/watem_sedem testfiles/molenbeek/modelinput/ini_molenbeek_scenario_1.ini
