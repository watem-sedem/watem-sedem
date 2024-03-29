.. _calibration:

###########
Calibration
###########

This chapter describes the calibration of :ref:`WaTEM/SEDEM <WS>`. In an efficient
and semi-automated way.

Theoretical background
======================

WaTEM/SEDEM can be calibrated for a specific study area by comparing the
calculated sediment fluxes to the rivers with the observed sediment fluxes in
a number of catchments. For every catchment in the calibration dataset an 
observed/measured sedimentflux must be present.
In the model, only the ktc parameters can be choosen freely,
and thus be used as calibration parameter. 
The ktc values only affect the :ref:`transport capacity <tcmodel>` (TC)
calculated by the WaTEM/SEDEM model.
Therefor, this parameter can be adapted in order to calibrate the model.

In the model runs, two ktc values are used, namely: ktc-low and ktc-high.
The first value, ktc-low, is used for land covers with low erosion potential
(i.e. forest, pasture and grass strips), the latter, ktc-high, is used for land
covers with high erosion potential (i.e. agricultural fields). Land covers with
no erosion potential (i.e. infrastructure, rivers and open water) are
automatically appointed with a very high ktc value (i.e. 9999).

In order to select the correct ktc values for a specific study area,
WaTEM/SEDEM must be ran for a range of ktc values for all measurement areas in
the dataset. The optimal combination of both ktc values is obtained by evaluating three
criteria. This evaluation is done by the modeler in an external analysis and is not 
in the scope of this documentation. 

The first criterium in the selection process is the calculation of the
model efficiency :math:`ME`, defined by Nash and Sutcliffe (1970) as:

.. math::
    ME = 1 - \frac{\sum_{i}^{n}(E_{obs,i}-E_{sim,i})^2}{\sum_{i}^{n}(E_{obs,i}-E_{avg})^2}

with

- :math:`E_{obs,i}`: the observed sediment export for measurement point :math:`i`
- :math:`E_{sim,i}`: the simulated sediment export for measurement point :math:`i`
- :math:`E_{avg}`: the average observed sediment export of all :math:`n` measurement points

The observed sediment export is typically determined by field measurements. 
In these field measurements, the total sediment load for an event and/or a total year is
determined. It is important to note that the observations from the field measurements 
should be processed (by the user) in a way they are representative for the sediment load
simulated by WaTEM/SEDEM. The simulated sediment load is the simulated load at a pixel
(cfr. SediExport-raster) overlapping with the location of field measurements.

Model efficiencies can vary between :math:`-\infty`  and 1. An :math:`ME` value smaller than
zero means that the model is not efficient, i.e. the model delivers a result
that is less accurate than the mean value of the observed values. An :math:`ME` value
of 1 can be interpreted as perfect model.

All calculated :math:`ME` values for the different ktc-combinations can be visualised
in a plot as shown by Van Rompaey et al. (2001).

.. figure:: _static/png/plot_ME_calibration.png
    :width: 300px
    :align: center

    Model efficiencies for different combinations of ktc-low and ktc-high (Van Rompaey et al., 2001)

Model simulations with a combination of ktc values with a high :math:`ME` value are then
further analysed. In the second criterium, the slope of the linear regression (with intercept 0)
between the observed and simulated values is being analyzed. The simulated sediment export is considered
'good' if the calculated slope lies between 0.95 and 1.05. If not, the
bias between model result and observation is considered too high (i.e. systematically more
than 5% too high or too low).

It should be noted, however, that in areas with a predominant
land cover (e.g. dominant cropland), only one ktc-value if often calibrated with
a high ME, whilst the ME vs KTC-graph for the other value is rather flat (see also image above).
In that case, a third and last criterium can be used, which examines the ratio between
ktc-low and ktc-high. Multiple calibrations using different
input data for river catchments in Flanders, Belgium, showed that this ratio typically
lies between 0.25 and 0.35. Verstraeten et al. (2006) showed that when a ratio in
this range is used, the simulated effect of grass buffer strips is regarded to be equal
to the measured effectiveness of this type of erosion control measure,
thus showing that the model is capable of predicting the impact of this type of soil and
water conservation measure whereby differences in vegetation cover are crucial.
However, in other environments, where the contrast in erosion potential between vegetated
and non-vegetated land use classes is different compared to that between cropland and
grassland/forest in a temperate region like Belgium,
a different ratio between ktc-high and ktc-low can be sought for.
Local insights into the different erosion potential may help in this respect.


Practical execution
===================

WaTEM/SEDEM has a built-in calibration tool. This tool is an extenstion develloped
during the work of Deproost et al. (2018). To use this tool, the user has to
create a set of input rasters for every catchment in the calibration dataset and
has to define all the :ref:`options <choicespage>` that are needed for the
calibration and the future model runs. In the ini-file for every catchment that should
be calibrated, the user
has to enable the :ref:`calibration option <calibrate>` and define the
:ref:`range of ktc values <calibrationparamters>`.

The model will then loop over all combinations of ktc values in the defined range.
First, a :ref:`ktc map <ktcmap>` is created by the model for every ktc combination.
Next, the full WaTEM/SEDEM model is run for all these combinations, for all the given
catchments. Finally, a :ref:`calibration file <calibrationtxt>` with the amount of
sediment at each outlet of the model, for each combination of ktc values in the defined
range is available for every catchment. These
files can be processed by the user, through e.g. a python script, to calculate the :math:`ME` and
the other criteria, mentioned above, in order to select the best set of ktc-values for the study area.

References
==========
Deproost, P., Renders, D., Van de Wauw, J., Van Ransbeeck, N.,
Verstraeten, G., 2018, Herkalibratie van WaTEM/SEDEM met het DHMV-II als
hoogtemodel: eindrapport. Brussel. (in Dutch)
https://archief.onderzoek.omgeving.vlaanderen.be/Onderzoek-1812384

Nash, J. E.; Sutcliffe, J. V. (1970). "River flow forecasting through conceptual
models part I — A discussion of principles". Journal of Hydrology. 10 (3):
282–290. https://doi.org/10.1016/0022-1694(70)90255-6

Verstraeten, G., Poesen, J., Gillijns, K., & Govers, G. (2006). The use of
riparian vegetated filter strips to reduce river sediment loads: an overestimated
control measure?. Hydrological Processes: An International Journal,
20(20), 4259-4267. https://doi.org/10.1002/hyp.6155
