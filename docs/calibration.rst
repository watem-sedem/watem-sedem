.. _calibration:

###########
Calibration
###########

.. note::
    This chapter only describes the calibration of :ref:`WaTEM/SEDEM <WS>`. The
    :ref:`CN module <CN>` can be calibrated using the parameters
    :ref:`alpha <alpha>` and :ref:`beta <beta>`. We refer to the literature
    about the Curve Number methodology on how to use these parameters.

Theoretical background
======================

WaTEM/SEDEM can be calibrated by comparing the calculated sediment fluxes to the
rivers with the observed sedimentfluxes in :math:`n` catchments.
The :ref:`transport capacity <tcmodel>` used in WaTEM/SEDEM contains the ktc-value.
This parameter can be adapted to calibrate the model.

Two kTC-values are used in the model runs: kTC-low and kTC-high. The first value
is used for land covers with low erosion potential (i.e. forest, grassland), the
latter, kTC-high, is used for arable land. Land covers with no erosion potential
(i.e. roads and water) are automatically appointed with a very high kTC value (i.e. 9999).

To select the correct kTC-values,
WaTEM/SEDEM must be ran for a range of kTC values for all measurement areas in
the dataset. The optimal combination of both kTC-values is obtained by three criteria.

The first step in the selection process is the calculation of the
model efficiency :math:`ME` defined by Nash and Sutcliffe (1970):

.. math::
    ME = 1 - \frac{\sum_{i}^{n}(SE_{obs,i}-SE_{sim,i})^2}{\sum_{i}^{n}(SE_{obs,i}-SE_{avg})^2}

with

- :math:`SE_{obs,i}`: the observed sediment export for measurement point :math:`i`
- :math:`SE_{sim,i}`: the simulated sediment export for measurement point :math:`i`
- :math:`SE_{avg}`: the average observed sediment export of all :math:`n` measurement points

Model efficiencies vary between :math:`-\infty`  and 1. An :math:`ME` smaller than
zero means that the model is not efficient, i.e., the model delivers a result
that is less accurate than the mean value of the observed values. A :math:`ME`
of 1 can be interpreted as a very performant model.

All calculated :math:`ME` for the different kTC-combinations can be visualised
in a plot as shown by Deproost et al. (2018).

.. figure:: _static/png/plot_ME_calibration.png
    :width: 300px
    :align: center

    Model efficiencies for different combinations of kTC-low and kTC-high (Deproost et al., 2018)

Model simulations with a combination of kTC-values with a high :math:`ME` are
further analysed. Two criteria are used to select the best performing kTC-factors.
The first criterium is the slope of the linear regression (with intercept 0)
between the observed and simulated values. The simulated sediment export is
good if the calculated slope lies between 0.95 and 1.05. Ohterwise, the
bias between modelresult and observation will be to high (systematically more
than 5 % too high or too low).

The last criterium is the ratio between kTC-low and kTC-high. Verstraeten et al.
(2006) showed that this ratio must lie between 0.25 and 0.35. When a ratio in
this range is used, the simulated effect of gras buffer strips equals the measured
effectivity of this type of erosion control measure.

Practical execution
===================

CN-WS has a built-in calibration tool for WaTEM/SEDEM. First, the user has to
make a set of input rasters for every catchment in the calibration dataset and
has to define all the :ref:`options <choicespage>` that need to be used in the
calibration and future model runs. In the ini-file for every catchment the user
has to enable the :ref:`calibration option <calibrate>` and define the
:ref:`range of ktc values <calibrationparamters>`.

The model will loop over all combinations of ktc values in the defined range.
A :ref:`ktc map <ktcmap>` is created by the programm for every combination.
Afterwards, the model is run for every combination.
:ref:`A calibration file <calibrationtxt>` with the amount of
sediment at each
outlet of the model, for each combination of ktc-values in the defined range is
available after running the model in calibration mode for every catchment. These
files can be processed with e.g. a python script to calculate the :math:`ME` and
the other criteria needed to select the best set of ktc-values.

References
==========
Deproost, P., Renders, D., Van de Wauw, J., Van Ransbeeck, N.,
Verstraeten, G., 2018, Herkalibratie van WaTEM/SEDEM met het DHMV-II als
hoogtemodel: eindrapport. Brussel.
https://archief.onderzoek.omgeving.vlaanderen.be/Onderzoek-1812384

Nash, J. E.; Sutcliffe, J. V. (1970). "River flow forecasting through conceptual
models part I — A discussion of principles". Journal of Hydrology. 10 (3):
282–290. https://doi.org/10.1016/0022-1694(70)90255-6

Verstraeten, G., Poesen, J., Gillijns, K., & Govers, G. (2006). The use of
riparian vegetated filter strips to reduce river sediment loads: an overestimated
control measure?. Hydrological Processes: An International Journal,
20(20), 4259-4267. https://doi.org/10.1002/hyp.6155