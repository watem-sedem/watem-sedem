###########
WaTEM-SEDEM
###########

Concept
=======

WaTEM-SEDEM is a spatially distributed model that was created at the Laboratroy for Experimental Geomorphology (KU Leuven, Belgium). 
WaTEM stands for Water and Tillage erosion model (Van Oost et al., 2000) and SEDEM is the abbreviation of Sediment Delivery Model (Van Rompaey et al., 2001). 

In WaTEM-SEDEM the mean annual soil erosion rate *E* and transport capacity *TC* are calculated for every pixel in the model domain. 
Next, the model iterates over all pixels according to the order determined by the routing algorithm. During the iteration, the outgoing sediment for every pixel is calculated 
by comparing the the total available sediment in the cell (incoming sediment + *E*) with the transport capacity.

Two cases exist:

- The total available sediment in a cell is lower than *TC*: 
	- the resulting mass balance is negative (too little sediment available to transport, so 'erosion' will occur)
	- the outgoing sediment is set equal to the available sediment
- The available sediment exceeds *TC*:
	- the resulting mass balance is positive (enough sediment to transport, some sediment will be 'deposited' in the cell)
	- the outgoing sediment is equal to *TC* and (available sediment - TC) will be deposited in the cell. 

The outgoing sediment of a cell is distributed to one or two target pixels. The target cells are determined by the routing algorithm.
The outgoing sediment of pixel X to pixel Y is added to the incoming sediment of pixel Y. Pixel Y can receive sediment of multple pixels.

This process is illustrated in figure (TO DO: create figure with pixel).

Mean annual soil erosion rate
=============================

For every pixel in the model domain or catchment, the mean annual soil erosion rate is calculated with an adapted version of the RUSLE (Revised Universal Soil Loss Equation, Renard et al., 1997). 
The mean annual soil erosion rate *E* (kg/m²/year) is calculated by

	*E = R.K.LS.C.P*
	
Where:

- *R*: rainfall erosivity factor (MJ mm m-² h-1 a-1)
- *K*: soil erodibility factor (ton 
- *LS*: topgographical slope and length factor
- *C*: crop erosivity factor
- *P*: erosion control factor

A detailed description of these factors is given :ref:`here <ruslefactors>`.


Transport capacity calculation
==============================

For every grid cell the transport capacity *TC* (kg/m/year)  is calculated by
	
	*TC = kTC.R.K.(LS - 4.12...)*
	
Where:

- *kTC*: calibration factor
- *R*: rainfall erosivity factor (MJ mm m-² h-1 a-1)
- *K*: soil erodibility factor (ton 
- *LS*: topgographical slope and length factor

A detailed description of these factors is given :ref:`here <ruslefactors>`.


Tillage erosion
===============

TO DO, see Van Oost et al. 2000.

.. _ruslefactors:

RUSLE factors
=============

In this paragraph the different parameters of the RUSLE equation (Renard et al., 1997) are described.

.. _rfactor:

R-factor
########

TO DO

.. _kfactor:

K-factor
########

TO DO

.. _lsfactor:

LS-factor
#########

TO DO

.. _cfactor:

C-factor
########

TO DO

.. _pfactor:

P-factor
########

TO DO


References
==========

* Renard K.G., Foster G.R., Weesies G.A., McCool D.K., Yoder D.C. (1997) Predicting soil erosion by water: a guide to conservation planning with the Revised Universal Soil Loss Equation (RUSLE). USDA Agricultural Handbook 703. `link <https://www.ars.usda.gov/ARSUserFiles/64080530/RUSLE/AH_703.pdf>`_
* Van Oost, K., Govers, G. & Desmet, P.J.J (2000) Evaluating the effects of changes in the landscape structure on soil erosion by water and tillage. Landscape Ecology 15, 577-589. `link <https://doi.org/10.1023/A:1008198215674>`_
* Van Rompaey, A., Verstraeten, G., Van Oost, K. Govers, G. & Poesen, J. (2001) Modelling mean annual sediment yield using a distributed approach. Earth Surface Processes and Landforms 26(11), 1221-1236. `link <https://doi.org/10.1002/esp.275>`_
* Verstraeten, G., Van Oost, K., Van Rompaey, A., Poesen, J. & Govers, G. (2003) Evaluating an integrated approach to catchment management to reduce soil loss and sediment pollution through modelling. Soin Use and Management, 18, 386-394. `link <https://doi.org/10.1111/j.1475-2743.2002.tb00257.x>`_



