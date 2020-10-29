=========
Changelog
=========

version 2.0.0
-------------

Version used for calibration with DHMVII.

- Add calculation of cumulative river sediment
- Don't write empty routing lines in routing.txt, use tab seperation
- Use proportion for upstream edges

version 1.4.0
-------------

This version was made for the third steering group of the 'calibration DHMVII
project'.

- remove name of ini-file from files-section in ini-file
- simplify code
- add option 'Buffer reduce area'
- enabling range checking to avoid errors
- PTEF is 100-based
- Remove sediment trapping in open water (-5 in parcel map) pixels and assign
  ktc 9999 to those pixels
- bugfix to prevent out of range when calculating adjusted slope
- Change ktc values to float (previously integer) and adapt calibration
  accordingly
- Skip ktc low values higher than ktc high in calibration mode
- Adding Force Routing option
- Don't route cells without lower cells to themselve
- Improved error message when input directory is missing
- Add River Routing option
- Change default value of Create ktc map to True
- Refactoring code

version 1.3.0
-------------

This version was made for the second steering group of the 'calibration DHMVII
project'.

- Use adjusted slope calculation in LS calculation
- Don't calculate slope and aspect twice
- Add -9999 as no data value in all output rasters
- Improve memory allocation for rasters
- Refactoring code to read idrisi rasters

version 1.2.0
-------------

This version was made for the first steering group of the 'calibration DHVMII
project'.

- raise exception when ini-file does not exist
- make key words in ini-file of LT version the same as in other version
- bugfix: distance calculation near buffers
- write routing table as output of console version
- raise exception if no outlet is present in outletmap
- add search radius to ini-file (make it a variable, not a constant)
- add calibration method (loop over all combinations of ktc low and ktc high in
  a certain amount of steps and write output to calibration.txt)
- add functionality to use different L (Desmet1996_Vanoost2003 and
  Desmet1996_McCool) and S models (Desmet1996, Nearing1997)
- return non-zero on unsuccesful exit
- refactoring and cleaning code
- amount of sediment at outlet is now calculated as sum of incoming sediment
  in the segment that contains the outlet

version 1.1.0
-------------

- three code bases (gui, console and long-term version) are merged to a
  common code base
- add function to write routing table
- correct distance calculation
- improved exception handling
- cleaning code

Version 1.0.0
-------------

Initial version of CN-WS, as developed at KU Leuven.
