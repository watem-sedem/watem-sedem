##########################
Frequently Asked Questions
##########################

This page lists a few known problems that may occur during the installation
or usage of WaTEM/SEDEM, and workarounds for them.

If your issue is not mentioned in this list, please search for the issues on
our issuetracker_, and file a new issue if it is needed.

Errors in the command line interface
====================================

Errors detected during execution are called **exceptions** and are causing the
model to stop. The last line of the error message indicates what happened.
The parser repeats the offending section and displays a little ‘arrow’ pointing
at the earliest point in the line where the error was detected.

.. _issuetracker: https://github.com/cn-ws/cn-ws/issues

My system cannot find cn_ws
===========================

**Question:** When I type `cn_ws` in my terminal it says it does not find the 
executable, e.g.

.. code-block:: bash

    C:\>cn_ws
    'cn_ws' is not recognized as an internal or external command,
    operable program or batch file.


.. code-block:: bash

    $ cn_ws
    bash: cn_ws command not found


What now?

**Answer:** Check if you have a compiled version in the subfolder cn_ws/cn_ws. 
If not, double check the steps in the :ref:`installation section <buildwindows>`. 
Also note that you have to be in the cn_ws/cn_ws subfolder to run the executable.
Alternatively you can add the subfolder to the paths in your environment (on 
windows see `here <https://docs.alfresco.com/4.2/tasks/fot-addpath.html>`).
This way you should be able to run cn_ws from any directory.