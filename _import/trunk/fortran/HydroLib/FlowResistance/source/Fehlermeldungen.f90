module Fehlermeldungen

1001 format (1X, 'One or more parameters are missing.'/ &
           & 1X, 'Please enter a valid value for all non-optional Parameters. ')

1002 format (1X, 'The value for lambda has been set to 1000'/ &
           & 1x, 'due to an even higher value from the calculation.'/ &
           & 1x, 'Please check all parameter values.')

1003 format (1X, 'The Diameter of trees is larger than the distance between trees.'/ &
           & 1X, 'Please enter a valid value for both diameter of trees (dp) and '/ &
           & 1X, 'distance between trees (a).')

1004 format (1X, 'The diameter of the roughness elements (dm) is to small to get acceptable results.'/ &
           & 1X, 'You should think about using another calculation method.'/ &
           & 1X, 'For example the COLEBROOK-WHITE formular with the equivelent sand roughness (ks).')

1005 format (1X, 'The diameter of the roughness elements (dm) is to big to get acceptable results.'/ &
           & 1X, 'You should think about using another calculation method.'/ &
           & 1X, 'For example the calculation of the DARCY-WEISBACH coefficient lambda by using' / &
           & 1x, 'the Formwiderstand of cylinders.')

1006 format (1X, 'There are wether no plants or the diameter of trees and branches is to small to  '/ &
           & 1X, 'calculate the DARCY-WEISBACH coefficient lambda by using the Formwiderstand of cylinders.')

1007 format (1X, 'The vegetaiontype has not been set correctly.'/ &
           & 1X, 'You can wether choose living grass (vegType = 1)'/ &
           & 1X, 'or dead grass (vegType = 2)')

1008 format (1X, 'The vegetaiontype has not been set correctly.'/ &
           & 1X, 'You can wether choose living grass (vegType = 1)'/ &
           & 1X, 'or dead grass (vegType = 2)')

1009 format (1X, 'The vegetaiontype has not been set correctly.'/ &
           & 1X, 'You can wether choose living grass (vegType = 1)'/ &
           & 1X, 'or dead grass (vegType = 2)')

end module
