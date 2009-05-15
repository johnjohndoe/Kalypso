function set_classpath( )
%SET_CLASSPATH Sets the required Java classpath and Matlab dynamic path

    %set Java classpath
    javaaddpath gaja3d-1.0.jar %Java helper functions    
    javaaddpath jts-1.10.jar %Java Topology Suite
    javaaddpath deegree2.jar %deegree2
    javaaddpath jai_codec.jar % Java Advanced Imaging
    javaaddpath jai_core.jar % Java Advanced Imaging
    javaaddpath geotiff-jai.jar % Java Advanced Imaging (Geotiff support)
    javaaddpath vecmath.jar;
    javaaddpath log4j-1.2.9.jar % Java Advanced Imaging (Geotiff support)
    
    % Matlab dynamic path
    addpath data %data import/export functions
    addpath utils %geographic utility functions    
    %addpath mlunit/src % unit testing framework
    %addpath test %unit testing functions
end
