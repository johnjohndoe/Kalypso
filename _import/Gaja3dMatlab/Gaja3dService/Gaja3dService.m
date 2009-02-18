function Gaja3dService(varargin)
    %set Java classpath
    javaaddpath jts-1.8.jar; %Java Topology Suite
    javaaddpath gaja3d-1.0.jar; %Java helper functions
    javaaddpath deegree2.jar; %deegree2 API
    javaaddpath jai_codec.jar;
    javaaddpath jai_core.jar;
    javaaddpath geotiff-jai.jar;
    javaaddpath vecmath.jar;
    
    % call Gaja3D in batch mode
    org.kalypso.gaja3d.matlab.Gaja3D(varargin{:});
    
    disp('Done.');
end
