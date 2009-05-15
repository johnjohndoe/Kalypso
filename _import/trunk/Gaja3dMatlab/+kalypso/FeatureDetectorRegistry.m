classdef FeatureDetectorRegistry < containers.Map
    methods
        function this = FeatureDetectorRegistry()
            keys = {'none', 'simple'};
            values = {kalypso.NoFeatureDetector, kalypso.SimpleFeatureDetector};
            this = this@containers.Map(keys, values);
        end
    end
end
