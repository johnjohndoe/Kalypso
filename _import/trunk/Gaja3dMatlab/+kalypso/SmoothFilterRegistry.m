classdef SmoothFilterRegistry < containers.Map
    methods
        function this = SmoothFilterRegistry()
            keys = {'none', 'gaussian', 'bilateral'};
            values = {kalypso.NoSmoothFilter, kalypso.GaussianFilter, kalypso.BilateralFilter};
            this = this@containers.Map(keys, values);
        end
    end
end

