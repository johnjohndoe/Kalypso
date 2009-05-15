classdef EdgeFilterRegistry < containers.Map
    methods
        function this = EdgeFilterRegistry()
            OOD = [1 sqrt(2) 1; 0 0 0; -1 -sqrt(2) -1] / (4 + 2 * sqrt(2));
            SOBEL = fspecial('sobel') / 8;
            PREWITT = fspecial('prewitt') / 6;
            
            keys = {'ood', 'sobel', 'prewitt'};
            values = {OOD, SOBEL, PREWITT};
            this = this@containers.Map(keys, values);
        end
    end
end

