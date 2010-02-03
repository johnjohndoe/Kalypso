function [ cline ] = contour2curve( c )
%CONTOUR2CURVE Create an array of kalypso.Curve from contour data c

    lengthc = size(c,2);
    to = 0;
    i = 0;
    while(to < lengthc)
        idx = to + 1;
        pcount = c(2,idx);
        from = idx + 1;
        to = idx + pcount;
        if(pcount <= 1)
            continue;
        end
        i = i + 1;
        cline(i) = kalypso.Curve(c(1,from:to),c(2,from:to));
        cline(i).contour = c(1,idx);
    end
end

