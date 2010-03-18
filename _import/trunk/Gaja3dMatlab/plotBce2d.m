function [handle] = plotBce2d( points, arcs, elements )
    elementcount = size(elements,1);
    arccount = size(arcs,1);
    faces = zeros(elementcount, 4) * NaN;
    elementnodecount = zeros(elementcount, 1);
    for i=1:arccount
        elementleft = arcs(i,3);
        if(elementleft > 0)
            arc = arcs(i,1:2);
            if(elementnodecount(elementleft)==0)
                elementnodecount(elementleft) = elementnodecount(elementleft) + 2;
                faces(elementleft,1:2) = arc;
            elseif(elementnodecount(elementleft)<4)
                if(faces(elementleft,elementnodecount(elementleft))==arc(1))
                    elementnodecount(elementleft) = elementnodecount(elementleft) + 1;
                    faces(elementleft,elementnodecount(elementleft)) = arc(2);
                elseif(faces(elementleft,1)~=arc(2))
                    elementnodecount(elementleft) = 4;
                    faces(elementleft,3:4) = arc;
                end
            end
        end
        elementright = arcs(i,4);
        if(elementright > 0)
            arc = arcs(i,[2 1]);
            if(elementnodecount(elementright)==0)
                elementnodecount(elementright) = elementnodecount(elementright) + 2;
                faces(elementright,1:2) = arc;
            elseif(elementnodecount(elementright)<4)
                if(faces(elementright,elementnodecount(elementright))==arc(1))
                    elementnodecount(elementright) = elementnodecount(elementright) + 1;
                    faces(elementright,elementnodecount(elementright)) = arc(2);
                elseif(faces(elementright,1)~=arc(2))
                    elementnodecount(elementright) = 4;
                    faces(elementright,3:4) = arc;
                end
            end
        end
    end
    faces(isnan(faces(:,1)),:) = [];
    FV.vertices = points(:,1:3);
    FV.faces = faces;
    handle = patch(FV, 'FaceVertexCData', points(:,3), 'FaceColor','interp','EdgeColor','black');
end

