function saveContoursAsShape( filename, cline )

 cshp = cline.asGeostruct;
 for i=1:numel(cshp)
     cvalue = cline(i).CONTOUR;
     if(isempty(cvalue))
         cvalue = NaN;
     end
     cshp(i).CONTOUR = cvalue;
 end
 
 % write shape file with id in dBase table
 shapewrite(cshp, filename, 'DbfSpec', makedbfspec(cshp));
end