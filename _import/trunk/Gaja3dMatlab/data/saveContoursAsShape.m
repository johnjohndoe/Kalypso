function saveContoursAsShape( filename, cline )

 cshp = cline.asGeostruct;
 for i=1:numel(cshp)
     cshp(i).CONTOUR = cline(i).contour;
 end
 
 % write shape file with id in dBase table
 shapewrite(cshp, filename, 'DbfSpec', makedbfspec(cshp));
end

