/*
 * Created on 29.07.2004
 *
 * TODO To change the template for this generated file go to
 * Window - Preferences - Java - Code Style - Code Templates
 */
import java.io.IOException;
import java.io.LineNumberReader;
import java.io.Reader;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;


import org.deegree.model.geometry.GM_Position;
import org.deegree_impl.model.geometry.GeometryFactory;
/**
 * @author kraessig
 *
 * TODO To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Style - Code Templates
 */
public class BlockFactory
{
    
    private static String aInteger="([-|+]?[0-9]+)";
    private static String aFloat="([-|+]?[0-9]+\\.[0-9]+)";
   
  
    private final static Pattern header = Pattern.compile(" BLOCK:\\s+?"+aFloat+"\\s+?"+aFloat+"\\s+?"+aFloat+"\\s+?"+aInteger+"\\s+?"+aInteger+"\\s+?"+aInteger);
    
    //  Pattern zum Einlesen von Stationskoordinaten und Gewichten
    private final static Pattern row = Pattern.compile(" "+aFloat+"\\s+?"+aFloat+"\\s+?"+aFloat+"\\s+?"+aInteger);
    
         public static Block[] createBlocks(Reader reader) throws IOException  
        {               
        
        List result=new ArrayList();
        LineNumberReader lineReader=new LineNumberReader(reader);
            String line=null;
            Matcher m=null;
            Block actualBlock=null;
            
            while((line=lineReader.readLine())!=null)
            {
                m = header.matcher(line); 
                // group: 1,2,3 Zuordnung
                if(m.matches()) // z.B. " BLOCK:  7 1 7"
                {
                 double cx= Double.parseDouble(m.group(1));
                 double cy= Double.parseDouble(m.group(2));    
                 
                 GeometryFactory geoFac=new GeometryFactory(); 
                 
                  GM_Position pos=geoFac.createGM_Position(cx,cy);
                    // Area [m²]
                    String n= m.group(3);
                    String blx= m.group(4);
                    String bly= m.group(5);
                    String blnr= m.group(6);
                    
                    actualBlock=new Block(pos);
                    result.add(actualBlock);
                }
                m = row.matcher(line);     
                if(m.matches()) // z.B. " 3528330.000 5962970.000 0.032 955555"
                {
                    double x= Double.parseDouble(m.group(1));
                    double y= Double.parseDouble(m.group(2));
                    GeometryFactory geoFac=new GeometryFactory();
                    GM_Position contStation = geoFac.createGM_Position(x,y);
                    
                    double w= Double.parseDouble(m.group(3));
                    String dwdkey =m.group(4);
                    
                    actualBlock.add(StationFactory.getInstance().getStation(dwdkey),w);
                    StationFactory.getInstance().getStation(dwdkey).setStationPos(contStation);
                }
            }
        return (Block[]) result.toArray(new Block[result.size()]);
    }           
}
