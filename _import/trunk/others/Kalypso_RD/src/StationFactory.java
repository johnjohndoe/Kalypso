import java.io.File;
import java.util.HashMap;
//import org.deegree.graphics.FeatureLayer;
//import org.deegree.model.feature.Feature;
import org.deegree.model.geometry.GM_Position;

/*
 * Created on 29.07.2004
 *
 * TODO To change the template for this generated file go to
 * Window - Preferences - Java - Code Style - Code Templates
 */

/**
 * @author kraessig
 *
 * TODO To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Style - Code Templates
 */
public class StationFactory
{
    
    private static StationFactory myInstance=new StationFactory();
    private final HashMap myStations;
    private static final String DIR_ZEITREIHEN = "data/Zeitreihe/";    
    private StationFactory()
    {
     myStations=new HashMap();    
    }
    public static StationFactory getInstance()
    {
        return myInstance;
    }
    
    public void setStationPos(String dwdkey, GM_Position pos){
        if( ! myStations.containsKey(dwdkey))
            ((Station) myStations.get(dwdkey)).setStationPos(pos);
    }//setStationPos
    
    public Station getStation(String dwdkey)
    {       
        if( ! myStations.containsKey(dwdkey))
            myStations.put(dwdkey,new Station(new File(DIR_ZEITREIHEN+dwdkey+".txt")));
        return (Station) myStations.get(dwdkey);
       
    }
  
    
    
    
}
