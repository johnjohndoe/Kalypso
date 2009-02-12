import java.io.File;
import java.util.Date;
//import org.deegree.model.geometry.GM_Object;
import org.deegree.model.geometry.GM_Position;

/*
 * Created on 27.07.2004
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
public class Station
{
   // private final GM_Position myPositionLZ;
    
    private GM_Position stationPosition = null;
    private final File myDataFile;    
    private Zeitreihe myZeitreihe=null;
  
    
    public Station(File dataFile)
    {
      this.myDataFile=dataFile;      

    }

    public void setStationPos( GM_Position pos ){
        this.stationPosition = pos;
        
    }//setStationPos
    
    public double getNiederschlag(long time)
    {
        if(myZeitreihe==null)
            myZeitreihe=ZeitreihenFactory.createZeitreihe(myDataFile,time);

        // TODO
        Date date=new Date(time);
        return myZeitreihe.getValueAt(date,date);
    }    
    public String toString(){
        String str = null;
        str = myDataFile.getName() + myZeitreihe.toString();
        return null;
    }//end toString
}
