
import java.util.ArrayList;
import java.util.List;
//import java.lang.Double;
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
 * TODO To change the template for this generated type comment go to Window -
 * Preferences - Java - Code Style - Code Templates
 */
public class Block
{
    private final List myStations;

    private final GM_Position myPosition;
    
    public Block(GM_Position position)
    {
        myStations = new ArrayList();
        myPosition=position;
    }
    //Methode
    public void add(Station station, double wichtung)
    {
        myStations.add(new StationWichtung(station, wichtung));
        //myStations.add(new StationWichtung1(station, wichtung));
    }

    public double getNiederschlag(long time)
    {
        
        
//      SK0305 Wichtung bei Stationsausfall -99
    	double result = 0;
    	
    	
    	 for (int i = 0; i < myStations.size(); i++)
         {
    	double nbcorr = 0;		
    	double Fak_nbcorr=0;
    	double Fak_nbcorr2=0;
    	int c=0;
    	int count=0;
    	for (int k = 0; k < myStations.size(); k++)
        {
    		
        	StationWichtung s=(StationWichtung) myStations.get(k);
            if (s.myStation.getNiederschlag(time)==-99.0)
            	{
            	//System.out.println("Restesting Abfrage Station: "+k+"     "+s1.myWichtung );
            	
            	nbcorr += (s.myWichtung);
                count=count+1;
            	//System.out.println("Restesting Abfrage Station: "+k+"     "+s1.myWichtung );
            	}
        }
        
    	if (count>0)
    	{
        //SK0305 Wichtung bei Stationsausfall

        	StationWichtung s1=(StationWichtung) myStations.get(i);
            //bestand 0305
        	//result += s.myWichtung * s.myStation.getNiederschlag(time);
            //bestand 0305
        	
        	if(s1.myStation.getNiederschlag(time)!=-99.0){
        		
        		
        		   
        		Fak_nbcorr=(nbcorr/(1-nbcorr)); 
        		Fak_nbcorr2 = (Fak_nbcorr*(s1.myWichtung))+(s1.myWichtung);
        		
        		if(nbcorr>0.99999999999)
            		{nbcorr=0.5;
            		Fak_nbcorr2 = ((nbcorr/(1-nbcorr))*(s1.myWichtung));
            		}
        		
        		if(Fak_nbcorr<0)
        			Fak_nbcorr2=1;
        		
        		if(nbcorr==1)
        			Fak_nbcorr2=1;
        		
        		if(count==(myStations.size()-1))
        			Fak_nbcorr2=1;
        		System.out.println("tesing");
            	{
            	c=c+1;
            	if (c==1)
            	{if(s1.myWichtung==0)
            	Fak_nbcorr2=1;	
            	}	
            	}
 		
        		
            	//System.out.println(Fak_nbcorr2 +"     "+s1.myStation.getNiederschlag(time));
        		result += (Fak_nbcorr2) * s1.myStation.getNiederschlag(time);
        		//if(result!=5.8)
        			//System.out.println("!=5.8!!!!!"+count);
        		//System.out.println("Restesting Abfrage Station2 nbcorr: "+i+"     "+nbcorr+"wichtung  "+s1.myWichtung+"ncoeff   "+Fak_nbcorr2); 
        	//System.out.println("Restesting Abfrage Station3 result: "+i+"     "+result+"    "+s1.myStation.getNiederschlag(time));
        	}
    	
    	}//count>0
    	if (count==0)
    	{
    	StationWichtung s2=(StationWichtung) myStations.get(i);
    	result += s2.myWichtung * s2.myStation.getNiederschlag(time);
    	}
    	
//          SK Test der Kriging Interpolation
            //System.out.println("Res: " + result + "Wichtung:  " + s.myWichtung + "Niederschalg: " +
            //        s.myStation.getNiederschlag(time));
            
            //SK, damit keine negativen Schätzergebnisse: negative Werte druch negative Gewichte
            //werden zu null gesetzt, nicht die Gewichte
    	
    	 if (result <0)   
         result=0; 
        }
    	 
    	 
        return result;
    }

    private class StationWichtung
    {
        public final Station myStation;

        public final double myWichtung;

        public StationWichtung(Station station, double wichtung)
        {
            myStation = station;
            myWichtung = wichtung;
        }
public String toString(){
    String str = null;
    str = myStation.toString() + myWichtung;
    return str;
}// end toString
    }

    /**
     * @return
     */
    public GM_Position getPosition()
    {
        return myPosition;
    }
}