import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.TimeZone;

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
public class TeilgebietsSchleife
{
    private Teilgebiet[] myTeilgebiete;

    public TeilgebietsSchleife(Teilgebiet[] teilgebiete)
    {
        myTeilgebiete=teilgebiete;
    }
    
    public void run(long time)
    {

    	Date datum=new Date(time);
    	
        //System.out.print("Datum TG-Schleife:   "+datum +" ");
        for(int i=0;i<myTeilgebiete.length;i++)
        {
          //SK wieder in code
        	double tgNiederschlag=myTeilgebiete[i].getNiederschlag(time);
            myTeilgebiete[i].generateOutput(time);            
        }        
    }
    
    
}
