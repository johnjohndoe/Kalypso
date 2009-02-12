
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.SimpleTimeZone;
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
 * TODO To change the template for this generated type comment go to Window -
 * Preferences - Java - Code Style - Code Templates
 */
public class Zeitschleife
{
    private Date myStart;

    private Date myEnd;

    private int myZeitschrittMinuten;

    //public Zeitschleife(Date start, Date end, int zeitschrittStunden)
    public Zeitschleife(Date start, Date end, int zeitschrittMinuten)
    {
        myStart = start;
        myEnd = end;
        myZeitschrittMinuten = zeitschrittMinuten;
    }

    public void run(TeilgebietsSchleife teilgebietsSchleife)
    {
        DateFormat dateFormat2 = new SimpleDateFormat("yyyy MM dd HH:mm:ss");
    	long start=myStart.getTime();
        long end=myEnd.getTime();
        //long step=((long)myZeitschrittStunden)*60*60*1000;
     long step=((long)myZeitschrittMinuten)*60*1000;
        
        for(long time=start ; time<=end ; time+=step)
        {

        	SimpleTimeZone timeZone=new SimpleTimeZone(0,"nSWZ");
            timeZone.getDSTSavings();
            Calendar calendar=Calendar.getInstance(timeZone);
            dateFormat2.setCalendar(calendar);
        	Date datum = new Date(time);
        	
        	//System.out.println("Datum Zeit-Schleife: " +datum+" ");
        	
        	//System.out.println("Berechnungs Zeitpunk: "+dateFormat2.format(datum));
        	teilgebietsSchleife.run(time);                       
        
        }
    }
}