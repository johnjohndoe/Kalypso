
import java.io.File;
import java.io.FileReader;
import java.io.LineNumberReader;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.SimpleTimeZone;
import java.util.TimeZone;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/*
 * Created on 11.08.2004
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
public class ZeitreihenFactory
{
    private static String aInteger = "([-|+]?[0-9]+)";

    private static String aFloat = "([-|+]?[0-9]+\\.[0-9]+)";
    private static DateFormat dateFormat = new SimpleDateFormat("yyyy_MM_dd HH:mm:ss");

    //1975_01_02 00:00:00;000000000000.000;I
    private static String aDate = "([0-9|_|:|\\s]+)";

    //private static Pattern pattern = Pattern.compile( aDate + ";"
    //      + aFloat + ";I");
   private static Pattern pattern = Pattern.compile( aDate + ";"
            + aFloat);

    public static Zeitreihe createZeitreihe(File file, long time)
    {
     System.out.println("lese Zeitreihe...");
        Zeitreihe zr=new Zeitreihe();
        LineNumberReader lineReader;
        try
        {
            lineReader = new LineNumberReader(new FileReader(file));
            
            String line = null;
            Matcher m = null;
            while ((line = lineReader.readLine()) != null)
            {             
                m = pattern.matcher(line);
                if (m.matches()) 
                {
                    String dateString = m.group(1);
                    String valueString = m.group(2);
                    
                    
                    SimpleTimeZone timeZone=new SimpleTimeZone(0,"nSWZ");
                    timeZone.getDSTSavings();
                    Calendar calendar=Calendar.getInstance(timeZone);
                    dateFormat.setCalendar(calendar);
                    
              
                    Date date= dateFormat.parse(dateString);
                    double value=Double.parseDouble(valueString);
                    zr.add(date,value);
                }
            }
        }
        catch (Exception e)
        {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
        System.out.println("lese Zeitreihe... done");
        return zr;
    }

}