import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.text.DateFormat;
//import java.text.DecimalFormat;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.Iterator;
import java.util.SimpleTimeZone;
import java.util.SortedMap;
import java.util.TimeZone;
import java.util.TreeMap;


/*
 * Created on 11.08.2004
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
public class Zeitreihe
{
    SortedMap myData; 
    public Zeitreihe()
    {
      myData=new TreeMap();    
    }

    /**
     * @param date
     * @param value
     */
    public void add(Date date, double value)
    {
        myData.put(date,new Double(value));
    }
    
    public double getValueAt(Date date1,Date date2)
    {
        //Date min=getDateBefore(dateMin);        
        //Date max=getDateAfter(dateMin);
      	//SK kein zeitlicher Versatz!!!
    	Double value=(Double) myData.get(date2);
    	if(value == null) value = new Double(0);
        return value.doubleValue();
    }
    
    private Date getDateBefore(Date date)
    {        
        return (Date) myData.headMap(date).lastKey();
    }
    
    private Date getDateAfter(Date date)
    {
        return (Date) myData.tailMap(date).firstKey();
    }
    
    public void exportAsGrap(File outputFile)
	throws IOException
    {
        String separator=" ";
        String datePattern="dd MM yyyy HH mm ss";
        String decimalPattern="##########0.00";

        //DecimalFormat decimalFormat=new DecimalFormat(decimalPattern);
 
	// header
       
        DateFormat dateFormat=new SimpleDateFormat("yyMMddHHmm0  0");
        //dateFormat.setTimeZone(TimeZone.getTimeZone("GMT+1"));
	String line="         "+dateFormat.format(myData.firstKey());
	FileWriter writer=new FileWriter(outputFile);
	writeln(writer,"");
	writeln(writer,line);
	writeln(writer,"grap");
	dateFormat=new SimpleDateFormat(datePattern);
	
	
	SimpleTimeZone timeZone=new SimpleTimeZone(0,"nSWZ");
    timeZone.getDSTSavings();
    Calendar calendar=Calendar.getInstance(timeZone);
    dateFormat.setCalendar(calendar);
	
	
	Iterator it=myData.keySet().iterator();
	
	while(it.hasNext())
	    {
		Date date=(Date) it.next();
		Object value=myData.get(date);
		Number number=(Number)value;
		
		line=dateFormat.format(date)+separator+number.toString();
		writeln(writer,line);
	    }
	writer.close();	
    }
    
    public void writeln(FileWriter writer,String line) throws IOException
    {
	line=line+System.getProperty("line.separator");
	writer.write(line,0,line.length());
    }
}
