package de.tuhh.wb.javagis.simulation;

import java.io.File;
import java.util.Properties;
import java.util.Calendar;
import java.util.GregorianCalendar;
import java.io.FileWriter;
import java.io.IOException;
import java.io.LineNumberReader;
import java.io.FileReader;

import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.TreeMap;
import java.util.SortedMap;

import java.util.Iterator;
import java.text.SimpleDateFormat;
import java.text.DecimalFormat;
import java.text.DateFormat;
import java.text.ParsePosition;

//wadas-kalypso

import datacenter.zeitreihen.Channel;
import datacenter.zeitreihen.TimeserieWrapper;
import de.tuhh.wb.javagis.view.LogView;

public class WaterLevelSeries extends SimpleTimeSeries
{
    public WaterLevelSeries(Date minDate,Date maxDate)
    {
	super(minDate,maxDate);
    }
    
    public void store(Date date,Double value)
    {
	if(date==null)
	    System.out.println("Wrong date");
	else if(date.equals(myMinDate) || date.equals(myMaxDate) ||
		(date.after(myMinDate) && date.before(myMaxDate)))
	    {
		myTable.put(date,value);
	    }
	else
	    {
		LogView.println("     "+LogView.format(date)+" value: "+value+" ignored (date outside simulation period)");
	    }   
    }
    
    public String getSeparatorRelative()
    {
	return ",";
    }
    
    public String getDatePatternRelative()
    {
	return "D H m";
    }
    
    public void toAsciiFile(File outputFile,String separator,String datePattern,String decimalPattern)
	throws IOException
    {
	DecimalFormat decimalFormat=new DecimalFormat(decimalPattern);
 
	// header
	DateFormat dateFormat=new SimpleDateFormat("yyMMddHHmm0  0");
	String line="         "+dateFormat.format(myMinDate);

	FileWriter writer=new FileWriter(outputFile);
	dateFormat=new SimpleDateFormat(datePattern);
	
	Iterator it=myTable.keySet().iterator();
	while(it.hasNext())
	    {
		Object key=it.next();
		Object test=myTable.get(key);
		Number number=(Number)test;
		Date date=(Date)key;
		line=dateFormat.format(date)+separator+number.toString();
		writeln(writer,line);
	    }
	writer.close();
	LogView.println("  "+LogView.format(myMinDate)+" to "+LogView.format(myMaxDate)+" >"+outputFile.getPath()+"\n");
    }

    public void toAsciiFile(File outputFile)
	throws IOException
    {
	toAsciiFile(outputFile," ","dd.MM.yyyy HH:mm:ss","##########0.00");
    }
    
}
