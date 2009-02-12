package de.tuhh.wb.javagis.simulation;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.text.DateFormat;
import java.text.DecimalFormat;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;

import de.tuhh.wb.javagis.view.LogView;

public class TempTimeSeries extends SimpleTimeSeries
{
    final static Double DEFAULT_TEMP=new Double(4.99);
    public TempTimeSeries(Date minDate,Date maxDate)
    {
	super(minDate,maxDate);
	myMinDate=roundToDay(minDate);
	//	myMaxDate=roundToDay(maxDate);
	store(myMinDate,DEFAULT_TEMP);
	store(myMaxDate,DEFAULT_TEMP);
    }
    
    private Date roundToYear(Date date)
    {
	Calendar calDate=new GregorianCalendar();
	calDate.setTime(roundToDay(date));
	calDate.set(Calendar.DAY_OF_YEAR,1);
	return calDate.getTime();
    }

    private Date roundToDay(Date date)
    {
	Calendar calDate=new GregorianCalendar();
	calDate.setTime(date);
	calDate.set(Calendar.HOUR_OF_DAY,0);
	calDate.set(Calendar.MINUTE,0);
	calDate.set(Calendar.SECOND,0);
	calDate.set(Calendar.MILLISECOND,0);
	return calDate.getTime();		
    }
    
    public void store(Date date,Double value)
    {
	if(date!=null)
	    {
		Date myDate=roundToDay(date);

		if(myDate.equals(myMinDate) || myDate.equals(myMaxDate) ||
		   (myDate.after(myMinDate) && myDate.before(myMaxDate)))
		    {
			myTable.put(myDate,value);
			//			System.out.println("store Date: "+myDate.toString()+" value:"+value.toString());
		    }
		else
		    {		
			LogView.println(" "+LogView.format(myDate)+"~"+LogView.format(date)+" value: "+value+" ignored (date outside simulation period)");
		    }
	    }
    }
    
    public String getSeparatorRelative()
    {
	return ",";
    }
    
    public String getDatePatternRelative()
    {
	return "D";
    }
    
    public void toAsciiFile(File outputFile,String separator,String datePattern,String decimalPattern)
	throws IOException
    {
	DecimalFormat decimalFormat=new DecimalFormat("0.0000");
	String line;
	// header
	DateFormat dateFormat=new SimpleDateFormat(datePattern);
	FileWriter writer=new FileWriter(outputFile);
	
	Calendar fileDate=new GregorianCalendar();
	Calendar fileEndDate=new GregorianCalendar();
	fileDate.setTime(roundToYear(myMinDate));
	fileEndDate.setTime(roundToYear(myMaxDate));
	fileEndDate.set(Calendar.DAY_OF_YEAR,1);
	fileEndDate.add(Calendar.YEAR,2);
	Number number;
	writeln(writer,"ex2");
	while(fileDate.before(fileEndDate))
	    {
		Date date=fileDate.getTime();
		Object valtest=myTable.get(date);
		
		if(fileDate.get(Calendar.DAY_OF_YEAR)==1)
		    {
			/*
			  writeln(writer,"#kalypso evaporation temperature file");
			  writeln(writer,"                    "+fileDate.getActualMaximum(Calendar.DAY_OF_YEAR));
			  writeln(writer,"# block of year "+fileDate.get(Calendar.YEAR));
			*/
		    }
		if(myTable.containsKey(date))
		    number=(Number)myTable.get(date);
		else
		    number=DEFAULT_TEMP;
//	        if(number.doubleValue()>0)
line=dateFormat.format(date)+formatKoma(decimalFormat.format(number)) ;//format(6,2,decimalFormat.format(number));
// 	  	else
//                 line=dateFormat.format(date)+separator+decimalFormat.format(number) ;//format(6,2,decimalFormat.format(number));

		//		line=dateFormat.format(date)+separator+decimalFormat.format(number);
		writeln(writer,line);
		//		System.out.println(line);
		fileDate.add(Calendar.DAY_OF_YEAR,1);
	    }
	writer.close();	
	LogView.println("  "+LogView.format(myMinDate)+" to "+LogView.format(myMaxDate)+" >"+outputFile.getPath()+"\n");

	//	System.out.println("wrote temperatue-file: "+outputFile.getPath());
    }

	public String formatKoma(String text)
{
return text.replace(',','.');
}
    public String format(int max,int right,String text)
    {
	//	System.out.println("vorher:"+text);
	text=text.replace(',','.');
	//	System.out.println("nach replace:"+text);
	int trim=text.indexOf(".");
	for(int i=0;i<max-trim-right-2;i++)
	    text=" "+text;
	//	System.out.println("nach replace:"+text);
	return text.substring(0,max-1);
    }
    public void toAsciiFile(File outputFile)
	throws IOException
    {
	//	toAsciiFile(outputFile," "," yy MM dd   1.00 ","##############0.00");
        //TODO Format readable in GraficTool (JH)
	toAsciiFile(outputFile," ","dd MM yyyy hh ","##0.00");
    }
}
