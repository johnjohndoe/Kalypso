package de.tuhh.wb.javagis.simulation;

import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.LineNumberReader;
import java.text.ParsePosition;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.SortedMap;
import java.util.TreeMap;

import com.bce.datacenter.kalypso.Timeserie;


import de.tuhh.wb.javagis.view.LogView;

public abstract class SimpleTimeSeries
{
    SortedMap myTable;
    Date myMinDate;
    Date myMaxDate;
    
    public SimpleTimeSeries(Date minDate,Date maxDate)
    {
	LogView.println("  "+LogView.format(minDate)+" to "+LogView.format(maxDate)+" new time series");
	this.myTable=new TreeMap();
	this.myMinDate=minDate;
	this.myMaxDate=maxDate;
	//	store(minDate,new Double(0d));
	//	store(maxDate,new Double(0d));
    }
    
    public String getSeperator()
    {
	return ",";
    }
    
    public String getDatePattern()
    {
	return "yyyy dd MM HH mm";
    }
        
    public Date loadFromWadas(String tableName,Date startDate,Date endDate)
	throws Exception
    {
	System.out.println(" table: "+tableName);
	System.out.println(" startDate:"+startDate);
	System.out.println(" endDate:"+endDate);
	    
	LogView.println("  "+LogView.format(startDate)+" to "+LogView.format(endDate)+" < "+tableName);
	try
	    {
		if(startDate.before(endDate))
		    {
			String seperator=getSeperator();
			String datePattern=getDatePattern();
			SimpleDateFormat dateFormat=new SimpleDateFormat(datePattern);
			File input=File.createTempFile("timeseries",".dat");
								
			Timeserie timeSerie=Timeserie.findTimeserie(tableName);
			//System.out.println("timeSerie:"+timeSerie);
			/*
			  public TimeserieWrapper(	String tableName,
			  String srcType,
			  int sourceID,		  
			  String info )
			*/       
			
            //in Temp-Zeitreihe 1 Tag am Ende hinzuaddiert (um später Tagesverschiebung bei Langzeitsim durchführen zu können.)
			Calendar calend = Calendar.getInstance(); 
			calend.setTime(endDate);
			//System.out.println("enddate_alt" +endDate);
			calend.add(Calendar.DATE, 1); 
			Date newEndDate = calend.getTime(); 
			//System.out.println("newEndDate" + newEndDate);
			endDate = newEndDate;
			
            //in Temp-Zeitreihe 10 Minuten am Anfang hinzuaddiert. 
			Calendar calanf = Calendar.getInstance(); 
			calanf.setTime(startDate);
			//System.out.println("startdate_alt" +startDate);
			calanf.add(Calendar.MINUTE, -10); 
			Date newStartDate = calanf.getTime(); 
			//System.out.println("newStartDate" + newStartDate);
			startDate = newStartDate;
			
			
			
			java.sql.Date sqlStartDate=new java.sql.Date(startDate.getTime());
			java.sql.Date sqlEndDate=new java.sql.Date(endDate.getTime());
			java.sql.Date realStart=timeSerie.getRealBegin();
			java.sql.Date realEnd=timeSerie.getRealEnd();
			java.util.Date firstDate=new java.util.Date(realStart.getTime());
			java.util.Date lastDate=new java.util.Date(realEnd.getTime());
			int i=timeSerie.ExportToFile(input.getPath(), sqlStartDate, sqlEndDate,seperator,datePattern);
						
			LogView.println("  "+LogView.format(firstDate)+" ... "+LogView.format(lastDate)+" available in "+tableName);

			// ************
			//	File input=new File(FileSystemUtils.getNaWorkDir(),"timeSeries.dummy");
			// ************
			
			LineNumberReader reader=new LineNumberReader(new FileReader(input));
			String line;
			while((line=reader.readLine())!=null)
			    if(!line.startsWith("#"))
				{
				    try
					{
					    int trim=line.indexOf(seperator,datePattern.length());
					    int trimFlag=line.indexOf(seperator,trim+1);
					    if(trim!=-1)
						{
						    String dateString=line.substring(0,trim);
						    String valueString=line.substring(trim+1,trimFlag);
						    /*
						   	  System.out.println("line: "+line);
						      System.out.print("date: \""+dateString+"\"");
						      System.out.println(" value is: \""+valueString+"\"");
						    */
						    Date date=dateFormat.parse(dateString,new ParsePosition(0));
						    Double value=new Double(valueString);
						    //System.out.println("got: "+date.toString()+" "+value.toString());
						    store(date,value);
						}
					}
				    catch(Exception e)
					{
					    System.out.println(e.getMessage());
					    //					    System.out.println("ignored line: "+line);
					    LogView.println("  ignore line:"+line+" (check format)");
					}
				}
			reader.close();
			input.delete();	
			System.out.println("lastDate is "+lastDate);	    
			return lastDate;
		    }
		else
		    return endDate;
	    }
	catch(Exception e)
		{
		    LogView.println("  import failed (empty table in database ?), ");
		    LogView.println("  but simulation will continue without failed import");
		    LogView.println("   notice: with the grafik-tool you can always check on what time series your results are based on\n");
		    return startDate;
		    //		    throw e;
		}
		
    }
    
    public abstract void store(Date date,Double value);
    
    private void storeRelative(Date offset,Date date,Double value)
    {
	Calendar calOffset=new GregorianCalendar();
	Calendar calValue=new GregorianCalendar();
	calOffset.setTime(offset);
	calValue.setTime(date);

	if(calValue.get(Calendar.DAY_OF_YEAR)<365)
	    calOffset.add(Calendar.DAY_OF_YEAR,calValue.get(Calendar.DAY_OF_YEAR));
	calOffset.add(Calendar.HOUR_OF_DAY,calValue.get(Calendar.HOUR_OF_DAY));
	calOffset.add(Calendar.MINUTE,calValue.get(Calendar.MINUTE));
	store(calOffset.getTime(),value);
    }
    
    public abstract String getSeparatorRelative();
    
    public abstract String getDatePatternRelative();

    public void loadFromRelativASCII(Date offsetDate,File input) throws IOException
    {
	LogView.println("  "+LogView.format(offsetDate)+" to ... < "+input+" (relative)");
	String seperator=getSeparatorRelative();
	String datePattern=getDatePatternRelative();
	SimpleDateFormat dateFormat=new SimpleDateFormat(datePattern);
	LineNumberReader reader=new LineNumberReader(new FileReader(input));
	String line;
	while((line=reader.readLine())!=null)
	    if(!line.startsWith("#"))
		{
		    try
			{
			    int trim=line.indexOf(seperator);//,datePattern.length());
			    if(trim!=-1)
				{
				    String dateString=line.substring(0,trim);
				    //				    System.out.print("date: \""+dateString+"\"");
				    String valueString=line.substring(trim+1);
				    //				    System.out.println(" value is: \""+valueString+"\"");
				    Date date=dateFormat.parse(dateString,new ParsePosition(0));
				    Double value=new Double(valueString);
				    //				    System.out.println("got: "+date.toString()+" "+value.toString());
				    storeRelative(offsetDate,date,value);
				}
			}
		    catch(Exception e)
			{
			    e.printStackTrace();
			    System.out.println(e.getMessage());
			    LogView.println("  ignored :"+line);
			}
		}
	reader.close();
    }
    
    public abstract void toAsciiFile(File outputFile,String separator,String datePattern,String decimalPattern)
	throws IOException;

    public void writeln(FileWriter writer,String line) throws IOException
    {
	line=line+System.getProperty("line.separator");
	writer.write(line,0,line.length());
    }

    public abstract void toAsciiFile(File outputFile)
	throws IOException;
}
