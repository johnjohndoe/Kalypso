package de.tuhh.wb.javagis.simulation;

import java.io.File;
import java.util.Hashtable;
import java.util.Vector;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import java.util.Properties;
import java.util.Enumeration;

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
import de.tuhh.wb.javagis.view.LogView;


public class BlockTimeSeries
{
    //    SortedMap myTable;
    private SimpleDateFormat dateFormat=new SimpleDateFormat("yyMMdd");
    private Hashtable blocks;
    
    public BlockTimeSeries()
    {
	this.blocks=new Hashtable();
    }
    
    private final static int SEARCH_TIMEOFFSET=0;
    private final static int SEARCH_BLOCK_HEADER=1;
    private final static int SEARCH_VALUES=2;
    
    public void importBlockFile(File blockFile,Vector allowedKeys)
    {
	long startDate=0;
	long timeStep=0;
	int valuesToGo=0;
	int valueIndex=0;
	SortedMap timeSeries=null;
	try
	    {
		Pattern pTime=Pattern.compile(".+simulationszeitraum.+([0-9]{6}).+?([0-9]{1,2}).+[0-9]{5,6}.+[0-9]{1,2}.+(\\d+\\.\\d+)\\D*");
		Pattern pBlock=Pattern.compile("\\D*(\\d+)\\D+(\\d+)\\D+(\\d+)\\D*");
		Pattern pHeader=Pattern.compile("\\D*(\\d+\\.\\d+)\\D*");
		
		LineNumberReader reader=new LineNumberReader(new FileReader(blockFile));
		String line;
		Matcher m=null;
		
		int step=SEARCH_TIMEOFFSET;
		while((line=reader.readLine())!=null)
		    {
			//			System.out.println("LINE: "+line);
			if(!line.startsWith("#"))
			    switch(step)
				{
				case SEARCH_TIMEOFFSET:
				    m=pTime.matcher(line);
				    if(m.matches())
					{
					    String sDate=m.group(1);
					    String sTime=m.group(2);
					    String sStep=m.group(3);
					    startDate=(dateFormat.parse(sDate)).getTime();
					    startDate+=Long.parseLong(sTime)*1000l*3600l;
					    timeStep=((long)(Float.parseFloat(sStep)*1000f))*3600l;
					    Date testDate=new Date(startDate);
					    System.out.println("startdate: "+testDate+"  step:"+sStep);
					    step++;
					}
				    break;
				case SEARCH_BLOCK_HEADER:
				    m=pBlock.matcher(line);
				    if(m.matches())
					{
					    String key=m.group(1);
					    String unknown=m.group(2);
					    valuesToGo=Integer.parseInt(m.group(3));
					    valueIndex=0;
					    if(allowedKeys==null || allowedKeys.contains(key))
						{
						    timeSeries=new TreeMap();
						    blocks.put(key,timeSeries);
						    step++;
						}
					}
				    break;
				case SEARCH_VALUES:
				    String values[]=line.split("\\s+");
				    for(int i=0;i<values.length;i++)
					{
					    m=pHeader.matcher(values[i]);
					    if(m.matches())
						{
						    String value=m.group(1);
						    Date valueDate=new Date(startDate+valueIndex*timeStep);
						    //						    System.out.println(valueIndex+" ("+valuesToGo+"): "+valueDate.toString()+" "+value);
						    timeSeries.put(valueDate,value);
						    valueIndex+=1;
						    if(valueIndex>=valuesToGo)
							step=SEARCH_BLOCK_HEADER;
						}
					}
				    break;
				default:
				    break;
				}
		    }
		reader.close();
	    }
	catch(Exception e)
	    {
		e.printStackTrace();
		System.out.println("could not read blockfile ");
	    }
    }
    
    public Enumeration getKeys()
    {
	return blocks.keys();
    }

    public void exportToFile(String key,File exportFile)
    {
	try
	    {
		if(blocks.containsKey(key))
		    {
			SortedMap map=(SortedMap)blocks.get(key);
			
			DateFormat dateFormat=new SimpleDateFormat("dd.MM.yyyy HH:mm:ss");
			FileWriter writer=new FileWriter(exportFile);
			String line;
			
			Iterator it=map.keySet().iterator();
			while(it.hasNext())
			    {
				Object dateKey=it.next();
				Object value=(String)map.get(dateKey);
				line=dateFormat.format((Date)dateKey)+" "+value;
				writeln(writer,line);
			    }
			writer.close();
		    }
	    }
	catch(Exception e)
	    {
		e.printStackTrace();
		System.out.println("wq-transformation: problems with object \""+key+"\"");
	    }
    }

    public void writeln(FileWriter writer,String line) throws IOException
    {
	line=line+System.getProperty("line.separator");
	writer.write(line,0,line.length());
    }
    
    /*
      try
      {
      int trim=line.indexOf(seperator,datePattern.length());
      int trimFlag=line.indexOf(seperator,trim+1);
      if(trim!=-1)
      {
      String dateString=line.substring(0,trim);
      String valueString=line.substring(trim+1,trimFlag);
      //					    System.out.println("line: "+line);
		      //					    System.out.print("date: \""+dateString+"\"");
		      //					    System.out.println(" value is: \""+valueString+"\"");
		      Date date=dateFormat.parse(dateString,new ParsePosition(0));
		      Double value=new Double(valueString);
		      //					    System.out.println("got: "+date.toString()+" "+value.toString());
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
		      //		}
		
		      }
		    */  

    /*
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
    */
}
