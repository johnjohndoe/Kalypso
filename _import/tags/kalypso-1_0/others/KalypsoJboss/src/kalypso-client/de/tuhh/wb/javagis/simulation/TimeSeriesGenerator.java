package de.tuhh.wb.javagis.simulation;

import java.util.Hashtable;
import java.util.Enumeration;
import java.io.File;
import java.util.Properties;
import java.io.FileWriter;
import java.io.IOException;
import java.util.Date;

//import FileSystemUtils;

import de.tuhh.wb.javagis.view.LogView;
import de.tuhh.wb.javagis.xml.KalypsoXmlImportListener;
import de.tuhh.wb.javagis.xml.GisTransferObject;
import de.tuhh.wb.javagis.xml.VectorSet;
import de.tuhh.kalypso.data.I_FilterImpl;
import datacenter.persistent.Database;
import de.tuhh.wb.javagis.simpleclient.StationFileDialog;

public class TimeSeriesGenerator implements KalypsoXmlImportListener
{
    private boolean useLongTerm=false;

    public void useLongtermData(boolean status)
    {
	this.useLongTerm=status;
    }
    
    private Date myStartDate;
    private Date myEndDate;
    private static Date myForecastDate;
    private static Hashtable rainStations=new Hashtable();
    private static Hashtable tempStations=new Hashtable();

    private File myDestinationDir;
    public TimeSeriesGenerator(File destinationDir,Date startDate,Date endDate)
    {
	this.myDestinationDir=destinationDir;
	this.myStartDate=startDate;
	this.myEndDate=endDate;

	//	this.rainStations=new Hashtable();
	//	this.tempStations=new Hashtable();

	/*
	  try
	  {
	  Database.init("ca.edbc.jdbc.EdbcDriver",//driver
	  "jdbc:edbc://128.1.5.67:LP7/BCE_PC067::dbflomatis/INGRES",//url
	  "ingres",//user
	  "ingres");//password
	  }
	  catch(Exception e)
	  {
	  e.printStackTrace();
	  System.out.println("Error: Connect to database failed");
	  }
	*/
    }
    
    public TimeSeriesGenerator(File destinationDir,GisTransferObject gto)
    {
	// gto: simpleclient
	this.myDestinationDir=destinationDir;
	this.myStartDate=gto.getSimplePropertyAsDate("m_startDate");
	this.myForecastDate=gto.getSimplePropertyAsDate("m_forecastDate");
	this.myEndDate=gto.getSimplePropertyAsDate("m_endDate");
	this.rainStations=new Hashtable();
	this.tempStations=new Hashtable();
	// load rainstations
	VectorSet vs=gto.getVectorSet(StationFileDialog.RAIN_KEY);
	String name;
	File file;
	Boolean ignore;
	if(vs!=null)
	    {
		for(int row=0;row<vs.size();row++)
		    {
			name=vs.getSimpleProperty("name",row);
			file=new File(vs.getSimpleProperty("file",row));
			ignore=new Boolean(vs.getSimpleProperty("ignore",row));
			System.out.println(name);
			if(!ignore.booleanValue())
			    rainStations.put(name2FileName(name),file);
		    }
	    }
	// load tempstations
	vs=gto.getVectorSet(StationFileDialog.TEMP_KEY);
	if(vs!=null)
	    {
		for(int row=0;row<vs.size();row++)
		    {
			name=vs.getSimpleProperty("name",row);
			file=new File(vs.getSimpleProperty("file",row));
			ignore=new Boolean(vs.getSimpleProperty("ignore",row));
			System.out.println(name);
			if(!ignore.booleanValue())
			    tempStations.put(name2FileName(name),file);
		    }
	    }
    }

    /**
     * generate timeseries for stations without database-tables
     */
    public void generateLeftStations()
    {
	generateLeftRainStations();
	//	generateLeftTempStations();
    }

    public void generateLeftRainStations()
    {
	LogView.println("generate rain series from relative sequences without corresponding database series:");
	String name=null;
	File file=null;
	for(Enumeration e=rainStations.keys();e.hasMoreElements();)
	    {
		name=(String)e.nextElement();
		file=(File)rainStations.get(name);
		try
		    {
			RainTimeSeries sequence=new RainTimeSeries(myStartDate,myEndDate);
			sequence.loadFromRelativASCII(myForecastDate,file);
			sequence.toAsciiFile(new File(myDestinationDir,name+".kz")); // relative rain is always short time
		    }
		catch(Exception err)
		    {
			System.out.println("Error while generating timeseries for "+name);
			err.printStackTrace();
		    }
	    }	
	name=null;
	file=null;
	LogView.println("generate temp series from relative sequences without corresponding database series:");
 	for(Enumeration e=tempStations.keys();e.hasMoreElements();)
	    {
		name=(String)e.nextElement();
		file=(File)tempStations.get(name);
		try
		    {
		        TempTimeSeries sequence=new TempTimeSeries(myStartDate,myEndDate);
			sequence.loadFromRelativASCII(myForecastDate,file);
			sequence.toAsciiFile(new File(myDestinationDir,name+".tem")); // relative temperatur
		    }
		catch(Exception err)
		    {
			System.out.println("Error while generating timeseries for "+name);
			err.printStackTrace();
		    }
	    }
	removeFileStations();
	LogView.println("done");
    }
    
    public void removeFileStations()	
    {
	rainStations.clear();
	tempStations.clear();
	myForecastDate=null;
    }
    
    public static String name2FileName(String name)
    {
	if(name==null)
	    name="default";
	return name.toLowerCase().replace(' ', '_');
    }

    public void importObject(GisTransferObject gto)
    {
	final int RAIN=1;
	final int WATERLEVEL=2;
	final int TEMP=3;
	final int UNKNOWN=4;
	String stationName=name2FileName(gto.getSimpleProperty("m_stationName"));
	System.out.println("stationName:"+stationName);
	if(stationName==null || "null".equals(stationName) || "".equals(stationName))
	    return;
	int status=UNKNOWN;
	if("node".equals(gto.getTableName()))
	    status=WATERLEVEL;
	if("rainStation".equals(gto.getTableName()))
	    status=RAIN;
	if("tempStation".equals(gto.getTableName()))
	    status=TEMP;
	try
	    {
		if(status!=UNKNOWN)
		    {
			Date endDate=myEndDate;
			File timeSeriesFile=null;
			switch(status)
			    {
			    case RAIN:
				String extension;
				if(useLongTerm)
				    extension=".lz";
				else
				    extension=".kz";
				timeSeriesFile=new File(myDestinationDir,stationName+extension);
				if(rainStations.containsKey(stationName))
				    endDate=myForecastDate;
				break;
			    case TEMP:
				timeSeriesFile=new File(myDestinationDir,stationName+".tem");
				if(tempStations.containsKey(stationName))
				    endDate=myForecastDate;
				break;
			    case WATERLEVEL:
				timeSeriesFile=new File(myDestinationDir,stationName+".dat");
				break;
			    default:
				break;
			    }
			if(timeSeriesFile.exists())
			    throw new Exception("timeserie exists local, no need to fetch from timeseries database");
			String text;

			System.out.println("station-file: "+timeSeriesFile.getPath());
			
			//measured:

			SimpleTimeSeries sequence=null;
			switch(status)
			    {
			    case RAIN:
				sequence=new RainTimeSeries(myStartDate,myEndDate);			    
				break;
			    case WATERLEVEL:
				sequence=new WaterLevelSeries(myStartDate,myEndDate);			    
				break;
			    case TEMP:
				sequence=new TempTimeSeries(myStartDate,myEndDate);			    
				break;
			}
			Date measuredEndDate=myStartDate;
			String tableName=null;
			String longTermExtension;
			if(useLongTerm && (status==RAIN))
			    longTermExtension="LongTerm";
			else
			    longTermExtension="";
			if((text=gto.getSimpleProperty("m_mesTableName"+longTermExtension))!=null)
			    {
				int trim=text.indexOf(",");
				tableName=text.substring(trim+1,text.length());
				if(!(tableName==null || "".equals(tableName)||"null".equals(tableName)))
				    {
					System.out.println("dbTableName:"+tableName);
					System.out.println("load measured");
					measuredEndDate=sequence.loadFromWadas(tableName,null,null,null,myStartDate,endDate);
				    }
			    }	    
			if(endDate.after(measuredEndDate))
			    {
				// synthetic:
				tableName=null;
				if((text=gto.getSimpleProperty("m_synTableName"+longTermExtension))!=null)
				    {
					int trim=text.indexOf(",");
					tableName=text.substring(trim+1,text.length());
					if(!(tableName==null || "".equals(tableName)||"null".equals(tableName)))
					    {
						System.out.println("dbTableName:"+tableName);
						System.out.println("load synthetic");
						Date syntheticEndDate=sequence.loadFromWadas(tableName,null,null,null,measuredEndDate,endDate);
						System.out.println("last synthetic date from wadas: "+syntheticEndDate);
					    }
				    }	    
			    }
			switch(status)
			    {
			    case RAIN:
				if(rainStations.containsKey(stationName))
				    {
					sequence.loadFromRelativASCII(myForecastDate,(File)(rainStations.get(stationName)));
					rainStations.remove(stationName);
				    }
				break;
			    case TEMP:
				if(tempStations.containsKey(stationName))
				    {
					sequence.loadFromRelativASCII(myForecastDate,(File)(tempStations.get(stationName)));
					// tempStations.remove(stationName);
				    }
				break;
			    default:
				break;
			    }
			sequence.toAsciiFile(timeSeriesFile);		    
		    }

		//-------
		// Verdundstung...
		if(status==TEMP)
		    {
			Date endDate=myEndDate;
			if(tempStations.containsKey(stationName))
			    endDate=myForecastDate;			
			String text;
			File timeSeriesFile=new File(myDestinationDir,stationName+".ver");
			System.out.println("station-file: "+timeSeriesFile.getPath());
			
			if(timeSeriesFile.exists())
			    throw new Exception("timeserie exists local, no need to fetch from timeseries database");
			//measured:

			SimpleTimeSeries sequence=new TempTimeSeries(myStartDate,myEndDate);
		
			Date measuredEndDate=myStartDate;
			String tableName=null;
			String longTermExtension;
			if(useLongTerm && (status==RAIN))
			    longTermExtension="LongTerm";
			else
			    longTermExtension="";
			if((text=gto.getSimpleProperty("m_mesTableNameEvaporation"+longTermExtension))!=null)
			    {
				int trim=text.indexOf(",");
				tableName=text.substring(trim+1,text.length());
				if(!(tableName==null || "".equals(tableName)||"null".equals(tableName)))
				    {
					System.out.println("dbTableName:"+tableName);
					System.out.println("load measured");
					measuredEndDate=sequence.loadFromWadas(tableName,null,null,null,myStartDate,endDate);
				    }
			    }	    
			if(endDate.after(measuredEndDate))
			    {
				// synthetic:
				tableName=null;
				if((text=gto.getSimpleProperty("m_synTableNameEvaporation"+longTermExtension))!=null)
				    {
					int trim=text.indexOf(",");
					tableName=text.substring(trim+1,text.length());
					if(!(tableName==null || "".equals(tableName)||"null".equals(tableName)))
					    {
						System.out.println("dbTableName:"+tableName);
						System.out.println("load synthetic");
						Date syntheticEndDate=sequence.loadFromWadas(tableName,null,null,null,measuredEndDate,endDate);
						System.out.println("last synthetic date from wadas: "+syntheticEndDate);
					    }
				    }	    
			    }

			    switch(status)
			    {
			    case RAIN:
				if(rainStations.containsKey(stationName))
				    {
					sequence.loadFromRelativASCII(myForecastDate,(File)rainStations.get(stationName));
					rainStations.remove(stationName);
				    }
				break;
			    case TEMP:
				if(tempStations.containsKey(stationName))
				    {
					//	relative evaporation does not make sense
					// sequence.loadFromRelativASCII(myForecastDate,(File)tempStations.get(stationName));
					tempStations.remove(stationName);
				    }
				break;
			    default:
				break;
			    }			    
			sequence.toAsciiFile(timeSeriesFile);		    
		    }

		//-------


	    }
	catch(Exception e)
	    {
		LogView.getInstance().print(e.getMessage());
		e.printStackTrace();
	    }
	LogView.getInstance().toFront();
	LogView.getInstance().repaint(1000l);
    }
}
