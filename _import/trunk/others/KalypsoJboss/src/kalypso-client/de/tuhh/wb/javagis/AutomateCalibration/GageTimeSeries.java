package de.tuhh.wb.javagis.AutomateCalibration;

import java.text.SimpleDateFormat;
import java.text.DateFormat;
import java.text.DecimalFormat;
import java.util.TimeZone;
import java.util.Date;

import java.util.TreeMap;
import java.util.SortedMap;
import java.util.Vector;
import java.util.Iterator;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import java.io.LineNumberReader;
import java.io.FileReader;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;

public class GageTimeSeries {

	private DateFormat dateFormat = new SimpleDateFormat("dd.MM.yyyy HH:mm:ss");

	private SortedMap timeSerie;

	public GageTimeSeries() {
		dateFormat.setTimeZone(TimeZone.getTimeZone("GMT+1:00"));
		timeSerie = new TreeMap();
	}

	//private final static int SEARCH_TIMEOFFSET = 0;
	//private final static int SEARCH_VALUES = 1;

	public void importPegelDat(File pegelFile) {
		Date date;

		try {
			Pattern pTime =
				Pattern.compile(
					"([0-9]{2}.+[0-9]{2}.+[0-9]{4}.+[0-9]{2}.+[0-9]{2}.+[0-9]{2}).+(\\d+\\.\\d+)");
			//Pattern pValue = Pattern.compile("\\D*(\\d+\\.\\d+)\\D*");

			LineNumberReader reader =
				new LineNumberReader(new FileReader(pegelFile));
			String line;
			Matcher m = null;

			while ((line = reader.readLine()) != null) {
				//			System.out.println("LINE: "+line);
				if (!line.startsWith("#"))
					m = pTime.matcher(line);
				if (m.matches()) {
					String sdate = m.group(1);
					String value = m.group(2);
					date = (dateFormat.parse(sdate));
					/*System.out.println(
						"Date: " + date + "Value: " + value);*/
					timeSerie.put(date, value);
				}
			}

		} catch (Exception e) {
			e.printStackTrace();
			System.out.println("could not read file ");
		}
	}
	
	public Vector getData(Date startDate, Date endDate){
		
		System.out.println("Startdate: " + startDate);
		System.out.println("Enddate: " + endDate);
		
		SortedMap map_all = timeSerie;
		SortedMap map_result = null;

		Date start_dateKey = null;
		Date end_dateKey = null;
		
		Vector resultData = new Vector();
		
		Iterator it_all = map_all.keySet().iterator();
		while (it_all.hasNext()) {
			Object dateKey = it_all.next();
			Date actualDate = (Date) dateKey;
			Object value = (String) map_all.get(dateKey);
			DateFormat dateformat = new SimpleDateFormat("dd.MM.yyyy HH:mm");
			dateformat.setTimeZone(TimeZone.getTimeZone("GMT+1:00"));
			System.out.println(
				"Actual Date: " + dateformat.format(actualDate) + "   Value: " + value);
			int start = actualDate.compareTo(startDate);
			int end = actualDate.compareTo(endDate);
			if (start == 0) {
				start_dateKey = actualDate;
				System.out.println("##StartKey: " + start_dateKey);
			}
			if (end == 0) {
				end_dateKey = actualDate;
				System.out.println("##EndKey: " + end_dateKey);
			}
		}

		map_result = map_all.subMap(start_dateKey,end_dateKey);
		Iterator it_result = map_result.keySet().iterator();
		while (it_result.hasNext())
		{
			Object dateKey = it_result.next();
			Object value = (String) map_result.get(dateKey);
			resultData.add(value);
		}
		resultData.add(map_all.get(end_dateKey));
		
		return resultData;
		
	}
	
	public void writeln(FileWriter writer,String line) throws IOException
	{
	line=line+System.getProperty("line.separator");
	writer.write(line,0,line.length());
	}
	
	public void writeDataToFile(File kalypsoInp,Vector data)throws IOException{
		
		DecimalFormat decimalFormat1=new DecimalFormat("00000.00");
		DecimalFormat decimalFormat2=new DecimalFormat("00000");
		
		FileWriter writer=new FileWriter(kalypsoInp);
		String line1 = "# DATA, OBJ FUNC, DATA TYPE";
		writeln(writer,line1);
		int numData = data.size();
		int flagObjFunc = 1; //default:sls
		int flagDataType = 0; //optimise with gauging station data
		String line2 = decimalFormat2.format(numData)+decimalFormat2.format(flagObjFunc)+decimalFormat2.format(flagDataType);
		writeln(writer,line2);
		String line3 = " TIME OBS.FLOW";
		writeln(writer,line3);
		for(int i = 0;i<data.size();i++){
			double value = Double.parseDouble((String)data.elementAt(i));
			String stringValue = decimalFormat1.format(value);
			String newStringValue = stringValue.replaceAll(",",".");
			String line = decimalFormat2.format(i)+newStringValue;
			writeln(writer,line);
		}
		writer.close();
		//System.out.println("Wrote File");
		
	}
	

}
