/**
 * Timeseries.java
 *
 * @author Christoph Küpferle
 */

package de.tuhh.kalypso.data.timeseries;

import java.util.TreeMap;
import java.util.Set;
import java.util.Hashtable;
import java.util.Vector;
import java.util.Iterator;
import java.io.File;
import java.text.SimpleDateFormat;
import java.text.FieldPosition;
import java.util.Date;
import java.util.Calendar;
import java.io.StreamTokenizer;
import java.io.InputStreamReader;
import java.io.FileInputStream;
import java.io.BufferedWriter;
import java.io.FileWriter;

public class Timeseries
{
    private String m_nameTS = null;
    private TreeMap m_timeseries = null;
    private SimpleDateFormat m_importSDF = new SimpleDateFormat("yymmddhh.m");
    private SimpleDateFormat m_exportSDF = null;
    private String m_seperatorTS = null;
    
    //    public Timeseries(){};

    public Timeseries( String name )
    {
	this.m_nameTS = name;
	TreeMap map = new TreeMap();
	this.m_timeseries = map;
    }

    public void setNameTS( String name ) { m_nameTS = name; }
    public String getNameTS() { return m_nameTS; }

    public TreeMap getTimeseries() { return m_timeseries; }

    public String getImportSDF() { return m_importSDF.toPattern(); }
    public String getExportSDF() { return m_exportSDF.toPattern(); }
    public void setImportSDF( String pattern )
    { m_importSDF = new SimpleDateFormat( pattern ); }
    public void setExportSDF( String pattern )
    {
	if( !pattern.equals( "null" ) )
	    m_exportSDF = new SimpleDateFormat( pattern );
	else m_exportSDF = new SimpleDateFormat( "yyyy/MM/dd kk:mm" );
    }

    public String getTSseperator() { return m_seperatorTS; }

    public void setTSseperator( String s )
    {
	if( !s.equals( null ) )
	    m_seperatorTS = s;
	else m_seperatorTS = ",";
    }

    public void addTimeseries( String key, Vector timeSeries )
    {
	TreeMap map = getTimeseries();
	map.put( key, timeSeries );
    }

    static public Timeseries createTS( String name )
    {
	Timeseries timeseries = new Timeseries( name );
	return timeseries;
    }
	
    public void importResultFile( File source ) throws Exception
    {
	try {
	    StreamTokenizer st = new StreamTokenizer(new InputStreamReader(new FileInputStream(source)));
	    st.parseNumbers();
	    st.whitespaceChars( 'a', 'z' );
	    st.whitespaceChars( 'A', 'Z' );
	    st.whitespaceChars( ':', ':' );
	    Date startDate = null;
	    Calendar c = Calendar.getInstance();
	    double dt = 0.0;
	    while( st.nextToken() != StreamTokenizer.TT_EOF )
		{
		    while( st.lineno() < 3)
			st.nextToken();
		    if( st.lineno() == 4 )
			{
			    String s = String.valueOf( (int) st.nval );
			    st.nextToken();
			    s = s + String.valueOf( st.nval );
			    startDate = m_importSDF.parse( s );
			    st.nextToken();
			    st.nextToken();
			    st.nextToken();
			    dt = st.nval;
			    st.nextToken();
			}
		    String key = String.valueOf( (int) st.nval );
		    st.nextToken();
		    st.nextToken();
		    int nrOfvalues = (int) st.nval;
		    Vector timeSeries = new Vector();
		    for( int i = 0; i < nrOfvalues; i++ )
			{
			    int timeToBeAdded = (int) (i * dt);
			    st.nextToken();
			    c.setTime( startDate );
			    c.add( Calendar.HOUR, timeToBeAdded );
			    DataPair pair = new DataPair( c.getTime(), String.valueOf( st.nval ) );
			    timeSeries.add( pair );
			}
		    addTimeseries( key, timeSeries );
		}
	}
	catch( Exception e )
	    {
		System.out.println(" Error Message: " + e);
		e.printStackTrace();
		throw e;
	    }
    }

    public Hashtable exportTS( File targetDir ) throws Exception
    {
	if( targetDir.exists() == false )
	    targetDir.mkdirs();
	Hashtable ht = new Hashtable();
	TreeMap map = getTimeseries();
	Set keySet = map.keySet();
	Iterator itKeySet = keySet.iterator();
	
	try
	    {
		while( itKeySet.hasNext() )
		    {
			String key = (String) itKeySet.next();
			Vector dataSet = (Vector) map.get( key );
			File targetFile = new File( targetDir,"tmp" + key + ".dat");
			BufferedWriter bw = new BufferedWriter( new FileWriter( targetFile ) );
			Iterator itDataSet = dataSet.iterator();
			while( itDataSet.hasNext() )
			    {
				DataPair dataPair = (DataPair) itDataSet.next();
				StringBuffer sb = new StringBuffer();
				sb = m_exportSDF.format( dataPair.getDateFromDataPair(), sb, new FieldPosition(0) );
				bw.write( sb.toString() + "," + dataPair.getValueFromDataPair() );
				bw.newLine();
			    }
			ht.put( key, targetFile );
			bw.close();
		    }
	    }
	catch( Exception e )
	    {
		if( e.getCause() == null )
		    System.out.println( "test" );
	    }
	return ht;
    }
}

class DataPair
{
	private Date date = null;
	private String value = null;
	public DataPair( Date date, String value )
	{
		this.date = date;
		this.value = value;
	}

	public String toString()
	{
		return date.toString() + "   " + value;
	}
	public Date getDateFromDataPair() { return date; }
	public String getValueFromDataPair() { return value; }
}
