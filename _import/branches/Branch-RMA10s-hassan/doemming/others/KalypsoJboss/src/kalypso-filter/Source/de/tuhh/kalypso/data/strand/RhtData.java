package de.tuhh.kalypso.data.strand;

import de.tuhh.kalypso.data.node.Node;
import de.tuhh.kalypso.util.Printf;

import java.util.Calendar;
import java.util.Date;
import java.text.DateFormat;
import java.text.NumberFormat;
import java.util.Locale;
import java.util.Vector;
import java.util.Iterator;
import java.io.StreamTokenizer;
import java.io.BufferedWriter;
import java.io.IOException;
import java.text.ParseException;

/** The class RhtData.java holds the data for all sesional functions of a reservoir.
 *
 * @author <A HREF="mailto:c.kuepferle@tuhh.de">Christoph Küpferle</A>
 * @version Rhb.java,v 1.0 2002/07/01
 */

public class RhtData
{
	/** String representing the name of this function ( user input: not implemented)
	 * @param m_nameFunction*/
	private String m_nameFunction = null;
	/** Date form which day of the year this function is valid.
	 * @param m_date Full date with the default year of 2001 because only the day of the year is need*/
	private Date m_date = null;
	/** This value holdes the initial volume of the reservoir.
	 * @param m_initVolume Default value: -1.0*/
	private double m_initVolume = -1.0;
	/** This value holds the max. volume of the reservoir.
	 * @param m_maxVolume Default value: -1.0*/
	private double m_maxVolume = -1.0;
	/** This value holds the outflow form the reservoir ( overflow ) */
	private double m_outFlow = -1.0;
	/** The model kalypso allows different calcuation cases. This value holds
	 * the case number ( see manual BCENA, Rht file, for further detail ). There
	 * can only be one case assigned for a complete sesional function, meaning
	 * it is not possible to switch case from data row to data row.
	 * @param m_case*/
	private int m_case = -1;
	/** This Vector holds the RhtData data sets.
	 * @param m_data*/
	private Vector m_data = new Vector();
	public RhtData(){};
	/** This constructor reads a row of rht data .
	 * @param st points to the first element of the row in the file. Once this
	 * section has parsed the data it must point to the next element ( Token )
	 * @exception Throws an IOException if the stream tokanizer is corrupetd.
	 */
	public RhtData( StreamTokenizer st ) throws IOException
	{
		//setNameFunction( st.sval );
		st.nextToken();
		setDate( (int) st.nval );
		st.nextToken();
		st.nextToken();
		st.nextToken();
		st.nextToken();
		setInitVolume( st.nval );
		st.nextToken();
		setMaxVolume( st.nval );
		st.nextToken();
		setOutFlow( st.nval );
		st.nextToken();
		int nrOfLines = (int) st.nval;
		for( int i = 0 ; i < nrOfLines ; i++ )
		{
			st.nextToken();
			double waterLevel = st.nval;
			st.nextToken();
			double volume = st.nval;
			st.nextToken();
			double q = st.nval;
			st.nextToken();
			setCase( (int) st.nval );
			addNewData( waterLevel, volume, q );
		}// for
		
	}// end of constuctor
	/** Returns the name of the sesional function.
	 * @param m_nameFunction*/
	public String getNameFunction() { return m_nameFunction; }
	/** Sets the name of the sesional function.
	 * @param name*/
	public void setNameFunction( String name ) {  m_nameFunction = name; }
	/** Returns the date of the sesional function.
	 * @param m_date*/
	public Date getDate() { return m_date; }
	/** Sets the initial Volume of this reservoir.
	 * @param volume Volume in m³*/
	public void setInitVolume( double volume ) { m_initVolume = volume; }
	/** Retruns the inital volume of this reservoir.
	 * @return m_initVolume Volume in m³*/
	public double getInitVolume() { return m_initVolume; }
	/** Sets the max. Volume for this reservoir.
	 * @param volume Max. volume m³*/
	public void setMaxVolume( double volume ) { m_maxVolume = volume; }
	/** Returns the max. volume of this reservoir.
	 *  m_maxVolume Volume in m³*/
	public double getMaxVolume() { return m_maxVolume; }
	/** Returns the Vector containing the sets of data for this reservoir.
	 * @return m_data As a Vector*/
	public Vector getData() { return m_data; }
	/** Returns the number of data sets in the m_data Vector.
	 * @return size The size of the Vector m_data*/
	public int getNrOfDataSets() { return m_data.size(); }
	/** Sets the case for calcualtion.
	 * @param i Case number.*/
	public void setCase( int i ) { m_case = i; }
	/** Returns the case number for calcuation.
	 * @return m_case*/
	public int getCase() { return m_case; }
	/** Sets the discharge for the outflow of the reservoir.
	 * @parma val Discharge in m³/s*/
	public void setOutFlow( double val ) { m_outFlow = val; }
	/** Returns the assigned discharg of the outflow of this reservoir
	 * @return m_outFlow*/
	public double getOutFlow() { return m_outFlow; }
	/** Sets the day using the day of the year as a number.
	 * @param dayOfYear The day of the year ( Leap years are not implemented in the model kalypso )*/
	public void setDate( int dayOfYear )
	{
		if( m_date == null )
		{
			Calendar c = Calendar.getInstance();
			// Default Year is 2001, because the year is not needed. If the year
			// is needed it has to be implemnented.
			c.set(c.YEAR, 2001 );
			c.set( c.DAY_OF_YEAR, dayOfYear );
			m_date = c.getTime();
		}// if
	}// setDate
	public void setDate ( long milisec )
	{
		if( m_date == null )
		{
			Calendar c = Calendar.getInstance();
			// Default Year is 2001, because the year is not needed. If the year
			// is needed it has to be implemnented.
			c.setTimeInMillis( milisec );
			Date date = c.getTime();
			m_date = date;
		}
		
	}// setDate
	public void setDate ( Date date ) { m_date = date; }
	/** This method adds a new data triplet to the m_data Vector ( sesional data ).
	 * @param level The water level for this volume and discharge.
	 * @param resVol The volume at this water level and discharge.
	 * @param Q The discharge at this wate level and volume.*/
	public void addNewData( double level, double resVol, double Q )
	{
		Vector v = getData();
		Data d = new Data();
		d.waterLevel = level;
		d.resevoirVolume = resVol;
		d.discharge = Q;
		v.add( d );
	}// addNewData
	/** This inner class holds the data for one triplet of the m_data vector.
	 *  A triplet descriebs the discharge-volume relationship for a reservoir in
	 * a sesional function.*/
	public class Data
	{
		/** This field holds the water level for at a specific reservoir volume.
		 * @param waterLevel Default value: -1.0*/
		public double waterLevel = -1.0;
		/** This field holds the reservoir volume at a specific water level.
		 * @param waterLevel Default value: -1.0*/
		public double resevoirVolume = -1.0;
		/** This field holds the discharge at a specific reservoir volume
		 * @param discharge Default value: -1.0*/
		public double discharge = -1.0;
	}
	/** Returns a 1d array holding strings to write one row of data
	 * to the rht file.
	 * @param i Element index ( Vector element ).
	 * @return array */
	protected String [] dataRowToArray ( int i )
	{
		Data d = ( Data ) getData().elementAt( i );
		String array[] = { String.valueOf( d.waterLevel ), String.valueOf( d.resevoirVolume ),
				String.valueOf( d.discharge ), String.valueOf( getCase() ) };
		return array;
	}//dataRowToArray
	/** Returns a 1d array holding strings to write the 1st two rows of a sesional
	 * data block to the rht file
	 * @return array */
	public String[] headOfDataBlockToArray()
	{
		java.text.SimpleDateFormat sdf = new java.text.SimpleDateFormat("d. MMM");
		Calendar c = Calendar.getInstance();
		c.setTime( getDate() );
		String array[] = { String.valueOf( c.get( c.DAY_OF_YEAR ) ),"Knl ", sdf.format( getDate() ),
				String.valueOf( getInitVolume() ), String.valueOf( getMaxVolume() ),
				String.valueOf( getOutFlow() ), String.valueOf( getData().size() ) };
		return array;
	}
	/** Writes the rht sesinal data block to the rht file.
	 * @param bw BufferedWriter of the rht file.
	 * @exception Throws IOException if the buffered writer is corrupted.*/
	public void writeRhtDataToFile( BufferedWriter bw ) throws IOException
	{
		for( int i = 0 ; i < getData().size() ; i++ )
		{
			bw.write( Printf.format("%20.6f%10.3f%12.5f%10.0d", dataRowToArray( i ) ) );
			bw.newLine();
		}// for
	}// writeRhtDataToFile
	public void writeRhtDataToXml( BufferedWriter bw ) throws IOException
	{
		Vector data = getData();
		Iterator it = data.iterator();
		bw.write(" <v key=\"m_data\">" );
		while( it.hasNext() )
		{
			Data d = (Data) it.next();
			
			bw.write( " <v_row m_WaterLevel=\"" + d.waterLevel + "\" m_Volume=\""
						 + d.resevoirVolume + "\" m_discharge=\"" + d.discharge + "\" />" );
		}// while
		bw.write( " </v>");
	}
	/** sets the date to the desired format.
	 * @param l The desired Format*/
	public void setLocale( Locale l )
	{
		NumberFormat nf = NumberFormat.getInstance( l );
	}
}// Class RhtData

