package de.tuhh.kalypso.data.strand;

import java.io.BufferedWriter;
import java.io.File;
import java.io.IOException;
import java.io.StreamTokenizer;
import java.util.Iterator;
import java.util.Vector;

import de.tuhh.kalypso.data.node.Node;
import de.tuhh.kalypso.util.Printf;
import de.tuhh.kalypso.util.xmlParser.GisTransferObject;
import de.tuhh.kalypso.util.xmlParser.VectorSet;

/** The class Rht.java holds the data of a reservoir ( @see de.tuhh.kalypso.data.strand.Rhb )
 * taking into account the evaporation of water from the water surface. It was also
 * designed to simulate controling patterns for reservoirs. There is the possiblity,
 * to have different volume to discharge relationships depending on the season.
 *
 * @author <A HREF="mailto:c.kuepferle@tuhh.de">Christoph Küpferle</A>
 * @version Rht.java,v 1.0 2002/07/01
 */



public class Rht extends StrandData
{
	/** Type name, no relevant parameter for calulation, but a comment from the
	 * user. It is not implemented to be changed from the user ( TODO )
	 * @param m_typeName Default: Reservoir*/
	private String m_typeName = "Reservoir";
	/** The node where the reservoir discharges water in case of overflow,
	 * the defalult value is null, meaning the traget node is the out node of
	 * the strand and the amount of overflow is added to the max outflow of the
	 * reservoir.
	 * @param m_tragetNode*/
	private Node m_targetNode = null;
	/** This value splits the discharge between stations.
	 * @param m_C*/
	private double m_C = -1.0;
	/** The file containing the evaporation data for the reservoir.
	 * @param m_evapFile*/
	private File m_evapFile = null;
	/** The file containing the data for the long term simulation of the reservoir.
	 * @param m_longTermSimFile*/
	private File m_longTermFileSim = null;
	/** The 2nd line in the file which allows the user to comment the data, User
	 * input not implemented ( TODO )
	 * @param m_comment Default null.*/
	private String m_comment = null;
	/** This vector contains the data describing the volume discharge relationship
	 * from this reservoir.
	 * @param m_sesionlaFunct*/
	private Vector m_sesionalFunct = new Vector();
	
	/** Sets the type name of the string
	 * @param name The user defined type name*/
	public void setTypeName( String name ) { m_typeName = name; }
	/** Returns the user defined type name of this reservoir.
	 * @return m_typeName*/
	public String getTypeName() { return m_typeName; }
	/** Sets the C fact for this reservoir.
	 * @param val C factor value between 0 and 1*/
	public void setCFact( double val ) { m_C = val; }
	/** Returns the C fact for this reservoir.
	 * @return m_C Value between 0 and 1*/
	public double getCFact() { return m_C; }
	/** Sets the comment string for this reservoir
	 * @param s The string containing the comment.*/
	public void setComment( String s ) { m_comment = "\\" + s; }
	/** Returns the comment for this reservoir.
	 * @return m_comment*/
	public String getComment() { return m_comment; }
	/** Sets the target node of the overflow at this strand.
	 * @param n The traget node of the overflow.*/
        public void setTargetNode( Node n ) { m_targetNode = n; }
	/** Returns the node where this reservoir is discharging at overflow.
	 * @retrun m_targetNode*/
	public Node getTargetNode() { return m_targetNode; }
	/** Creates the EvapFile using s as its filename and path.
	 * @param s Name and/or path of the evap file.*/
	public void setEvapFile( String s ) { m_evapFile = new File( s ); }
	/** Creates the LongTermSimfFile using s as its filename and path.
	 * @param s Name and/or path of the long term sim file.*/
	public void setLongTermSimFile( String s ) { m_longTermFileSim = new File( s ); }
	/** Returns the Vector containing a data set of the sesional function.
	 * @return m_sesionalFunct */
	public Vector getSesionalFunct() { return m_sesionalFunct; }
	/** Returns the Type number. Type number for rhb is 2.
	 * @return int type number "3"*/
	public int getType() { return 3; }
	/** Returns the attached evap data file.
	 * @return m_evapFile*/
	public File getEvapFile() { return m_evapFile; }
	/** Returns the attached long term simulation data file.
	 * @return m_longTermSimFile*/
	public File getLongTermSimFile() { return m_longTermFileSim; }
	/** Returns the number of sesional fuctions of this reservoir.
	 * @return int Size of the sesional function vector.*/
	public int getNrOfSets() { return m_sesionalFunct.size(); }
	/** Retuns this object (Rht)
	 * @return rht*/
	public StrandData getStrandData()
	{
		Rht rht = this;
		return rht;
	}// getStrandData
	public void mapStrandData( GisTransferObject  gto ) throws Exception
	{
		setTypeName( gto.getSimpleProperty( "m_typeName"));
		setCFact( Double.parseDouble( gto.getSimpleProperty( "m_C" )));
		//setComment ( gto.getSimpleProperty( "m_comment" ));
		setEvapFile( gto.getSimpleProperty( "m_evapFile" ));
		setLongTermSimFile( gto.getSimpleProperty( "m_longTermFileSim" ));
		VectorSet vs = gto.getVectorSet( "m_rhtData" );
		for( int i = 0; i < vs.size(); i++ )
		{
			RhtData rd = new RhtData();
			addRhtData( rd );
			rd.setNameFunction(	vs.getSimpleProperty( "v_NameFunction", i ));
			rd.setDate( Long.parseLong(( vs.getSimpleProperty( "v_date",  i ))));
			rd.setInitVolume( Double.parseDouble( vs.getSimpleProperty( "v_initVolume", i )));
			rd.setMaxVolume( Double.parseDouble( vs.getSimpleProperty( "v_maxVolume", i )));
			rd.setOutFlow( Double.parseDouble( vs.getSimpleProperty( "v_outFlow", i )));
			rd.setCase( (int) Integer.parseInt( vs.getSimpleProperty( "v_case", i )));
			
			VectorSet vs2 = vs.getVectorSet( "m_data" , i );
			for( int j = 0; j < vs2.size(); j++ )
			{
				double wL = ( Double.parseDouble( vs2.getSimpleProperty( "m_WaterLevel", 1 )));
				double vol = ( Double.parseDouble( vs2.getSimpleProperty( "m_Volume", 2 )));
				double q = ( Double.parseDouble( vs2.getSimpleProperty( "m_discharge", 3 )));
				rd.addNewData( wL, vol, q );
			}
		}
		
	}// mapStrandData
	/** Adds RhtData to the sesiional function Vector. One element of the vector
	 * contains a complete control function ( for one specific date )
	 * @param d The RhtData to be added to the vector.*/
        private void addRhtData( RhtData d ) { m_sesionalFunct.add( d ); }
	/** Inizialieses the m_sesionalFunct vector with a new empty vector. */
	private void clearRhtData() { m_sesionalFunct = new Vector(); };
	/** This method reads a complete sesional function for this reservoir.
	 * @param st StreamTokanizer form the rhb file beeing parsed.
	 * @param nrOfSets The number of sesional functions for this reservoir.
	 * @exception Throws an IOException if the stream tokanizer is corrupted.*/
	public void readRhtFromFile( StreamTokenizer st, int nrOfSets ) throws IOException
	{
		clearRhtData();
		for( int i = 0 ; i < nrOfSets ; i++ )
			addRhtData( new RhtData( st ) );
	}// readRhtFromFile
	/** Writes all rows contained in a sesional function to the rht file.
	 * @param bw BufferedWriter of the rht file.
	 * @exception Throws IOException if bufferd writer is corrupted.*/
	public	void writeRhtToFile( BufferedWriter bw ) throws IOException
	{
		Iterator it = getSesionalFunct().iterator();
		
		while( it.hasNext() )
		{
			RhtData rd = (RhtData) it.next();
			bw.write( Printf.format("%4d%6s%4.0s%10.6f%10.6f%10.6f%4.0d", rd.headOfDataBlockToArray() ) );
			bw.newLine();
			rd.writeRhtDataToFile( bw );
			
		}//while
		bw.write("END");
		bw.newLine();
	}// writeRhtToFile
        public void writeRhtToXML( BufferedWriter bw ) throws IOException
	{
	    bw.write( " m_typeName=\"" + getTypeName() + "\" m_C=\"" + getCFact() +
		      "\" m_evapFile=\"" + getEvapFile() + "\" m_longTermFileSim=\""
		      + getLongTermSimFile() + "\"> <v key=\"m_rhtData\">" );
		Vector sFunctions = getSesionalFunct();
		Iterator it = sFunctions.iterator();
		while( it.hasNext() )
		{
			java.text.SimpleDateFormat sdf = new java.text.SimpleDateFormat("d. MMM");
			java.util.Calendar c = java.util.Calendar.getInstance();
			RhtData rd = (RhtData) it.next();
			c.setTime( rd.getDate() );
			bw.write( " <v_row v_NameFunction=\"" + rd.getNameFunction() + "\" v_date=\""
						 + c.getTimeInMillis() + "\" v_initVolume=\"" + rd.getInitVolume()
						 + "\" v_maxVolume=\"" + rd.getMaxVolume() + "\" v_outFlow=\""
						 + rd.getOutFlow() + "\" v_case=\"" + rd.getCase() + "\">" );
			rd.writeRhtDataToXml( bw );
			bw.write( " </v_row>" );
		}// while
		bw.write( " </v>" );
	}// writeRhtToXML
}// Class Rht

