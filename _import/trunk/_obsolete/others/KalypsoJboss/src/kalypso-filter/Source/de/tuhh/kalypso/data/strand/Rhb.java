package de.tuhh.kalypso.data.strand;

import java.io.BufferedWriter;
import java.io.IOException;
import java.io.StreamTokenizer;
import java.util.Iterator;
import java.util.Vector;

import de.tuhh.kalypso.data.node.Node;
import de.tuhh.kalypso.util.Printf;
import de.tuhh.kalypso.util.xmlParser.GisTransferObject;
import de.tuhh.kalypso.util.xmlParser.VectorSet;
/** This class Rhb.java holds the data for a strand which represents a reservoir.
 * A Reservoir always describes a discharge waterlevel relationship. It can not
 * only discharge water in the out node but also to different target node. This
 * Type does not account for the evaporation from large water surfaces.
 *
 * @author <A HREF="mailto:c.kuepferle@tuhh.de">Christoph Küpferle</A>
 * @version Rhb.java,v 1.0 2002/07/01
 */
public class Rhb extends StrandData
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

    /**
     * work-around by AvD, better use m_targetNode !
     */
    private int m_targetNodeInt=0;
    public void setTargetNode(int node) { m_targetNodeInt = node; }
    /** Returns the node where this reservoir is discharging at overflow.
     * @retrun m_targetNode*/
    public int getTargetNodeAsInt() { return m_targetNodeInt; }


    private Node m_targetNode = null;
    /** This value splits the discharge between stations.
     * @param m_C*/
    private double m_C = -1.0;
    /** The 2nd line in the file which allows the user to comment the data, User
     * input not implemented ( TODO )
     * @param m_comment Default null.*/
    private String m_comment = null;
    /** This value holdes the initial volume of the reservoir.
     * @param m_initVolume Default value: -1.0*/
    private double m_initVolume = -1.0;
    /** This value holdes the max. volume of the reservoir.
     * @param m_maxVolume Default value: -1.0*/
    private double m_maxVolume = -1.0;
    /** This value holdes the min. volume of the reservoir.
     * @param m_maxVolume Default value: -1.0*/
    private double m_minVolume = -1.0;
    /** This Vector holds the rhb data sets.
     * @param m_data*/
    private Vector m_data = new Vector();
	
    /** Sets the type name of the string
     * @param name The user defined type name*/
    public void setTypeName( String name ) { m_typeName = name; }
    /** Returns the user defined type name of this reservoir.
     * @return m_typeName*/
    public String getTypeName() { return m_typeName; }
    /** Sets the target node of the overflow at this strand.
     * @param n The traget node of the overflow.*/
    public void setTargetNode( Node n ) { m_targetNode = n; }
    /** Returns the node where this reservoir is discharging at overflow.
     * @retrun m_targetNode*/
    public Node getTargetNode() { return m_targetNode; }
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
    /** Sets the min. volume of this reservoir.
     *  @param volume Min. volume in m³*/
    public void setMinVolume( double volume ) { m_minVolume = volume; }
    /** Returns the min. volume of this reservooir.
     * @return m_minVolume Min. Volume in m³ */
    public double getMinVolume() { return m_minVolume; }
    /** Returns the Type number. Type number for rhb is 2.
     * @return int type number "2"*/
    public int getType() { return 2; }
    /** Returns the Vector containing the sets of data for this reservoir.
     * @return m_data As a Vector*/
    public Vector getData() { return m_data; }
    /** Returns the number of data sets in the m_data Vector.
     * @return size The size of the Vector m_data*/
    public int getNrOfDataTriplets() { return m_data.size(); }
    /** Inizialieses the m_data vector with a new empty vector. */
    private void clearRhtData() { m_data = new Vector(); };

    public void mapStrandData( GisTransferObject gto )
    {
	setTypeName( gto.getSimpleProperty( "m_typeName"));


		
	setTargetNode(Integer.parseInt(gto.getSimpleProperty("m_targetNode")));
		


		      setCFact( Double.parseDouble( gto.getSimpleProperty( "m_C" )));
		      setComment ( gto.getSimpleProperty( "m_comment" ));
		      setInitVolume( Double.parseDouble( gto.getSimpleProperty( "m_initVolume" )));
		      setMaxVolume( Double.parseDouble( gto.getSimpleProperty( "m_maxVolume" )));
		      setMinVolume( Double.parseDouble( gto.getSimpleProperty( "m_minVolume" )));
		
		      VectorSet vs = gto.getVectorSet( "m_data" );
		      for( int row = 0; row < vs.size(); row++ )
	{
	    double wL = ( Double.parseDouble( vs.getSimpleProperty( "m_WaterLevel", row )));
	    double vol = ( Double.parseDouble( vs.getSimpleProperty( "m_Volume", row )));
	    double q = ( Double.parseDouble( vs.getSimpleProperty( "m_discharge", row )));
	    addNewData( wL, vol, q );
	}// for
		
		      }// mapStrandData
	/** This inner class holds the data for one triplet of the m_data vector.
	 *  A triplet descriebs the discharge-volume relationship for a reservoir.*/
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
		
	}//inner class Data
	
	/** This method adds a new data triplet to the m_data Vector.
	 * @param level The water level for this volume and discharge.
	 * @param resVol The volume at this water level and discharge.
	 * @param Q The discharge at this wate level and volume.*/
	public void addNewData( double level, double resVol, double Q )
	    {
		if( m_data == null )
		    {
			Vector v = new Vector();
			m_data = v;
		    }
		Vector v = getData();
		Data d = new Data();
		d.waterLevel = level;
		d.resevoirVolume = resVol;
		d.discharge = Q;
		v.add( d );
	    }// addNewData
	
	/** This method reads a data triplet from the rhb file.
	 * @param st StreamTokanizer form the rhb file beeing parsed
	 * @exception Throws an IOException if the stream tokanizer is corrupted*/
	public	void readRhbFromFile( StreamTokenizer st ) throws Exception
	    {
		st.nextToken();
		st.nextToken();
		st.nextToken();
		setInitVolume( st.nval );
		st.nextToken();
		setMaxVolume( st.nval );
		st.nextToken();
		setMinVolume( st.nval );
		st.nextToken();
		int nrOfLines = (int) st.nval;
		for( int i = 0 ; i < nrOfLines ; i++ )
		    {
			st.nextToken();
			double waterLevel = st.nval;
			st.nextToken();
			double volume = st.nval;
			st.nextToken();
			double outFlow = st.nval;
			addNewData( waterLevel, volume, outFlow );
		    }
		st.nextToken();// pushes ENDE
	    }// readRhbFromFile
	
	/** Retuns this object (Rhb)
	 * @return rhb*/
	public StrandData getStrandData()
	    {
		Rhb rhb = this;
		return rhb;
	    }// getStrandData
	
	/** Returns a 1d array holding strings to write the 1st row of a data block
	 * to the rhb file
	 * @return array */
	protected String[] headRhbToArray()
	    {
		String array[] = { "Function" ,String.valueOf( getInitVolume() ), String.valueOf( getMaxVolume() ),
				   String.valueOf( getMinVolume() ), String.valueOf( m_data.size() ) };
		return array;
	    }// headRhbToArray
	
	/** Returns a 1d array holding strings to write one triplet of data
	 * to the rhb file.
	 * @param i Element index ( Vector element ).
	 * @return array */
	protected String[] dataRhbToArray( int i )
	    {
		Data d = (Data) getData().elementAt( i );
		String array[] = { String.valueOf( d.waterLevel ), String.valueOf( d.resevoirVolume ),
				   String.valueOf( d.discharge ) };
		return array;
	    }// dataRhbToArray
	
	/** Writes the rhb data block to the rhb file.
	 * @param bw BufferedWriter of the rhb file.
	 * @exception Throws IOException if the buffered writer is corrupted.*/
	public	void writeRhbToFile( BufferedWriter bw ) throws IOException
	    {
		bw.write( Printf.format("%10.0s%10.6f%10.6f%10.6f%4.0d", headRhbToArray() ) );
		bw.newLine();
		for( int i = 0 ; i < getData().size() ; i ++ )
		    {
			bw.write( Printf.format("%12.2f%17.6f%14.3f", dataRhbToArray( i ) ) ) ;
			bw.newLine();
		    }
		bw.write("END");
		bw.newLine();
	    }// writeRhbToFile
	public void writeRhbToXML( BufferedWriter bw ) throws IOException
	    {
		bw.write( " m_typeName=\"" + getTypeName() + 
			  "\" m_targetNode=\""+getTargetNodeAsInt()+
			  "\" m_C=\"" + getCFact() +
			  "\" m_comment=\"" + getComment() + "\" m_initVolume=\"" +
			  + getInitVolume() + "\" m_maxVolume=\"" + getMaxVolume() +
			  "\" m_minVolume=\"" + getMinVolume() + "\" /> <v key=\"m_data\">" );
		Vector data = getData();
		Iterator it = data.iterator();
		while( it.hasNext() )
		    {
			Data d = (Data) it.next();
			bw.write( " <v_row m_WaterLevel=\"" + d.waterLevel + "\" m_Volume=\""
				  + d.resevoirVolume + "\" m_discharge=\"" + d.discharge + "\" />" );
		    }// while
		bw.write( " </v>" );
	    }// writeRhbToXML
    } // class Rhb

    
