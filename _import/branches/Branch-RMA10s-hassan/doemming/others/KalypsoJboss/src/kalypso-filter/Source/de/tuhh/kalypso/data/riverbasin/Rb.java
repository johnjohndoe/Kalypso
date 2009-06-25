package de.tuhh.kalypso.data.riverbasin;

import de.tuhh.kalypso.data.strand.Strand;
import de.tuhh.kalypso.data.RbTable;
import de.tuhh.kalypso.data.node.Node;
import de.tuhh.kalypso.data.Wc;
import de.tuhh.kalypso.util.KeyGenerator;
import de.tuhh.kalypso.util.xmlParser.GisTransferObject;
import de.tuhh.kalypso.util.xmlParser.VectorSet;

import java.util.Iterator;
import java.util.Vector;
import java.util.HashMap;
import java.io.File;
import java.io.BufferedWriter;
import java.io.IOException;
/** The class Rb.java holds the data of a riverbaisn in a catchment area. The data
 * describes the surface and underground properties of an rb. Per definition ( Kalypso )
 * a river basin is always connected to the out node of a strand. The object river basin
 * is not attached to a node but to a strand ( strand is hirarchically higher then
 * the node ). It is assumed that the rb is attached to the outnode of the rootstrand.
 *
 * @author <A HREF="mailto:c.kuepferle@tuhh.de">Christoph Küpferle</A>
 * @version Rb.java,v 1.0 2002/07/01
 */


public class Rb extends Object implements Comparable
{
	/** The field holding the number of the river basin.
	 * @param m_rbNumber */
	private int m_rbNumber = -1;
	/** The field holding the rootstrand of a rb.
	 * @param m_strandRb The strand where the rb is connected to.*/
	private Strand m_strandRb = null;
	/** This flag indicates if the river basin has hydrotopes or not.
	 * @param m_flagHydotop Default value: true.*/
	private boolean m_flagHydotop = true;
	/** The field holding the users comment
	 * @param m_comment Default value: null*/
	private String m_comment = null;
	/** The field holding the sealed area of the rb.
	 * @param m_sealedAreaRb Default value: -1.0 */
	private double m_sealedAreaRb = -1.0;
	/** The field holding the natural area ( not sealed ) of the rb.
	 * @param m_natAreaRb Default value: -1.0 */
	private double m_natAreaRb = -1.0;
	/** The field holding the correction factor for rain data.
	 * @param m_corrRain Default value: -1.0 */
	private double m_corrRain = -1.0;
	/** Temperatur dependent correction factor.
	 * @param m_corrTemp*/
	private double m_corrTemp = -1.0;
	/** Evaporation dependent correction factor.
	 * @param m_corrEvap */
	private double m_corrEvap = -1.0;
	/** The fiels holding the correction factor for the max interception.
	 * @param m_corrMaxInter Default value: -1.0. */
	private double m_corrMaxInter = -1.0;
	/** The fiels holding the correction factor for the initial interception.
	 * @param m_corrInitInter Default value: -1.0. */
	private double m_corrInitInter = -1.0;
	/** The field holding the usage correction factor, usualy 1 because it is
	 * determined in the usage file for the rb.
	 * @param m_corrUsage Default value: -1.0 */
	private double m_corrUsage = -1.0;
	/** The field holding the correction factors for all soil layers in a rb.
	 * @param m_corrFactSoil Defalult value: an empty vector. */
	private Vector m_corrFactSoil = new Vector();
	/** Parameter for the water content of the snow.
	 * @param m_snowWaterCont*/
	private double m_snowWaterCont = -1.0;
	/**Paramter for the max possible water content in the snow matrix.
	 * @param m_snowMaxWaterCont */
	private double m_snowMaxWaterCont = -1.0;
	/**  Paramter for the temperatur dependent snow melting rate.
	 * @param m_snowMeltingRateTemp*/
	private double m_snowMeltingRateTemp = -1.0;
	/**Parameter for the radiation dependent snow melting rate.
	 * @param m_snowMeltingRateRad*/
	private double m_snowMeltingRateRad = -1.0;
	/** Parameter for the initial hight of snow layer.
	 * @param m_initSnowHight*/
	private double m_initSnowHeight = -1.0;
	/** The initial  value for interception.
	 * @param m_initInterception*/
	private double m_initInterception = -1.0;
	/** The initial value for the aquifer.
	 * @param m_initAquif */
	private double m_initAquif = -1.0;
	/** This parameter is read but not written to the data base ( still to be implemented )
	 @param m_fInit Not in use	 */
	private double m_fInit = 0.0; // this value is always zero because not implemented
	/** This parameter is read but not written to the data base ( still to be implemented )
	 * @param m_fTra Not in use*/
	private double m_fTra = 0.0;  // this value is always zero because not implemented
	/** Retention factor for the sealed surface area portion.
	 * @param m_retSealedArea */
	private double m_retSealedArea = -1.0;
	/** Retention factor for the overland flow portion.
	 * @param m_retOverlandFlow */
	private double m_retOverlandFlow = -1.0;
	/** Retention factor for interflow portion.
	 * @param m_retInterflow */
	private double m_retInterflow = -1.0;
	/** Retention factor for the aquifer for the portion in the channel.
	 * @param m_retAquif */
	private double m_retAquif = -1.0;
	/** Retention factor for the aquifer of the naighbouring aquifer.
	 * @param m_retAquifNaighbour */
	private double m_retAquifNeighbour = -1.0;
	/** Retention factor for the low aquifer.
	 * @param m_retOutflowAquifDeep*/
	private double m_retOutflowAquifDeep = -1.0;
	/** The field holding all river basins connected through aquifers. This rb
	 * discharges water to the rbs in the Vector.
	 * @param m_outConRb Default value: null ( Vector ) */
	private Vector m_outConRb = null;
	/** The field holding data for the shallow and deep aquifer layers in the rb.
	 * @param m_aquifData Defalult value: null */
	//private AquifData m_aquifData = null;
	/** Minimal height of channel (aquifer).
	 * @param m_aquifMinHeightChannel */
	private double m_aquifMinHeightChannel = -1.0;
	/** Maximal height of channel (aquifer).
	 * @param m_aquifMaxHeightChannel */
	private double m_aquifMaxHeightChannel = -1.0;
	/** Correction factor, spliting water flow for shallow aquifer and the deep aquifer.
	 * @param m_aquifSplitAquiferDeep */
	private double m_aquifSplitAquiferDeep = -1.0;
	/** Porosity of the shallow aquifer.
	 * @param m_aquifPorosity  ( 0 < porosity < 1 )*/
	private double m_aquifPorosity = -1.0;
	/** Extracted discharge from shallow aquifer.
	 * @param m_aquifExtact [m³/s].*/
	private double m_aquifExtract = -1.0;
	/** Rate of inflow to the deep aquifer.
	 * @param m_aquifInflowDeepAquif ( 0 < inflow < 1 )*/
	private double m_aquifInflowDeepAquif = -1.0;
	/**  The Rb only has a target Strand as a DeepAquifer, per definition the Rb
	 * drains to the InNode of the targetStrandDeepAquifer!! Is already implemented
	 * in the getDeepAquiferTargetNode() method
	 * @param m_aquifTargetStrandDeepAquif Target node wher the deep aquifer discharges to.*/
	private Strand m_aquifTargetStrandDeepAquif = null;
	/** The field holding the info if the rb is calculated using measured rain series,
	 * or statistically calcualted rain seres. m_key = n ( measured series ),
	 * m_key = s ( statistical series )
	 * @param m_key Defalult value: null */
	private String m_key = null;
	/** File with cliamte data for the rb.
	 * @param m_fileClimate Defalult value: null */
	private File m_fileClimate = null;
	/** File with long term simulation data for the rb.
	 * @param m_fileLongTermSim Defalult value: null */
	private File m_fileLongTermSim = null;
	/** File with short term simulation data for the rb.
	 * @param m_fileShortTermSim Defalult value: null */
	private File m_fileShortTermSim = null;
	/** File with time area funciton data for the rb.
	 * @param m_fileTimeAreaFunct Defalult value: null */
	private File m_fileTimeAreaFunct = null;
	/** File with hydrotope data for the rb.
	 * @param m_hydrotopFile Defalult value: null */
	private File m_fileHydrotop = null;
	
	public Rb( int rbNumber , Strand sNumber )
	{
		this.m_rbNumber = rbNumber;
		this.m_strandRb = sNumber;
	}
	public Rb( int rbNumber )
	{
		this.m_rbNumber = rbNumber;
	}
	/** This method cenerates a unique Hash Code for a river basin object. It is
	 * composed of the numer of the river basin puls the strands WcIndex where the
	 * river basin is attached to.
	 * @return hashCode The unique hash code of a river basin object.
	 */
	public int hashCode()
	{
		String hashString = m_rbNumber + m_strandRb.getWcIndex().getWcIndex();
		return hashString.hashCode();
	}// hashCode
	/** Sets the number of rb.
	 * @param number Number of rb. */
	public void setNumberRb( int number ) { m_rbNumber = number ; }
	/** Sets the users comment of rb.
	 * @param comment Users Comment 2nd line in ***.geb file. */
	public void setComment( String comment )
	{
		if( m_comment == null )
			m_comment = comment ;
		else m_comment = m_comment + " " + comment;
	}
	/** Gets the users comment of rb.
	 * @return m_comment Users Comment 2nd line in ***.geb file. */
	public String getComment() { return m_comment; }
	/** Returns the number of rb as interger.
	 * @return m_rbNumber */
	public int getNumberRb() { return m_rbNumber; }
	/** Returns the rootstrand of the rb ( where rb is attached to ).
	 * @return m_strandRb */
	public Strand getStrandRb () { return m_strandRb; }
	/** Sets the rootstrand of rb ( where rb is attached to ).
	 * @param s Rootstrand of rb. */
	public void setStrandRb( Strand s ) { m_strandRb = s;}
	/** Sets the hydrotope flag of rb. If the flag in the file equals to seven the
	 * flag is set true otherwise fault.
	 * @param flag Interger for flag. */
	public void setFlagHydrotop( int flag ) { if( flag == 7 ) m_flagHydotop = true; else m_flagHydotop = false; }
	public void setFlagHydrotop( String flag ) { if( flag.equals( "true" )) m_flagHydotop = true; else m_flagHydotop = false; }
	/** Returns the hydrotope flag as an integer. returns 7 if true, otherwise returns -1.
	 * @return integer */
	public boolean getFlagHydrotop() { return m_flagHydotop; }
	/** Returns watercourse index of rb ( meaning wcIndex of rootstrand ).
	 * @return wc Wc of rb. */
	public Wc getWcIndexRb () { return m_strandRb.getWcIndex(); }
	/** Sets the value of the sealed area of rb.
	 * @param area */
	public void setSealedAreaRb( double area ) { m_sealedAreaRb = area ; }
	/** Returns the value of sealed area of rb.
	 * @return m_sealedAreaRb */
	public double getSealedAreaRb() { return m_sealedAreaRb; };
	/** Sets the value of the natural ( not sealed ) area of rb.
	 * @param area */
	public void setNatAreaRb( double area ) { m_natAreaRb = area; }
	/** Returns the value of natural ( not sealed ) area of rb.
	 * @return m_natAreaRb */
	public double getNatAreaRb() { return m_natAreaRb; }
	/** Sets the rain correction factor for rb.
	 * @param value */
	public void setCorrRain( double value ) { m_corrRain = value; }
	/** Returns the rain correction factor for rb.
	 * @return m_corrRain */
	public double getCorrRain () { return m_corrRain; }
	
	/** Sets the evaporation correction factor for rb.
	 * @param value */
	public void setCorrEvap( double value ) { m_corrEvap = value; }
	/** Gets the evaporation correction factor for rb.
	 * @return m_corrEvap */
	public double getCorrEvap() { return m_corrEvap; }
	/** Sets the temperature correction factor for rb.
	 * @param value */
	public void setCorrTemp( double value ) { m_corrTemp = value; }
	/** Gets the evaporation correction factor for rb.
	 * @return m_corrTemp */
	public double getCorrTemp() { return m_corrTemp; }
	
	/** Sets the correction factor for the max interception.
	 * @param value */
	public void setCorrMaxInter ( double value ) { m_corrMaxInter = value; }
	/** Retruns the correction factor for the max interception.
	 * @return m_corrMaxInter */
	public double getCorrMaxInter() { return m_corrMaxInter; }
	/** Sets the correction factor for the inital value for interception.
	 * @param value */
	public void setCorrInitInter( double value ) { m_corrInitInter = value; }
	/** Returns the correction factor for the inital value for interception.
	 * @param m_corrInitInter */
	public double getCorrInitInter() { return m_corrInitInter; }
	/** Sets the correction factor for land useage.
	 * @param value */
	public void setCorrUsage( double value ) { m_corrUsage = value; }
	/** Returns the correction factor for land usage.
	 * @return m_useageCorr */
	public double getCorrUsage() { return m_corrUsage; }
	/** Returns the ratio of sealed area to total area of the rb.
	 * @return double sealing ratio. A value between 1 and 0. */
	public double getSealingFact() { return m_sealedAreaRb/( m_sealedAreaRb + m_natAreaRb ); }
	/** Returns the total area of the rb.
	 * @return double The sum of sealed area puls natural area. */
	public double getTotAreaRb() { return m_sealedAreaRb + m_natAreaRb; }
	/** Returns the number of layers definde by the correction factors for soil.
	 * @return integer The size of the Vector containing all layers ( Correction factor ).*/
	public int getNrOfLayerCorrFactSoil() { return m_corrFactSoil.size(); }
	/** Sets the variable m_key.
	 * @param s Either an "n" or a "s", for meaning see fieldvalue m_key. */
	public void setKey( String s ) { m_key = s; }
	/** Gets the variable m_key.
	 * @retrun m_key Default value: null. */
	public String getKey() { return m_key; }
	/** Creates an new instant of File ( for long term simulation data file )with
	 * the filename as String and assigns it to m_longTermSimFile varaible.
	 * @param filename The parsed filename. */
	public void setFileLongTerm( String filename ) { m_fileLongTermSim = new File( filename); }
	/** Creates an new instant of File ( for short term simulation data file )with
	 * the filename as String and assigns it to m_shortTermSimFile varaible.
	 * @param filename The parsed filename. */
	public void setFileShortTerm( String filename ) { m_fileShortTermSim = new File( filename); }
	/** Creates an new instant of File ( for time area function data file )with
	 * the filename as String and assigns it to m_timeAreaFunctionFile varaible.
	 * @param filename The parsed filename. */
	public void setFileTimeAreaFunct( String filename ) { m_fileTimeAreaFunct = new File( filename); }
	/** Creates an new instant of File ( for hydrotope data file )with
	 * the filename as String and assigns it to m_hydrotopFile varaible.
	 * @param filename The parsed filename. */
	public void setFileHydrotop( String filename ) { m_fileHydrotop = new File( filename );}
	/** Creates an new instant of File ( for climate data file )with
	 * the filename as String and assigns it to m_climateFile varaible.
	 * @param filename The parsed filename. */
	public void setFileClimate( String filename ) { m_fileClimate = new File( filename );}
	/** Returns the file for long term simulation data.
	 * @return m_longTermSimFile */
	public File getFileLongTerm() { return m_fileLongTermSim; }
	/** Returns the file for short term simulation data.
	 * @return m_shortTermSimFile */
	public File getFileShortTerm() { return m_fileShortTermSim; }
	/** Returns the file for time area function data.
	 * @return m_longTermSimFile */
	public File getFileTimeAreaFunct() { return m_fileTimeAreaFunct; }
	/** Returns the file for the hydrotope data.
	 * @return m_longTermSimFile */
	public File getFileHydrotop() { return m_fileHydrotop; }
	/** Returns the file for climate data.
	 * @return m_longTermSimFile */
    public File getFileClimate() { return m_fileClimate; }
	/** Returns the String of the river basin number.
	 * @return m_rbNumber As a string. */
	public String toString()
	{
		return "" + m_rbNumber + m_strandRb.getWcIndex();
	}// toString
	/** This method creates a new ( empty ) Vector to add connected rb to ( aquifer net ).*/
	public void createOutConRb() { m_outConRb = new Vector(); }
	/** Returns the Vector m_outConRb, containing all connected rb through aquifers.
	 * @return m_outConRb ( Vector containing rbs )*/
	public Vector getOutConRbVect() { return m_outConRb; }
	/** Returns the number of connected river basins to this river basin ( aquifer net ).
	 * @return integer The number of rb, if there are none returns zero.*/
	public int getNrOfConRb()
	{
		if( m_outConRb == null )
			return 0;
		else
			return m_outConRb.size();
	}// getNrOfConRb
	/** This method gets a RbTable of all connected river basin to a river basin.
	 * @return rbConTable A Table of all connected river basins, null if no conRbs exist.
	 */
	public RbTable getOutConRbRbTable()
	{
		RbTable rbConTable = new RbTable();
		if(m_outConRb != null )
		{
			Iterator it = m_outConRb.iterator();
			while( it.hasNext() )
			{
				RbOutConnected rbCon = ( RbOutConnected ) it.next();
				rbConTable.add( rbCon.conRb );
			}// while
			return rbConTable;
		}// if
		return null;
	}// getOutConRbRbTable
	/** This method implements the compareTo method for the TreeSet. It compares
	 * this object with the specified object for order. Returns a negative integer,
	 * zero, or a positive integer as this object is less than, equal to,
	 * or greater than the specified object.
	 * @param other The object to be compared to this object.
	 * @return int Returns a -1/0/1 if the node number of the other object is larger/equal/smaller then the node number of this object.
	 */
	public int compareTo( Object other  )
	{
		if( other instanceof Rb && ( (Rb) other ).m_rbNumber > m_rbNumber )
			return -1;
		if( other instanceof Rb && ( (Rb) other ).m_rbNumber < m_rbNumber )
			return 1;
		else
			return 0;
	}// compareTo
	
	/** This inner class contains parameters for one soil layer in a river basin.
	 * If the model is run using Hydrotopes these values become soil correction
	 * factors for a layer in the total river basin ( not for a Hydrotope ).
	 */
	public class CorrFactSoilLayer
	{
		/** Infiltration parameter for short time simulation ( or correction factor ).
		 * @param infiltShortTime */
		private double infiltShortTime = -1.0;
		/** Infiltration parameter daily value ( or correction factor ).
		 * @param infiltDailyVal */
		private double infiltDailyVal = -1.0;
		/** Perkulation for a soil layer ( or correction factor ).
		 * @param perkForLayer */
		private double perkForLayer = -1.0;
		/** Maximum pore volume of the soil layer ( or correction factor ).
		 * @param maxPoreVolume */
		private double maxPoreVolume = -1.0;
		/** Initial moisture of the soil layer ( or correction factor ).
		 * @param initialMoisture */
		private double initialMoisture = -1.0;
		/** Value for the useable field capacity of the soil layer ( or correction factor ).
		 * @param usableFieldCap */
		private double usableFieldCap = -1.0;
		/** Retention constant for interflow in the soil layer ( or correction factor ).
		 * @param retLayer */
		private double retLayerInterflow = -1.0;
		/** Not in use in Kalypso.
		 * @param evaLayer not used*/
		private double evapLayer = -1.0;
		
	}// Inner class CorrFactSoil
	
	/** This method sets the parameter for a soil layer, and adds it to the member
	 * variable m_corrFactSoil of Type Vector containing all the layers for a river
	 * basin.
	 * @param v Vector conntaining a set of data for one soil layer, either real parameters or correction factors.
	 */
	public void setCorrFactSoil ( Vector v )
	{
		CorrFactSoilLayer cFactLay = new CorrFactSoilLayer();
		Iterator it = v.iterator();
		cFactLay.infiltShortTime = ( ( Double )it.next()).doubleValue();
		cFactLay.infiltDailyVal = ( ( Double )it.next()).doubleValue();
		cFactLay.perkForLayer = ( ( Double )it.next()).doubleValue();
		cFactLay.maxPoreVolume = ( ( Double )it.next()).doubleValue();
		cFactLay.initialMoisture = ( ( Double )it.next()).doubleValue();
		cFactLay.usableFieldCap = ( ( Double )it.next()).doubleValue();
		cFactLay.retLayerInterflow = ( ( Double )it.next()).doubleValue();
		cFactLay.evapLayer = ( ( Double )it.next()).doubleValue();
		m_corrFactSoil.addElement( cFactLay );
	}// setCorrFactSoil
	public void setCorrFactSoil ( HashMap map )
	{
		CorrFactSoilLayer cFactLay = new CorrFactSoilLayer();
		Iterator keySet = map.keySet().iterator();
		while( keySet.hasNext() )
		{
			String key = (String) keySet.next();
			if( key.equals( "v_infiltShortTime" ))
				cFactLay.infiltShortTime = Double.parseDouble((String)  map.get( key ) );
			if( key.equals( "v_infiltDailyVal" ))
				cFactLay.infiltDailyVal = Double.parseDouble((String)  map.get( key ) );
			if( key.equals( "v_perkForLayer" ))
				cFactLay.perkForLayer = Double.parseDouble((String)  map.get( key ) );
			if( key.equals( "v_maxPoreVolume" ))
				cFactLay.maxPoreVolume = Double.parseDouble((String)  map.get( key ) );
			if( key.equals( "v_initialMoisture" ))
				cFactLay.initialMoisture = Double.parseDouble((String)  map.get( key ) );
			if( key.equals( "v_usableFieldCap" ))
				cFactLay.usableFieldCap = Double.parseDouble((String)  map.get( key ) );
			if( key.equals( "v_retLayerInterflow" ))
				cFactLay.retLayerInterflow = Double.parseDouble((String)  map.get( key ) );
			if( key.equals( "v_evapLayer" ))
				cFactLay.evapLayer = Double.parseDouble((String)  map.get( key ) );
		}
		m_corrFactSoil.addElement( cFactLay );
	}// setCorrFactSoil
	/** This method returns an object CorrFactSoilLayer from the layer i.
	 * @param layer layer number, 1 is top layer and all layers below have asscendig indecies.
	 * @return c The CorrFactSoilLayer at layer i.*/
	public CorrFactSoilLayer getCorrFactSoilLayer( int i )
	{
		CorrFactSoilLayer c = ( CorrFactSoilLayer )m_corrFactSoil.elementAt( i );
		return c;
	}// getCorrFactSoilLayer
	
	/** This class contains the Data for river basins connected through an aquifer.*/
	public class RbOutConnected
	{
		/** The connected riverbasin to this river basin.
		 * @param numberOfConRb */
		private Rb conRb = null;
		/** Percentage ( value between 0 and 1 ) of the outflow to the connected river basin.
		 * @param percentOfOutFlow */
		private double percentOfOut = -1.0;
		
		public RbOutConnected (){};
		
		public RbOutConnected ( Rb rb, double value )
		{
			this.conRb = rb;
			this.percentOfOut = value;
		}
		public Rb getConRb() { return conRb; }
		public double getConRbValue() { return percentOfOut; }
	}// Inner class RbOutConnected
	
	/** Adds an Object of the class RbOutConnected to the memeber variable
	 * m_outConRb of the river basin.
	 * @param numberRb River basin number of the connected Rib.
	 */
	public void createRbOutConnected( Rb rbCon, double value )
	{
		if( getOutConRbVect() != null )
		{
			RbOutConnected r = new RbOutConnected ( rbCon, value );
			m_outConRb.addElement( r );
		}
		else
		{
			Vector vect = new Vector();
			RbOutConnected r = new RbOutConnected ( rbCon, value );
			vect.addElement( r );
			m_outConRb = vect;
		}
	}// createRbOutConnected
	
	
	/** This method gets a connected river basin.
	 * @param rbConNumber The number of the connecting Rib.
	 * @return r If it exists returing connected river basin, otherwise null is returned.
	 */
	public RbOutConnected getConRb( Rb rbCon )
	{
		Iterator it = m_outConRb.iterator();
		while ( it.hasNext() )
		{
			RbOutConnected r = ( RbOutConnected ) it.next();
			if( r.conRb == rbCon )
				return r;
		}// while
		return null;
	}// getConRb
	
	/** This method gets a connected river basin with the number ( conRbNr ) of the
	 * river basin.
	 * @param rbConNumber The number of the connecting Rib.
	 * @return r If it exists returing connected river basin, otherwise null is returned.
	 */
	public Rb getConRbNr( int conRbNr )
	{
		Iterator it = m_outConRb.iterator();
		while ( it.hasNext() )
		{
			RbOutConnected rb = ( RbOutConnected ) it.next();
			if( rb.conRb.getNumberRb() == conRbNr )
				return rb.conRb;
		}// while
		return null;
	}// getConRbNr
	public boolean isDeepAquifExisting()
	{
		if( getAquifTargetStrandDeepAquif() != null )
			return true;
		return false;
	}
	public void setAquifExtract( double value ) { m_aquifExtract = value; }
	public double getAquifExtract() { return m_aquifExtract; }
	public void setAquifPorosity( double value ) { m_aquifPorosity = value ; }
	public double getAquifPorosity() { return m_aquifPorosity; }
	public void setAquifSplitAquiferDeep( double value ) { m_aquifSplitAquiferDeep = value; }
	public double getAquifSplitAquiferDeep() { return m_aquifSplitAquiferDeep; }
	public void setAquifMinHeightChannel( double value ) { m_aquifMinHeightChannel = value; }
	public double getAquifMinHeightChannel() { return m_aquifMinHeightChannel; }
	public void setAquifMaxHeightChannel( double value ) { m_aquifMaxHeightChannel = value; }
	public double getAquifMaxHeightChannel() { return m_aquifMaxHeightChannel; }
	public void setAquifInflowDeepAquifVal( double value ) { m_aquifInflowDeepAquif = value; }
	public double getAquifInflowDeepAquifVal() { return m_aquifInflowDeepAquif; }
	/** Sets the strand s as the targetStrandDeepAquif of the river basin.
	 * @param s Strand.*/
	public void setAquifTargetStrandDeepAquif( Strand s ) { m_aquifTargetStrandDeepAquif = s; }
	/** Returns the deep aquifer target strand of the riverbasin.
	 * @return m_aquifTargetStrandDeepAquif .*/
	public Strand getAquifTargetStrandDeepAquif() { return m_aquifTargetStrandDeepAquif; }
	/** Sets the retention factor for the sealed area.
	 * @param value */
	public void setRetSealedArea( double value ) { m_retSealedArea = value; }
	/** Gets the retention factor for the sealed area.
	 * @return m_retSealedArea */
	public double getRetSealedArea() { return m_retSealedArea; }
	/** Sets the retention factor for the overland flow portion.
	 * @param value */
	public void setRetOverlandFlow( double value ) { m_retOverlandFlow = value; }
	/** Gets the retention factor for the overland flow portion.
	 * @return m_retOverlandFlow */
	public double getRetOverlandFlow() { return m_retOverlandFlow; }
	/** Sets the retention factor for interflow portion.
	 * @param value */
	public void setRetInterflow ( double value ) { m_retInterflow = value; }
	/** Gets the retention factor for interflow portion.
	 * @return m_retInterflow */
	public double getRetInterflow () { return m_retInterflow; }
	/** Sets the retention factor for the aquifer for the portion in the channel.
	 * @param value */
	public void setRetAquif( double value ) { m_retAquif = value; }
	/** Gets the retention factor for the aquifer for the portion in the channel.
	 * @return m_retAquif */
	public double getRetAquif() { return m_retAquif; }
	/** Sets the retention factor for the aquifer of the naighbouring aquifer.
	 * @param value */
	public void setRetAquifNeighbour( double value ) { m_retAquifNeighbour = value; }
	/** Gets the retention factor for the aquifer of the naighbouring aquifer.
	 * @return m_retAquifNaighbour */
	public double getRetAquifNeighbour() { return m_retAquifNeighbour; }
	/** Sets the retention factor for the low aquifer.
	 * @param value*/
	public void setRetOutflowAquifDeep( double value ) { m_retOutflowAquifDeep = value; }
	/** Sets the retention factor for the low aquifer.
	 * @return m_retOutflowAquifDeep*/
	public double getRetOutflowAquifDeep() { return m_retOutflowAquifDeep; }
	
	/** Sets the value for the initial hight of snow layer.
	 * @param value*/
	public void setInitSnowHeight( double value ) { m_initSnowHeight = value; }
	/** Gets the value for the initial hight of snow layer.
	 * @return m_initSnowHeight*/
	public double getInitSnowHight() { return m_initSnowHeight; }
	/** Sets the initial  value for interception.
	 * @param value*/
	public void setInitInterception ( double value ){ m_initInterception = value; }
	/** Gets the initial  value for interception.
	 * @return m_initInterception*/
	public double getInitInterception (){ return m_initInterception; }
	/** Sets the initial value for the aquifer.
	 * @param value*/
	public void setInitAquif( double value ) { m_initAquif = value; }
	/** Sets the initial value for the aquifer.
	 * @return m_initAquif */
	public double getInitAquif() { return m_initAquif; }
	/** Empty set method for unused parameter finit (default always 0.0)
	 @param none Not in use	 */
	public void setfInit() { ;}
	/** Gets finit, is always 0.0 because not in use.
	 @return m_fInit Always 0.0*/
	public double getfInit() { return m_fInit; }
	/** Empty set method for unused parameter ftra (default always 0.0)
	 @param none Not in use	 */
	public void setfTra() { ;}
	/** Gets ftra, is always 0.0 because not in use.
	 @return m_fInit Always 0.0*/
	public double getfTra() { return m_fTra; }
	/** Sets the water content in the snow matrix.
	 * @param value */
	public void setSnowWaterCont( double value ) { m_snowWaterCont = value; }
	/** Gets the water content in the snow matrix.
	 * @return m_snowMaxWaterCont */
	public double getSnowWaterCont() { return m_snowWaterCont; }
	/** Sets the max. possible water content in the snow matrix.
	 * @param value */
	public void setSnowMaxWaterCont( double value ) { m_snowMaxWaterCont = value; }
	/** Gets the max. possible water content in the snow matrix.
	 * @return m_snowMaxWaterCont */
	public double getSnowMaxWaterCont() { return m_snowMaxWaterCont; }
	/** Sets the temperatur dependent snow melting rate.
	 * @param value*/
	public void setSnowMeltingRateTemp( double value ) { m_snowMeltingRateTemp = value; }
	/** Gets the temperatur dependent snow melting rate.
	 * @return m_snowMeltingRateTemp*/
	public double getSnowMeltingRateTemp() { return m_snowMeltingRateTemp; }
	/** Sets the radiation dependent snow melting rate.
	 * @param value*/
	public void setSnowMeltingRateRad( double value ) { m_snowMeltingRateRad = value; }
	/** Gets the radiation dependent snow melting rate.
	 * @return m_snowMeltingRateRad*/
	public double getSnowMeltingRateRad() { return m_snowMeltingRateRad ; }
	/** Returns a 1d array containing the snow data in the right order to write
	 * to the river basin file.
	 * @return array */
	public String[] snowDataToArray()
	{
		String array[] = { String.valueOf( m_snowWaterCont ),
				String.valueOf( m_snowMaxWaterCont ), String.valueOf( m_snowMeltingRateTemp ),
				String.valueOf( m_snowMeltingRateRad ), String.valueOf( m_initSnowHeight ),
				String.valueOf( m_corrTemp ), String.valueOf( m_corrEvap ) };
		return array;
	}// snowDataToArray
	/** Returns a 1d array containing the correction factors for one soil layer
	 * data in the right order to write to the river basin file.
	 * @return array */
	public String[] corrFactSoilLayerToArray( int layer )
	{
		String array[] = { String.valueOf( getCorrFactSoilLayer( layer ).infiltShortTime ),
				String.valueOf( getCorrFactSoilLayer( layer ).infiltDailyVal ),
				String.valueOf( getCorrFactSoilLayer( layer ).perkForLayer ),
				String.valueOf( getCorrFactSoilLayer( layer ).maxPoreVolume ),
				String.valueOf( getCorrFactSoilLayer( layer ).initialMoisture ),
				String.valueOf( getCorrFactSoilLayer( layer ).usableFieldCap ),
				String.valueOf( getCorrFactSoilLayer( layer ).retLayerInterflow ),
				String.valueOf( getCorrFactSoilLayer( layer ).evapLayer ) };
		return array;
	}// corrFactSoilLayerToArray
	/** Returns a 1d array containing the initial values in the right order to write
	 * to the river basin file.
	 * @return array */
	public String[] initialValToArray()
	{
		String array[] = { String.valueOf( getCorrUsage() ), String.valueOf( getInitInterception() ),
				String.valueOf( getInitAquif() ), String.valueOf( getfInit() ),
				String.valueOf( getfTra() ) };
		return array;
	}// initialValToArray
	/** Returns a 1d array containing the aquifer data in the right order to write
	 * to the river basin file.
	 * @return array */
	public String[] aquifDataToArray()
	{
		if( isDeepAquifExisting() == true )
		{
			String array[] = { String.valueOf( getAquifMinHeightChannel() ),
					String.valueOf( getAquifMaxHeightChannel() ), String.valueOf( getAquifSplitAquiferDeep() ),
					String.valueOf( getAquifPorosity() ), String.valueOf( getAquifExtract() ),
					String.valueOf( getAquifInflowDeepAquifVal() ),
					String.valueOf( getAquifTargetStrandDeepAquif().getNodeIn().getNodeNr() ),
					getAquifTargetStrandDeepAquif().getWcIndex().getWcIndex() };
			return array;
		}// if
		
		String array[] = { String.valueOf( getAquifMinHeightChannel() ),
				String.valueOf( getAquifMaxHeightChannel() ), String.valueOf( getAquifSplitAquiferDeep() ),
				String.valueOf( getAquifPorosity() ), String.valueOf( getAquifExtract() ), "0", "0" };
		return array;
	}// aquifDataToArray
	/** Returns a 1d array containing the connected river basins ( rbNumber )
	 * data in the right order to write to the river basin file.
	 * @return array or null if there is no object*/
	public String[] conRbToArrayNr()
	{
		String array[] = new String[ getNrOfConRb()];
		
		if( m_outConRb != null )
		{
			Iterator it =  m_outConRb.iterator();
			for( int i = 0 ; i < getNrOfConRb() ; i++ )
			{
				RbOutConnected rbOutCon = (RbOutConnected) it.next();
				array[i] = String.valueOf( rbOutCon.conRb.getNumberRb() );
			}// while
			return array;
		}// if
		return null;
	}// conRbToArrayNr
	/** Returns a 1d array containing the connected river basin wcIndex
	 * data in the right order to write to the river basin file.
	 * @return array or null if there is no object*/
	public String[] conRbToArrayWcIndex()
	{
		String array[] = new String[ getNrOfConRb()];
		
		if( m_outConRb != null )
		{
			Iterator it =  m_outConRb.iterator();
			for( int i = 0 ; i < getNrOfConRb() ; i++ )
			{
				RbOutConnected rbOutCon = (RbOutConnected) it.next();
				array[i] = rbOutCon.conRb.getStrandRb().getWcIndex().getWcIndex();
			}// for
			return array;
		}// if
		return null;
	}// conRbToArrayWcIndex
	/** Returns a 1d array containing the connected river basin data ( values )
	 * in the right order to write to the river basin file.
	 * @return array or null if there is no object */
	public String[] conRbValToArray()
	{
		int nrOfConRb = m_outConRb.size();
		String array[] = new String[ nrOfConRb ];
		Iterator it =  m_outConRb.iterator();
		for( int i = 0 ; i < nrOfConRb ; i++ )
		{
			RbOutConnected rbOutCon = (RbOutConnected) it.next();
			array[i] = String.valueOf( rbOutCon.percentOfOut );
		}// for
		return array;
	}// conRbValToArray
	/** Returns a 1d array containing the retention data in the right order
	 * to write to the river basin file.
	 
	 * @return array */
	public String[] retConstToArray()
	{
		
		String arrayRetConst[] = { String.valueOf( getRetSealedArea() ),
				String.valueOf( getRetOverlandFlow() ), String.valueOf( getRetInterflow() ),
				String.valueOf( getRetAquif() ), String.valueOf( getRetAquifNeighbour() ),
				String.valueOf( getRetOutflowAquifDeep() )};
		return arrayRetConst;
	}// retConstToArray
	/** Returns a 1d array containing the interception data in the right order
	 * to write to the river basin file.
	 * @return array */
	public String[] interceptionToArray()
	{
		String array[] = { String.valueOf( getSealingFact() ), String.valueOf( getNrOfLayerCorrFactSoil() ),
				String.valueOf( m_corrMaxInter ), String.valueOf( m_corrInitInter ) };
		return array;
	}// interceptionToArray
	public void mapRbCorrFact( GisTransferObject gto )
	{
		VectorSet vs = gto.getVectorSet( "m_corrFactSoil" );
		Vector values = new Vector();
		Vector key = new Vector();
		
		if( vs != null )
		{
			key = vs.getSimplePropertyKeys( 0 );
			HashMap map = new HashMap();
			//values.clear();
			for( int i = 0; i < vs.size(); i++ )
			{
				
				for( int j = 0; j < key.size(); j++ )
				{
					map.put( key.elementAt( j ), vs.getSimpleProperty( (String) key.get( j ), i ));
					//values.add( Double.valueOf( vs.getSimpleProperty( (String) key.get( j ), i )));
				}
				//setCorrFactSoil( values );
				setCorrFactSoil( map );
				//values.clear();
			}
		}
	}// mapRb
	public void writeRbToXML( BufferedWriter bw ) throws IOException
	{
		bw.write( " <o ID=\"" + hashCode() + "\">" + " <sp m_rbNumber=\"" + getNumberRb()
					 + "\" m_comment=\"" + getComment()
					 + "\" m_sealedAreaRb=\"" + getSealedAreaRb()
					 + "\" m_natAreaRb=\"" + getNatAreaRb()
					 + "\" m_corrRain=\"" + getCorrRain()
					 + "\" m_corrMaxInter=\"" + getCorrMaxInter()
					 + "\" m_corrInitInter=\"" + getCorrInitInter()
					 + "\" m_corrUsage=\"" + getCorrUsage()
					 + "\" m_key=\"" + getKey()
					 + "\" m_fileClimate=\"" + getFileClimate()
					 + "\" m_fileLongTermSim=\"" + getFileLongTerm()
					 + "\" m_fileShortTermSim=\"" + getFileShortTerm()
					 + "\" m_fileTimeAreaFunct=\"" + getFileTimeAreaFunct()
					 + "\" m_fileHydrotop=\"" + getFileHydrotop() );
		if( getFlagHydrotop() == true )
			bw.write( "\" m_flagHydrotop=\"true\"" );
		else bw.write( "\" m_flagHydrotop=\"false\"" );
		bw.write( " m_aquifMaxHeightChannel=\"" + getAquifMaxHeightChannel()
					 + "\" m_aquifMinHeightChannel=\"" + getAquifMinHeightChannel()
					 + "\" m_aquifExtract=\"" + getAquifExtract()
					 + "\" m_aquifPorosity=\"" + getAquifPorosity()
					 + "\" m_aquifSplitAquiferDeep=\"" + getAquifSplitAquiferDeep()
					 + "\" m_fInit=\"" + getfInit()
					 + "\" m_fTar=\"" + getfTra()
					 + "\" m_initAquif=\"" + getInitAquif()
			  + "\" m_initInterception=\"" + getInitInterception()
  			                 + "\" m_retAquif=\"" + getRetAquif()
					 + "\" m_retAquifNeighbour=\"" + getRetAquifNeighbour()
					 + "\" m_retInterflow=\"" + getRetInterflow()
					 + "\" m_retOutflowAquifDeep=\"" + getRetOutflowAquifDeep()
					 + "\" m_retOverlandFlow=\"" + getRetOverlandFlow()
					 + "\" m_retSealedArea=\"" + getRetSealedArea()
					 + "\" m_corrEvap=\"" + getCorrEvap()
					 + "\" m_corrTemp=\"" + getCorrTemp()
					 + "\" m_initSnowHeight=\"" + getInitSnowHight()
					 + "\" m_snowMaxWaterCont=\"" + getSnowMaxWaterCont()
					 + "\" m_snowMeltingRateRad=\"" + getSnowMeltingRateRad()
					 + "\" m_snowMeltingRateTemp=\"" + getSnowMeltingRateTemp()
					 + "\" m_snowWaterCont=\"" + getSnowWaterCont() + "\"/>" );
		bw.write( " <v key=\"m_corrFactSoil\">" );
		for( int i = 0; i < m_corrFactSoil.size(); i++ )
		{
			CorrFactSoilLayer layer = getCorrFactSoilLayer( i );
			bw.write( " <v_row v_infiltShortTime=\"" + layer.infiltShortTime
						 + "\" v_infiltDailyVal=\"" + layer.infiltDailyVal
						 + "\" v_perkForLayer=\"" + layer.perkForLayer
						 + "\" v_maxPoreVolume=\"" + layer.maxPoreVolume
						 + "\" v_initialMoisture=\"" + layer.initialMoisture
						 + "\" v_usableFieldCap=\"" + layer.usableFieldCap
						 + "\" v_retLayerInterflow=\"" + layer.retLayerInterflow
						 + "\" v_evapLayer=\"" + layer.evapLayer + "\"/>" );
		}
		bw.write( " </v>" );
	}// writeRbToXML
} // class Rb

