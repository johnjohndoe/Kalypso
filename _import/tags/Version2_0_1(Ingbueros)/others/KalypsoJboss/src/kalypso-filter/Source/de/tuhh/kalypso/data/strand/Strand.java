package de.tuhh.kalypso.data.strand;

import java.io.BufferedWriter;
import java.io.IOException;

import de.tuhh.kalypso.data.NodeTable;
import de.tuhh.kalypso.data.RbTable;
import de.tuhh.kalypso.data.Wc;
import de.tuhh.kalypso.data.node.Node;
import de.tuhh.kalypso.data.riverbasin.Rb;
import de.tuhh.kalypso.util.KeyGenerator;
import de.tuhh.kalypso.util.LogFile;
import de.tuhh.kalypso.util.Printf;

/** The Class Strand.java holds the data for a strand of the rainfall-run-off-model
 * Kalypso. A strand always connects two nodes and it can have four differnet
 * hydraulic properties, just one and not a combination of them.
 * <UL>
 * <LI>No Retention - NullChannel
 * <LI>A natural channel - Channel
 * <LI>A reservoir - Rhb
 * <LI>A reservoir with evaporation form the surface - Rht
 * </UL>
 * Strands only describe surface water and reservoir elements.
 *
 * @author <A HREF="mailto:c.kuepferle@tuhh.de">Christoph Küpferle</A>
 * @version Strand.java,v 1.0 2002/07/01
 */
public class Strand implements Comparable
{
    /** The field holding the number of the strand.
     * @param m_strandNr */
    private int m_strandNr = -1;
    /** Member Holding the In node of an Strand @see Node.
     * @param m_nodeIn The inflow node of the strand.*/
    private Node m_nodeIn = null;
    /** Member Holding the Out node of an Strand
     * <BR>
     * @see  de.tuhh.kalypso.node.Node#  Node.
     * @param m_nodeIn The outflow node of the strand.*/
    private Node m_nodeOut = null;
    /** Member holding the watercourse index of a Strand.
     * <BR>
     * @see  de.tuhh.kalypso.data.Wc.java#m_wcIndex Watercourse index
     * @param m_wcIndex */
    private Wc m_wcIndex = null;
    /** Field holding the current state id of a Strand.
     * @param m_wcStateId The state id refering to the name of the state ( Db ).*/
    //private int m_wcStateId = 1; // dieser Wert muss wieder auf -1 zurückgesetzt werden wenn GUI gemacht
    /** Field holding all attached riverbasins to this Strand
     * @param m_rbs A river basin tabel containing all attached river basins*/
    private RbTable m_rbis = null;
    /** Member hodlding data hydrolicaly relevant parameters of the Strand.
     * <BR>
     * @see  de.tuhh.kalypso.strand.StrandData.class#  StrandData.
     * @param m_data The data of the strand, specifing one of the four hydrualic properies.*/
    private StrandData m_data = null;
    /** Field holding a boolean to set the Strand as furcating.
     * @param m_isFurcation ( Default false )*/
    private boolean m_isFurcation = false;
    /** Field holding a boolean to set the Strand as confluent.
     * @param m_isConfluence ( Default false )*/
    private boolean m_isConfluence = false;
    /** Constructs a Strand with the specified strand number, in node, out node
     * and watercourse index.*/
    public Strand( int number, Node nodeIn, Node nodeOut, Wc wcIndex )
    {
	this.m_strandNr = number;
	this.m_nodeIn = nodeIn;
	this.m_nodeOut = nodeOut;
	this.m_wcIndex = wcIndex;
    }
	
    public Strand( int number, Node nodeIn, Node nodeOut )
    {
	this.m_strandNr =  number;
	this.m_nodeIn = nodeIn;
	this.m_nodeOut = nodeOut;
    }
    // Standardkonstruktor
	
    /** Returns the watercourse index of a strand.
     * @param id*/
    public Wc getWcIndex() { return m_wcIndex; }
    /** Retruns the strands number.
     * @return m_strandNr*/
    public int getStrandNr() { return m_strandNr; }
    /** Returns the inflow node of the strand.
     * @return m_nodeIn */
    public Node getNodeIn() { return m_nodeIn; }
    /** Returns the outflow node of the strand.
     * @return m_nodeOut */
    public Node getNodeOut() { return m_nodeOut; }
    /** Returns the type number of the strand ( Type 0 to 3 ).
     * @return type Strand type*/
    public int getType() { return m_data.getType(); }
	
    public String getTypeName()
    {
	int i = getType();
	String name=null;
	if( i == 0)
	    name = "nullStrand";
	if( i == 1 )
	    name = "channel";
	if( i == 2 )
	    name = "rhb";
	if( i == 3 )
	    name =  "rht";
	if(name==null)
	    {
		LogFile.log("unknown strand type!");
		System.out.println( "unknown strand type!" );
	    }
	return name;
    }
	
    /** Sets the boolean value for confluence ( default value is false ).
     * @param b */
    public void setIsConfluence( boolean b ) { m_isConfluence = b ; }
    /** Returns the boolean value for confluence.
     * @param m_isConfluence*/
    public boolean getIsConfluence() { return m_isConfluence; }
    /** Sets the boolean value for furcation ( default value is false ).*/
    public void setIsFurcation( boolean b ) { m_isFurcation = b; }
    /** Returns the boolean value for furcation.
     * @param m_isFurcation */
    public boolean getIsFrucation() { return m_isFurcation; }
    /** Adds a river basin to this strand.
     * @param rb River basin to be attached to this strand.*/
    public void addRbToStrand( Rb rb )
    {
	if( m_rbis == null )
	    {
		RbTable ribs = new RbTable();
		m_rbis = ribs;
	    }
	m_rbis.add( rb );
    }
    /** Returns a river basin table containing all attached river basins at this strand.
     * @retrun m_rbs Attached river basins.*/
    public RbTable getAttachedRb() { return m_rbis; }
    /** Sets the type of the strand ( creating a new object ).
     * @param strandType The number of the specific strand type.*/
    public StrandData setType( int strandType )
    {
	StrandData d = StrandData.createData( strandType );
	m_data = d;
	return m_data;
    }// setType
    /** Returns the data attached to the strand.*/
    public StrandData getData() { return m_data; };
    /** This method generates a unique hash code ( coming strand number and
     * watercourse index ) and returns the unique hashcode.*/
    public int hashCode()
    {
	String hashString = m_wcIndex.getWcIndex() + m_strandNr;
	return hashString.hashCode();
    }// hashCode
    /** This method implements the compareTo method for the TreeSet. It compares
     * this object with the specified object for order. Returns a negative integer,
     * zero, or a positive integer as this object is less than, equal to,
     * or greater than the specified object.
     * @param other The object to be compared to this object.
     * @return int Returns a -1/0/1 if the node number of the other object is larger/equal/smaller then the node number of this object.
     */
    public int compareTo( Object other  )
    {
	if( other instanceof Strand && ( (Strand) other ).m_strandNr > m_strandNr )
	    return -1;
	if( other instanceof Strand && ( (Strand) other ).m_strandNr < m_strandNr )
	    return 1;
	else
	    return 0;
    }//compareTo
    /** This method gets the wcIndex of the attached node on a strand.
     * The nodes wcIndex is defined by the wcIndex of the strand.
     * @param n Node where to get the wcIndex from.
     * @return String The wcIndex of the node if it is assigned otherwise return null ( definde by the strand ).
     */
    public Wc getWcIndexNode( Node n )
    {
	if( getNodeIn().getNodeNr() == n.getNodeNr() ^ getNodeOut().getNodeNr() == n.getNodeNr() )
	    return getWcIndex();
	return null;
    }// getWcIndexNode
    /** This method returns a node if it is an inflow node or an outflow node of
     * a strand. If it is neither  it returns null.
     * @param nodeNr The node number of the desired node.
     * @return node The in- or outflow node of the strand or null.*/
    public Node getNodeFromStrand( int nodeNr )
    {
	if( getNodeIn().getNodeNr() == nodeNr )
	    return getNodeIn();
	if( getNodeOut().getNodeNr() == nodeNr )
	    return getNodeOut();
	return null;
    }// getNodeFromStrand
	
    /** This method checks if a strands in- or outNode has HydroData. It returns a
     * NodeTable containing nodes with HydroData. If the strand has no nodes with
     * HydroData it returns null.
     * @return nodeTable A node table containg nodes with Hydrodata.
     */
    public NodeTable isHydroDataFromNodesExisting()
    {
	NodeTable nodesWithHydroData = null;
	if( getNodeIn().getHydroData() != null )
	    {
		nodesWithHydroData = new NodeTable();
		nodesWithHydroData.add( getNodeIn() );
	    }// if
	if( getNodeOut().getHydroData() != null )
	    {
		nodesWithHydroData = new NodeTable();
		nodesWithHydroData.add( getNodeOut() );
	    }// if
	return nodesWithHydroData;
    }// isHydroDataFromNodesExisting
    /** This method returns the in- and out node of a strand.
     * @return bothNodes Two nodes ( in- and out node of strand ).
     */
    public NodeTable getInAndOutNodeOfStrand()
    {
	NodeTable bothNodes = new NodeTable();
	bothNodes.add( getNodeIn() );
	bothNodes.add( getNodeOut() );
	return bothNodes;
    }// getInAndOutNodeOfStrand
    /** This method returns the specific data of the strand ( Type 0 to 3 ).
     * @return rhb If the Strand has a Rhb properiy assigned it returns it, otherwise null.
     */
    public Rhb getRhb()
    {
	StrandData sd = getData();
	if( sd instanceof Rhb )
	    {
		Rhb rhb = (Rhb) sd.getStrandData();
		return rhb;
	    }// if
	return null;
    }// getRhb
    /** This method returns the specific data of the strand ( Type 0 to 3 ).
     * @return rht If the Strand has a Rht properiy assigned it returns it, otherwise null.
     */
    public Rht getRht()
    {
	StrandData sd = getData();
	if( sd instanceof Rht )
	    {
		Rht rht = (Rht) sd.getStrandData();
		return rht;
	    }// if
	return null;
    }// getRht
    /** This method returns the specific data of the strand ( Type 0 to 3 ).
     * @return c If the Strand has a Channel properiy assigned it returns it, otherwise null.
     */
    public Channel getChannel()
    {
	StrandData sd = getData();
	if( sd instanceof Channel )
	    {
		Channel c = (Channel) sd.getStrandData();
		return c;
	    }// if
	return null;
    }// getChannel
    /** This method returns the specific data of the strand ( Type 0 to 3 ).
     * @return nc If the Strand has a NullChannel properiy assigned it returns it, otherwise null.
     */
    public NullChannel getNullChannel()
    {
	StrandData sd = getData();
	if( sd instanceof NullChannel )
	    {
		NullChannel nc = (NullChannel) sd.getStrandData();
		return nc;
	    }// if
	return null;
    }// getNullChannel
	
    /** This method returns the Rhb data ( first row of Table in file )in an array.
     * @retrun array */
    protected String[] headOfRhtToArray()
    {
	Rht rht = getRht();
	if( rht != null )
	    {
		Node n = rht.getTargetNode();
		int nodeNumber = -1;
		if( n != null )
		    nodeNumber = n.getNodeNr();
		else nodeNumber = 0;
		String array[] = { "Reservoir", String.valueOf( getStrandNr() ),
				   String.valueOf( nodeNumber ), String.valueOf( rht.getCFact() ),
				   String.valueOf( rht.getNrOfSets() ), rht.getEvapFile().getName(), rht.getLongTermSimFile().getName() };
		return array;
	    }// if
	return null;
    } // headOfRhtToArray
    /** This method writes ChannelData to the channel file.
     * strand number
     * type number
     * 1001
     *    0
     * @param bw The BufferedWriter of the channel file.
     * @exception Throws IOException if the buffered writer is corrupted.
     */
    public void writeChannelData( BufferedWriter bw ) throws Exception
    {
	bw.write( Printf.format("%12.0d", String.valueOf( getStrandNr() ) ) );
	bw.newLine();
	bw.write( Printf.format("%12.0d", String.valueOf( getType() ) ) );
	bw.newLine();
		
	StrandData sd = getData();
	sd.writeChannelToFile( bw );
    }// writeChannelData
    /** This method writes RhbData to the Rhb file ( 1st to 3rd row ).
     * @param bw The BufferedWriter of the Rhb file.
     * @exception Throws IOExcetption if buffered writer is corrupted.
     */
    public void writeRhbData( BufferedWriter bw ) throws IOException
    {
	Rhb rhb = getRhb();
	if( rhb == null )
	    return;
		
	Node n = rhb.getTargetNode();
	int nodeNumber = -1;
	if( n != null )
	    {
		nodeNumber = n.getNodeNr();
		System.out.println("rhb "+getStrandNr()+" has TargetNode");
	    }	
	else nodeNumber = 0;

	// ToDo
	// workaround:
	nodeNumber=rhb.getTargetNodeAsInt();

	String array[] = { "SPEICHER", String.valueOf( getStrandNr() ),
			   String.valueOf( nodeNumber ), String.valueOf( rhb.getCFact() ) };
		
	//	bw.write( Printf.format("%8.0s%8.0d%8.0s%6.2f", array ) );
	bw.write( Printf.format("%8.0s%8.0d%8.0d%6.2f", array ) );
	bw.newLine();
	bw.write( "\\Name of Rhb" ) ; //TODO  comment of user, to be implemented
	bw.newLine();
	bw.write( String.valueOf( getNodeOut().getNodeNr() ) );
	rhb.writeRhbToFile( bw );
    }// writeRhbData
    /** This method writes RhtData to the rht file ( 1st and 2nd row ).
     * @param bw The BufferedWriter of the rhb file.
     * @exception Throws IOException if buffered writer is corrupted.
     */
    public void writeRhtData( BufferedWriter bw ) throws IOException
    {
	Rht rht = getRht();
	if( rht == null )
	    return;
	bw.write( Printf.format("%8.0s%8.0d%8.0d%6.2f%4d%14.0s%14.0s", headOfRhtToArray() ) );
	bw.newLine();
	bw.write( "\\Name of Rht" ) ; //TODO  comment of user, to be implemented
	bw.newLine();
	StrandData sd = getData();
	sd.writeRhtToFile( bw );
    }// writeRhtData
	
    /** This method returns a String representation of the strand.
     * @return string String representaion of strand. Format: strand number, in node, out node.
     */
    public String toString()
    {
	return  "strandNr: " + m_strandNr + " inNode: " + m_nodeIn.getNodeNr() + " outNode: "
	    + m_nodeOut.getNodeNr() + " wcIndex: " + getWcIndex().getWcIndex();
    }// toString
    /** This method returns a String representaion of a strand to write to the
     * Database.
     * @return string String of strand for Database. Format: strand number, watercourse index, state id, in node, out node.
     */
    public String toStringDb()
    {
	return "(" + m_strandNr + ",'" + m_wcIndex + "'," + m_nodeIn.getNodeNr()
	    + "," + m_nodeOut.getNodeNr() + ")";
    }// toStringDb
    /** This method returns the strand in an array.
     * @retrun array*/
    public String[] strandToArray()
    {
	String array[] = { String.valueOf( m_strandNr ), String.valueOf( m_nodeIn ),
			   String.valueOf( m_nodeOut ), null, m_wcIndex.getWcIndex()};
	return array;
    }// strandToArray
	
    public void writeStrandToXML( BufferedWriter bw ) throws IOException
    {
		
	StrandData sd = getData();
	int type = sd.getType();
	switch( type )
	    {
	    case 0:
		{
		    // Simple Properties
		    bw.write( " <table key=\"nullStrand\"> <o ID=\"" + hashCode()
			      + "\"> <sp m_strandNr=\"" + getStrandNr() + "\""
			      + " /> </o> </table>" );
		    // Relation strand2node
		    bw.write( " <table key=\"strand2node\">" );
		    bw.write( " <o ID=\"" + KeyGenerator.getKey() + "\"> <sp /> <rel srcKey=\"nullStrand\" srcID=\"" +
			      hashCode() + "\" destKey=\"node\" destID=\""
			      + getNodeOut().hashCode() + "\" /> </o>");
		    bw.write( " </table>" );
		    // Relation node2strand
		    bw.write( " <table key=\"node2strand\">" );
		    bw.write( " <o ID=\"" + KeyGenerator.getKey() + "\"> <sp /> <rel srcKey=\"node\" srcID=\"" +
			      getNodeIn().hashCode() + "\" destKey=\"nullStrand\" destID=\""
			      + hashCode() + "\" /> </o>");
		    bw.write( " </table>" );
		    // Relation wc2objects
		    bw.write( " <table key=\"wc2objects\"> <o ID=\"" + KeyGenerator.getKey()
			      + "\"> <rel srcKey=\"wc\" srcID=\"" +
			      getWcIndex().hashCode() + "\" destKey=\"nullStrand\" destID=\""
			      + hashCode() + "\" />");
		    bw.write( " </o> </table>" );
		    break;
		}
	    case 1:
		{
		    // Simple Properties
		    bw.write( " <table key=\"channel\"> <o ID=\"" + hashCode()
			      + "\"> <sp m_strandNr=\"" + getStrandNr() + "\"" );
		    Channel ch = getChannel();
		    ch.writeChannelToXML( bw );
		    bw.write( " </o> </table>" );
		    // Relation strand2node
		    bw.write( " <table key=\"strand2node\">");
		    bw.write( " <o ID=\"" + KeyGenerator.getKey() + "\"> <sp /> <rel srcKey=\"channel\" srcID=\"" +
			      hashCode() + "\" destKey=\"node\" destID=\""
			      + getNodeOut().hashCode() + "\" /> </o>");
		    bw.write( " </table>" );
		    // Relation node2strand
		    bw.write( " <table key=\"node2strand\">" );
		    bw.write( " <o ID=\"" + KeyGenerator.getKey() + "\"> <sp /> <rel srcKey=\"node\" srcID=\"" +
			      getNodeIn().hashCode() + "\" destKey=\"channel\" destID=\""
			      + hashCode() + "\" /> </o>");
		    bw.write( " </table>" );
		    // Relation wc2objects
		    bw.write( " <table key=\"wc2objects\"> <o ID=\"" + KeyGenerator.getKey()
			      + "\"> <rel srcKey=\"wc\" srcID=\"" +
			      getWcIndex().hashCode() + "\" destKey=\"channel\" destID=\""
			      + hashCode() + "\" />");
		    bw.write( " </o> </table>" );
		    break;
		}
	    case 2:
		{
		    // Simple Properties
		    bw.write( " <table key=\"rhb\"> <o ID=\"" + hashCode()
			      + "\"> <sp m_strandNr=\"" + getStrandNr() + "\"" );
		    Rhb rhb = getRhb();
		    rhb.writeRhbToXML( bw );
		    bw.write( " </o> </table>" );
		    // Relation strand2node
		    bw.write( " <table key=\"strand2node\">");
		    bw.write( " <o ID=\"" + KeyGenerator.getKey() + "\"> <sp /> <rel srcKey=\"rhb\" srcID=\"" +
			      hashCode() + "\" destKey=\"node\" destID=\""
			      + getNodeOut().hashCode() + "\" /> </o>");
		    bw.write( " </table>" );
		    // Relation node2strand
		    bw.write( " <table key=\"node2strand\">" );
		    bw.write( " <o ID=\"" + KeyGenerator.getKey() + "\"> <sp /> <rel srcKey=\"node\" srcID=\"" +
			      getNodeIn().hashCode() + "\" destKey=\"rhb\" destID=\""
			      + hashCode() + "\" /> </o>");
		    bw.write( " </table>" );
		    // Relation wc2objects
		    bw.write( " <table key=\"wc2objects\"> <o ID=\"" + KeyGenerator.getKey()
			      + "\"> <rel srcKey=\"wc\" srcID=\"" +
			      getWcIndex().hashCode() + "\" destKey=\"rhb\" destID=\""
			      + hashCode() + "\" />");
		    bw.write( " </o> </table>" );
		    break;
		}
	    case 3:
		{
		    // Simple Properties
		    bw.write( " <table key=\"rht\"> <o ID=\"" + hashCode()
			      + "\"> <sp m_strandNr=\"" + getStrandNr() + "\"" );
		    Rht rht = getRht();
		    rht.writeRhtToXML( bw );
		    bw.write( " </sp> </o> </table>" );
		    // Relation strand2node
		    bw.write( " <table key=\"strand2node\">");
		    bw.write( " <o ID=\"" + KeyGenerator.getKey() + "\"> <sp /> <rel srcKey=\"rht\" srcID=\"" +
			      hashCode() + "\" destKey=\"node\" destID=\""
			      + getNodeOut().hashCode() + "\" /> </o>");
		    bw.write( " </table>" );
		    // Relation node2strand
		    bw.write( " <table key=\"node2strand\">" );
		    bw.write( " <o ID=\"" + KeyGenerator.getKey() + "\"> <sp /> <rel srcKey=\"node\" srcID=\"" +
			      getNodeIn().hashCode() + "\" destKey=\"rht\" destID=\""
			      + hashCode() + "\" /> </o>");
		    bw.write( " </table>" );
		    // Relation wc2objects
		    bw.write( " <table key=\"wc2objects\"> <o ID=\"" + KeyGenerator.getKey()
			      + "\"> <rel srcKey=\"wc\" srcID=\"" +
			      getWcIndex().hashCode() + "\" destKey=\"rht\" destID=\""
			      + hashCode() + "\" />");
		    bw.write( " </o> </table>" );
		    break;
		}
	    }
		
    }// writeStrandToXML
} // class Strand
