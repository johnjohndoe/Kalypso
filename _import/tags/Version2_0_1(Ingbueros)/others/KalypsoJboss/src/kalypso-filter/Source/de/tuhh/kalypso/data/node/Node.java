package de.tuhh.kalypso.data.node;

import java.util.Iterator;

import de.tuhh.kalypso.data.NodeTable;
import de.tuhh.kalypso.data.StrandTable;
import de.tuhh.kalypso.data.Wc;
import de.tuhh.kalypso.data.strand.Strand;

/** The Node Class holds the data for a node. A node is defined by its node number.
 *  If the node is an intersection, in- or/and outflowing stands are attached to
 * the node. The node can have HydroData attached to it.
 * @see de.tuhh.kalypso.data.node.HydroData
 *
 * @author <A HREF="mailto:c.kuepferle@tuhh.de">Christoph Küpferle</A>
 * @version Node.java,v 1.0 2002/07/01
 */

public class Node extends Object implements Comparable
{
	/** Field holding the number of the node.
	 * @param nodeNumber Default value: -1*/
	private int m_nodeNumber = -1;
	/** A strand table holding all strands which are entering or leaving a node
	 * of intersection.
	 * @param m_adjStrands Strands at a node of confluence/furcation.*/
	private StrandTable m_adjStrands = null;
	/** Field holding the watercourse of the main channel at an node of intersection.
	 * If this field is null the node is no intersection and the watercourse is determined
	 * by the strand where the node is attached to.
	 * @param mainWcAtNode Flag to indicate if the node is a main watercours. Default value: null*/
	private Wc m_mainWcAtNode = null;
	/** This field holds hydrological data ( five types ) @see de.tuhh.kalypso.data.node.HadroData .
	 * @param m_hydroData Hydrological data of the node. default value: null; */
	private HydroData m_hydroData = null;
	
	public Node ( int nodeNumber )
	{
		this.m_nodeNumber = nodeNumber;
	}
	
	/** This method generates a unique Hash Code for each node depending on its
	 * node number. Because it is just one component ( node number ) the node number
	 * can only appear once in a set.
	 */
	public int hashCode()
	{
		String hashString = String.valueOf( m_nodeNumber );
		return hashString.hashCode();
	}// hashCode
	/** Returns the node number of a node.
	 * @return node_number */
	public int getNodeNr () { return m_nodeNumber; }
	/** Returns the mainWcAtnode.
	 * @return mainWcAtNode */
	public Wc getMainWcAtNode() { return m_mainWcAtNode; }
	/** Sets the field mainWcAtNode of a node.
	 * @param wc The main wc at a node. ( only set if node is an interection )*/
	public void setMainWcAtNode( Wc wc ) { m_mainWcAtNode = wc; }
	/** Returns the hydro data of a node.
	 * @return m_hydroData */
	public HydroData getHydroData() { return m_hydroData; }
	/** This method creates an object HydroData ( only if it is not alreay existing )
	 * and returns it.
	 * @return m_HydroData */
	public HydroData createHydroData()
	{
		if( m_hydroData != null )
			return m_hydroData;
		else
		{
			HydroData h = new HydroData();
			m_hydroData = h;
			return m_hydroData;
		}// else
	}// createHydroData
	/** Adds a strand to the member adjStrands of a node.
	 * @param s The strand to be added. */
	public void addAdjStrand( Strand s ) { m_adjStrands.add( s ); }
	/** Returns the StrandTable holding the adjStrands at a node.
	 * @return m_adjStrands */
	public StrandTable getAdjStrands() { return m_adjStrands; }
	
	/** This method sets all adjStrands at a node of intersection to confluent/not confluent.
	 * @param b Boolean value true if confluent, false if not confluent.
	 */
	public void setIsConfluenceOnAdjStrand( boolean b )
	{
		Iterator it = getAdjStrands().iterator();
		while( it.hasNext() )
		{
			Strand s = ( Strand ) it.next();
			s.setIsConfluence( b );
		}
	}// setIsConfluenceOnAdjStrand
	/** This method sets all adjStrands of at the Noode of intersection to
	 * frucation/not furcation.
	 * @param b Boolean value true if furcation, false if not frucation.
	 */
	public void setIsFurcationOnAdjStrand( boolean b )
	{
		Iterator it = getAdjStrands().iterator();// es ist nicht klar an welchen Knoten es zu schreiben ist!!!!
		while( it.hasNext() )
		{
			Strand s = ( Strand ) it.next();
			s.setIsFurcation( b );
		}
	}// setIsFurcationOnAdjStrand
	/** Returns the node number as a String.
	 * @return m_nodeNumber */
	public String toString()
	{
		return "" + m_nodeNumber;
	}// toString
	
	public void createAdjStrands()
	{
		StrandTable st = new StrandTable();
		m_adjStrands = st;
	}// createAdjStrands
	/** This method implements the compareTo method for the TreeSet. It compares
	 * this object with the specified object for order. Returns a negative integer,
	 * zero, or a positive integer as this object is less than, equal to,
	 * or greater than the specified object.
	 * @param other The object to be compared to this object.
	 * @return int Returns a -1/0/1 if the node number of the other object is larger/equal/smaller then the node number of this object.
	 */
	public int compareTo( Object other  )
	{
		if( other instanceof Node && ( (Node) other ).m_nodeNumber > m_nodeNumber )
			return -1;
		if( other instanceof Node && ( (Node) other ).m_nodeNumber < m_nodeNumber )
			return 1;
		else
			return 0;
	}// compareTo
	/** This method returns a node tabel of all nodes that recieve water
	 * form this node.
	 * @return conNodes*/
	public NodeTable getConNodesFromNode()
	{
		NodeTable conNodes = new NodeTable();
		if( getHydroData().outflowToNode != null )
			conNodes.add( getHydroData().outflowToNode.targetNode );
		if( getHydroData().overFlow != null )
			conNodes.add( getHydroData().overFlow.targetNode );
		if( getHydroData().precentExtract!= null )
			conNodes.add( getHydroData().precentExtract.targetNode );
		return conNodes;
	}// getConNodesFromNode
	
	/** This method returns a list of all target nodes of this node, meaning all
	 * nodes that recieve water from this node.
	 * @return targetNodes A NodeTable containing all nodes wher this node is feeding with water.
	 */
	public NodeTable getTargetNodesFromHydroData()
	{
		NodeTable targetNodes = new NodeTable();
		if( m_hydroData == null )
			return null;
		if( m_hydroData != null && m_hydroData.outflowToNode != null )
			targetNodes.add( m_hydroData.outflowToNode.targetNode );
		
		if( m_hydroData != null && m_hydroData.overFlow != null )
			targetNodes.add( m_hydroData.overFlow.targetNode );
		
		if( m_hydroData != null && m_hydroData.precentExtract != null )
			targetNodes.add( m_hydroData.precentExtract.targetNode );
		return targetNodes;
	}// getTargetNodeFromHydroData
	/** Returns a 1d array holding the flags for the HydroData.
	 * @return array */
	public String[] nodeToArrayFlags()
	{
		HydroData hd = getHydroData();
		if( hd == null )
		{
			String[] array = { String.valueOf( m_nodeNumber ), "0", "0", "0", "0", "0" };
			return array;
		}
		else
		{
			String[] array = hd.hydroFlagsToArray();
			array[0] = String.valueOf( m_nodeNumber );
			return array;
		}
	}// nodeToArrryFlags
} // class Node

