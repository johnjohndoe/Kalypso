package de.tuhh.kalypso.data;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Locale;
import java.util.TreeSet;

import de.tuhh.kalypso.data.node.Node;
import de.tuhh.kalypso.data.strand.Strand;
import de.tuhh.kalypso.util.KeyGenerator;
import de.tuhh.kalypso.util.LogFile;
import de.tuhh.kalypso.util.Printf;


/** The Class StrandTable.java can hold a set of Strands, the strand are listed
 * in assending order of the strand numbers.
 *
 *
 * @author <A HREF="mailto:c.kuepferle@tuhh.de">Christoph Küpferle</A>
 * @version StrandTable.java,v 1.0 2002/07/01 $
 */
public class StrandTable extends TreeSet
{
	private static String col1 = "strand_nr_model";
	private static String col2 = "wc_index";
	private static String col3 = "wc_state_id";
	private static String col4 = "strand_type_id";
	private static String col5 = "node_nr_model_in";
	private static String col6 = "node_nr_model_out";
	
	/** This method gets a strand with a specific strand number.
	 * @param strandNr Number of the strand to get.
	 * @return s The strand that matches the strand number.
	 */
	public Strand getStrand( int strandNr )
	{
		Iterator it = iterator();
		while( it.hasNext() )
		{
			Strand s = ( Strand ) it.next();
			if( s.getStrandNr() == strandNr )
				return s;
		}
		return null;
	}//  getStrand
	/** This method gets a Node from a Strand. It can be an in- or outflow
	 * node.
	 * @param nodeNumber The node number of the node to get.
	 */
	public Node getNodeFromStrand ( int nodeNumber )
	{
		Iterator it = iterator();
		while( it.hasNext() )
		{
			Strand s = ( Strand ) it.next();
			if( s.getNodeIn().getNodeNr() == nodeNumber )
			    return s.getNodeIn();
			if( s.getNodeOut().getNodeNr() == nodeNumber )
			    return s.getNodeOut();
		}// while
		return null;
	}// getNodeFromStrand
	
	/** This method gets a set of Strands where a given node number is either a
	 * inNode or outNode.
	 * @param nodeNumber The node number to match to inNode or outNode.
	 * @return sT A Strand table containing all strands attached to the node.
	 */
	public StrandTable getStrandsFromNode( int nodeNumber )
	{
		StrandTable sT = new StrandTable();
		Iterator itr = iterator();
		while (itr.hasNext())
		{
			Strand strand = (Strand) itr.next();
			if ( strand.getNodeIn().getNodeNr() == nodeNumber ^ strand.getNodeOut().getNodeNr() == nodeNumber )
			{
				sT.add( strand );
			}// if
		}//while
		return sT;
	}// getStrandsFromNode
	
	/** This Method determines for a Set of strands at a junction point in
	 * the grid which strands is the main watercours.
	 * @return s A strand which is the main watercours at a junction, or null if there is no mainWc.
	 */
	public Strand getMainWc()
	{
		Iterator it1 = iterator();
		while (it1.hasNext())
		{
		    Strand s1 = (Strand) it1.next();
		    Iterator it2 = iterator();
		    while (it2.hasNext())
			{
			    Strand s2 = (Strand) it2.next();
			    if( s1 == s2 )
				continue;
			    if ( (s1.getNodeOut().getNodeNr() == s2.getNodeIn().getNodeNr() ) &
				  s2.getWcIndex().equals( s1.getWcIndex() )  )
				{
				    return s1;
				}
			}//while it2
		}// while it1
		return null;
	}// getMainWc
	/** This mehtod finds strands which are intersecting and distinguishes confluence
	 * or frucation of the strands. If there are intersections the method assings the
	 * adjWcIndex to the confluenting and/or frucating strands. The flag isConfluence
	 * or isFurcation is only assigned to the adjWc and NOT the mainWc. The adjStrands
	 * are also assigned to the node at the intersection ( StrandTable ).
	 */
	public void assignAdjStrands ()
	{
		Iterator it1 = iterator();
		boolean intersecFlag = false;
		while (it1.hasNext() )
		{
			Strand s1 = (Strand) it1.next();
			Iterator it2 = iterator();
			while (it2.hasNext())
			{
				Strand s2 = (Strand) it2.next();
				if( s1 == s2 )
					continue;
				// checks for confluence
				if( s1.getNodeOut().getNodeNr() == s2.getNodeOut().getNodeNr() &&  !s2.getWcIndex().equals( s1.getWcIndex() ) ) // adjacient
				{
					Node mainNode = s2.getNodeOut();
					StrandTable sTable = getStrandsFromNode( mainNode.getNodeNr() );
					Strand mainStrand = sTable.getMainWc();
					Wc wc = mainStrand.getWcIndex();
					s2.getNodeOut().setMainWcAtNode( wc );
					sTable.setAdjStrandsToMainNode( mainNode );
					mainNode.setIsConfluenceOnAdjStrand( true );
				}// if adjacient
				
				// checks for furctaion
				if( s1.getNodeIn().getNodeNr() == s2.getNodeIn().getNodeNr() &&  !s2.getWcIndex().equals( s1.getWcIndex() ) ) // furcation
				{
					Node mainNode = s2.getNodeIn();
					StrandTable sTable = getStrandsFromNode( mainNode.getNodeNr() );
					Strand mainStrand = sTable.getMainWc();
					Wc wc = mainStrand.getWcIndex();
					s2.getNodeIn().setMainWcAtNode( wc );
					sTable.setAdjStrandsToMainNode( mainNode );
					mainNode.setIsFurcationOnAdjStrand( true );
				}// if furcation
			}// while it2
		}// while it1
	}// assignAdjStrands
	/** This method sets a set of AdjStrands to a node at the intersection. It fills
	 * the field adjStrands for a node at an intersection.
	 * @param s The main watercours stramd where all the other strands are to be attached to.
	 */
	public void setAdjStrandsToMainNode( Node mainNode )
	{
		mainNode.createAdjStrands();
		Iterator it = iterator();
		while( it.hasNext() )
		{
			Strand s = ( Strand ) it.next();
			if( !s.getWcIndex().equals( mainNode.getMainWcAtNode() ) )
				mainNode.addAdjStrand( s );
		}//while
	}// setAdjStrandsToMainNode
	/** This method gets a strand from a Strand Table where the node n is attached to.
	 * @param n The node of a strand.
	 * @return s The strand containing the desired node, or null if it is not existing.
	 */
	public Strand getStrandFormNode( Node n )
	{
		Iterator it = iterator();
		while( it.hasNext() )
		{
			Strand s = ( Strand ) it.next();
			if( n.getNodeNr() == s.getNodeIn().getNodeNr() ^ n.getNodeNr() == s.getNodeOut().getNodeNr() )
				return s;
		}
		return null;
	}// getStrandFormNode
	/** Returns a strand from a Strand Table with the node number beeing either a in- or outNode
	 * @param nodeNr The node number of the node.
	 * @return s The strand having nodeNr as an in- or outNode, or null if there is no strand with this node.
	 */
	public Strand getStrandFormNode( int nodeNr )
	{
		Iterator it = iterator();
		while( it.hasNext() )
		{
			Strand s = ( Strand ) it.next();
			if( nodeNr == s.getNodeIn().getNodeNr() ^ nodeNr == s.getNodeOut().getNodeNr() )
				return s;
		}// while
		return null;
	}//getStrandFormNode
	
	/** This method returns a list of all nodes attached to strands.
	 * @return theNodes A NodeTable containg all nodes.
	 */
	public NodeTable getNodes()
	{
		NodeTable theNodes = new NodeTable();
		Iterator it = iterator();
		while( it.hasNext() )
		{
			Strand s = ( Strand ) it.next();
			Node nIn = s.getNodeIn();
			theNodes.add( nIn );
			Node nOut = s.getNodeOut();
			theNodes.add( nOut );
		}// while
		return theNodes;
	}// getNodes
	
	/** This method gets a strand from a Strand Table where the node n is the nodeOut
	 * of the strand. Used to get the Strand for the deep Aquifer
	 * @param n The strand with nodeIn = n.
	 * @return s The strand containing the desired node, or null if it is not existing.
	 */
	public Strand getStrandWithOutNode( int nodeOut )
	{
		Iterator it = iterator();
		while( it.hasNext() )
		{
			Strand s = ( Strand ) it.next();
			if( nodeOut == s.getNodeOut().getNodeNr() )
				return s;
		}// if
		return null;
	}// getStrandWithOutNode
	
	/** Returns all strands where the outNode is the inNode of the strand above.
	 * It is selecting a subnet of strands connected to eachother.
	 * @param rootStrnd
	 */
	public StrandTable getUpperStrandsFromStrand( Strand rootStrand )
	{
		return getUpperStrandsFromNode( rootStrand.getNodeIn() );
	}
	
	
	/** This method gets all upper Strands where a given node is an outNode of the strand.
	 * Only first level of upper strand.
	 * @param outNode The node to match the outNode on the strand.
	 * @return sT A Strand table containing all strands attached to the node
	 */
	public StrandTable getUpperStrandsFromNode( Node outNode )
	{
		StrandTable strandTable = new StrandTable();
		Iterator itr = iterator();
		while ( itr.hasNext() )
		{
			Strand strand = (Strand) itr.next();
			if ( strand.getNodeOut() == outNode  )
			{
				strandTable.add( strand );
			}// if
		}//while
		return strandTable;
	}// getUpperStrandsFromNode
	
	/** This method gets all lower Strands from a given Strand s above.
	 * @param s The Upper Strand.
	 * @return sT A Strand table containing all lower strands attached to the upper strand.
	 */
	public StrandTable getLowerStrands( Strand s )
	{
		StrandTable strandTable = new StrandTable();
		Iterator itr = iterator();
		while ( itr.hasNext() )
		{
			Strand strand = (Strand) itr.next();
			if ( strand.getNodeIn() == s.getNodeOut() )
			{
				strandTable.add( strand );
			}// if
		}//while
		return strandTable;
	}// getLowerStrands
	/** Returns the first strand of a StrandTable with a specific watercourse.
	 * @param wc The desired watercourse.
	 * @return s The first strand where the watercourse == wc, or null if not existing.
	 */
	public Strand getFirstStrandWithWc( Wc wc )
	{
		Iterator it = iterator();
		while( it.hasNext() )
		{
			Strand s = (Strand)it.next();
			if( s.getWcIndex().equals( wc ) )
				return s;
		}// while
		return null;
	}// getFirstStrandWithWc
	/** This method dumps a StrandTable to the console ( System.out ).*/
	public void dump()
	{
		if( size() > 1 ^ size() == 0 )
			System.out.println( "Strand Table with " + size() + " strands" );
		else System.out.println( "Strand Table with " + size() + " strand" );
		
		Iterator it = iterator();
		while( it.hasNext() )
		{
			Strand s = ( Strand ) it.next();
			System.out.println( " dump Strand: " + s.toString() );
		}// while
	}// dump
	/** Returns a StrandTable containing all strands "connected" through in- or
	 * outflowing water between nodes.
	 * @param rootStrand The base strand.
	 * @return sT StrandTable contining all upper strands connected through nodes.
	 */
	public StrandTable getConStrandsFromStrand( Strand rootStrand )
	{
		StrandTable sT = new StrandTable();
		
		NodeTable nT = getConNodesFromNode( rootStrand.getNodeOut() );
		Iterator it = nT.iterator();
		while( it.hasNext() )
		{
			Node n = (Node)it.next();
			sT.addAll( getUpperStrandsFromNode( n ) );
		} // while it
		
		return sT;
	} // getConStrandsFromStrand
	
	/** This method gets a strand from a Strand Table where the node n is the nodeIn
	 * of the strand.
	 * @param n The strand with nodeIn number.
	 * @return s The strand with nodeIn == inflow node, or null if there is no strand with nodeIn == inflow node.
	 */
	public Strand getStrandWithInNode( int nodeIn )
	{
		Iterator it = iterator();
		while( it.hasNext() )
		{
			Strand s = ( Strand ) it.next();
			if( nodeIn == s.getNodeIn().getNodeNr() )
				return s;
		}// while
		return null;
	}// getStrandWithInNode
	
	
	/** This method finds all contributing nodes ( in- or outflowing water )
	 * to a specified node. Even such nodes which recieve water over serveral
	 * hirarchial steps. Possible flows are:
	 * <UL>
	 * <LI>1. OutFlowToNode
	 * <LI>2. OverFlow
	 * <LI>3. PercentExtract
	 * <UL/>
	 * @param findNode The specified node to recieve water from other nodes.
	 * @return nodeCon A table nodes which contribute water to findNode.
	 */
	public NodeTable getConNodesFromNode( Node findNode )
	{
		NodeTable nodeCon = new NodeTable();
		Iterator it = iterator();
		while( it.hasNext() )
		{
			Strand s = (Strand) it.next();
			NodeTable hydroNodes = s.isHydroDataFromNodesExisting();// Liste der Knoten des stangs s die hydrodata enthalten
			if( hydroNodes != null )
			{
				Iterator itNodes = hydroNodes.iterator();
				NodeTable conNode = new NodeTable();
				Node n = null;
				while( itNodes.hasNext() )
				{
					n = (Node)itNodes.next();
					NodeTable targetNodes = n.getTargetNodesFromHydroData();
					conNode.addAll( targetNodes );
				}
				if( conNode != null )
				{
					Iterator itConNode = conNode.iterator();
					while( itConNode.hasNext() )
					{
						Node node = ( Node ) itConNode.next();
						if( node == findNode )
							nodeCon.add( n );
					} // while
				}//if
			}// if
		} // while it
		return nodeCon;
	}// getConNodesFromNode
	
	/** Removes all strands from this.StrandTable not contained in a template StrandTable.
	 * @param sT Template StrandTable.
	 */
	public void intersect( StrandTable sT )
	{
		Iterator it = iterator();
		while( it.hasNext() )
		{
			Strand s = (Strand)it.next();
			if( !sT.contains( s ) )
			{
				remove( s );
				it = iterator();
			}// if
		} // while it
		
	} // intersect
	
	/** This method writes all channel files ( channel, rhb, rht file ).
	 * @param stateName The name of the current state == filename plus appendix.
	 * @exception Throws IOException if the buffered writer is corrupted <BR> or the file can not be opened.
	 */
	public void writeChannelToFiles( String stateName ) throws Exception
	{
		Printf.setLocale( Locale.ENGLISH );
		File channelFile = new File( stateName + ".ger" );
		File rhbFile = new File ( stateName + ".rhb" );
		File rhtFile = new File ( stateName + ".rht" );
		BufferedWriter bwChannel = new BufferedWriter( new FileWriter( channelFile ) );
		BufferedWriter bwRhb = new BufferedWriter( new FileWriter( rhbFile ) );
		BufferedWriter bwRht = new BufferedWriter( new FileWriter( rhtFile ) );
		try
		{
			Iterator it = iterator();
			while( it.hasNext() )
			{
				Strand s = (Strand) it.next();
				
				s.writeChannelData( bwChannel );
				
				s.writeRhbData( bwRhb );
				
				s.writeRhtData( bwRht );
				
			}// while
			Printf.setLocale( Locale.getDefault() );
			bwChannel.close();
			bwRhb.close();
			bwRht.close();
		}// try
		catch( IOException e )
		{
			bwChannel.write( "Error while writing Channel file: " + e.getMessage() );
			bwRhb.write( "Error while writing river Rhb file: " + e.getMessage() );
			bwRht.write( "Error while writing river Rht file: " + e.getMessage() );
			throw e;
		}// chatch
		
		// empty files are removed
		try
		{
			if( channelFile.length() == 0 )
				channelFile.delete();
			if( rhbFile.length() == 0 )
				rhbFile.delete();
			if( rhtFile.length() == 0 )
				rhtFile.delete();
		}// try
		catch( SecurityException se ) {}
		
		finally
		{
			Printf.setLocale( Locale.getDefault() );
			bwChannel.close();
			bwRhb.close();
			bwRht.close();
		}// finnaly
	}// channelToFile
	public void writeStrandsToXML( BufferedWriter bw ) throws IOException
	{
		Iterator it = iterator();
		while( it.hasNext())
		{
			Strand s = (Strand) it.next();
			s.writeStrandToXML( bw );
		}
	}
	public void	writeNodesRelWcToXML ( NodeTable theNodes, BufferedWriter bw ) throws IOException
	{
		// Relation wc2objects
		bw.write( " <table key=\"wc2objects\">" );
		
		Iterator itNodes = theNodes.iterator();
		while( itNodes.hasNext() )
		{
			Node n = (Node) itNodes.next();
			Strand s = getStrandFormNode( n );
			bw.write( "<o ID=\"" + KeyGenerator.getKey()
						 + "\"> <rel srcKey=\"wc\" srcID=\"" +
						 s.getWcIndex().hashCode() + "\" destKey=\"node\" destID=\""
						 + n.hashCode() + "\" /> </o>");
		}
		bw.write( " </table>" );
	}//writeNodesToXML
	public HashMap getNrOfStrandTypes()
	{
		HashMap results = new HashMap();
		Iterator it = iterator();
		while( it.hasNext() )
		{
			String counter = null;
			Strand s = (Strand) it.next();
			if( s.getType() == 0 )
			{
				counter = (String) results.get( "nullStrand" );
				if( counter != null )
					counter = String.valueOf( Integer.parseInt( counter ) + 1 );
				else counter = String.valueOf( 1 );
				results.put("nullStrand", counter );
			}
			if( s.getType() == 1 )
			{
				counter = (String) results.get( "channel" );
				if( counter != null )
					counter = String.valueOf( Integer.parseInt( counter ) + 1 );
				else counter = String.valueOf( 1 );
				results.put("channel", counter );
			}
			if( s.getType() == 2 )
			{
				counter = (String) results.get( "rhb" );
				if( counter != null )
					counter = String.valueOf( Integer.parseInt( counter ) + 1 );
				else counter = String.valueOf( 1 );
				results.put("rhb", counter );
			}
			if( s.getType() == 3 )
			{
				counter = (String) results.get( "rht" );
				if( counter != null )
					counter = String.valueOf( Integer.parseInt( counter ) + 1 );
				else counter = String.valueOf( 1 );
				results.put("rht", counter );
			}
		}
		return results;
	}
	public HashMap getNrOfStrandTypesPerWcIndex()
	{
		HashMap results = new HashMap();
		Iterator itS = iterator();
		while( itS.hasNext() )
		{
			String c = null;
			Strand s = (Strand) itS.next();
			c = (String) results.get( s.getWcIndex().getWcIndex() );
			if( c != null )
				c = String.valueOf( Integer.parseInt( c ) + 1 );
			else c = String.valueOf( 1 );
			results.put( s.getWcIndex().getWcIndex(), c );
		}
		return results;
	}
	public void strandStatsToLogFile()
	{
		String total = "0";
		HashMap map = getNrOfStrandTypes();
		Iterator set = map.keySet().iterator();
		LogFile.log( "---------------------------------------------------------------------------" );
		while( set.hasNext() )
		{
			String key = (String) set.next();
			total = String.valueOf( (Integer.parseInt( total ) + Integer.parseInt( String.valueOf( map.get( key ) )))) ;
			LogFile.log( (String) map.get( key ) +  " " + key + "s exist in this river system" );
		}
		LogFile.log( "---------------------------------------------------------------------------" );
		LogFile.log( "Total number of strands in this river system: " + total );
		map = getNrOfStrandTypesPerWcIndex();
		set = map.keySet().iterator();
		while( set.hasNext() )
		{
			String key = (String) set.next();
			LogFile.log( "A total of " + (String) map.get( key ) +  " strands belong to the wcIndex " + key );
		}
	}
}// class StrandTable
