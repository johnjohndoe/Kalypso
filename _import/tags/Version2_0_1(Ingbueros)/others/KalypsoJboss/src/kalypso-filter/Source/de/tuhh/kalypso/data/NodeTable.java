package de.tuhh.kalypso.data;
import java.io.BufferedWriter;
import java.io.IOException;
import java.util.Iterator;
import java.util.Locale;
import java.util.TreeSet;

import de.tuhh.kalypso.data.node.HydroData;
import de.tuhh.kalypso.data.node.Node;
import de.tuhh.kalypso.util.KeyGenerator;
import de.tuhh.kalypso.util.LogFile;
import de.tuhh.kalypso.util.Printf;

/**
 * NodeTable.java
 * containts all methods concerning node collections (HashSet)
 * imported from a grid file for the Rainfall-Runoff-Model Kalypso.
 * It belongs to the class ReadGrid.
 * @author <A HREF="mailto:c.kuepferle@tuhh.de">Christoph Küpferle</A>
 * @version NodeTable.java,v 1.0 2002/07/01
 */
public class NodeTable extends TreeSet
{
	
	/** This method writes a NodeTable with all hydro data into the grid file
	 * in the aproprate format for Kalypso.
	 * @param bw The BufferedWriter of the ( open ) new grid file.
	 */
	public void writeNodeTable( BufferedWriter bw ) throws Exception
	{
		Printf.setLocale( Locale.ENGLISH );
		try
		{
			while( size() > 0 )
			{
				Iterator it = iterator();
				
				Node n1 = ( Node ) it.next();
				bw.newLine();
				bw.write( Printf.format("%5.0d%5.0d%5.0d%5.0d%5.0d%5.0d" , n1.nodeToArrayFlags() ) );
				HydroData hd = n1.getHydroData();
				if( hd != null )
					hd.writeHydroDataToFile( bw );
				remove( n1 );
			}// while
			bw.newLine();
			bw.write( Printf.format("%5.0d", "99999" ) );
			bw.newLine();
			Printf.setLocale( Locale.getDefault() );
		}// try
		catch( Exception e )
		{
			System.out.println( "Error while writing node table to grid file!" );
			LogFile.log(" Error while writing node table to grid file!" );
			LogFile.log( " Error message: " + e );
			e.printStackTrace();
			throw e;
		}// catch
		finally
		{
			Printf.setLocale( Locale.getDefault() );
		}// finally
	}// writeNodeTable
	/** THis method dumps a node table to the system console.*/
	public void dump()
	{
		System.out.println( "A nodeTable with " + size() + " Elements " );
		Iterator it = iterator();
		while( it.hasNext() )
		{
			Node n = (Node)it.next();
			System.out.println( n );
		} // while it
	}// dump
	
	public void writeNodesToXML( BufferedWriter bw ) throws IOException
	{
		Iterator it = iterator();
		while( it.hasNext() )
		{
			Node n = (Node) it.next();
			// Simple properties of this node
			bw.write( " <table key=\"node\"> <o ID=\"" + n.hashCode()
						 + "\"> <sp m_nodeNr=\"" + n.getNodeNr() + "\"" );
			if( n.getHydroData() != null && n.getHydroData().isInToNodeExisting() > 0 )
			{
				bw.write( " m_inToNodeConst=\"" + n.getHydroData().getInToNodeVal()
							 + "\" />" );
				bw.write( " </o> </table>" );
			}
			else
			{
				bw.write( " />" );
				bw.write( " </o> </table>" );
			}
			// Relation node2nodeConstOut (const. Outflow to traget node)
			if( n.getHydroData() != null && n.getHydroData().isOutToNodeExisting() > 0 )
			{
				bw.write( " <table key=\"node2nodeConstOut\"> <o ID=\"" + KeyGenerator.getKey()
							 + "\"> <sp value=\"" + n.getHydroData().getOutToNodeVal() + "\"/>");
				bw.write( " <rel srcKey=\"node\" srcID=\"" +
							 n.hashCode() + "\" destKey=\"node\" destID=\""
							 + n.getHydroData().getOutToNodeTargetNode().hashCode()
							 + "\" />");
				bw.write( " </o> </table>" );
			}
			// Relation node2nodeFunctOut (outflow function to traget node)
			if( n.getHydroData() != null && n.getHydroData().isOutToNodeFunctExisting() > 0 )
			{
				bw.write( " <table key=\"node2nodeFunctOut\"> <o ID=\"" + KeyGenerator.getKey()
							 + "\"> <sp path=\"" + n.getHydroData().getOutToNodeFunctFile() + "\"/>");
				bw.write( " <rel srcKey=\"node\" srcID=\"" +
							 n.hashCode() + "\" destKey=\"node\" destID=\""
							 + n.getHydroData().getOutToNodeFunctNode().hashCode()
							 + "\" />");
				bw.write( " </o> </table>" );
			}
			// Relation node2nodeFunctIn (inflow function to node)
			if( n.getHydroData() != null && n.getHydroData().isInToNodeFunctExisting() > 0 )
			{
				bw.write( " <table key=\"node2nodeFunctIn\"> <o ID=\"" + KeyGenerator.getKey()
							 + "\"> <sp path=\"" + n.getHydroData().getInToNodeFunctFile() + "\"/>");
				bw.write( " <rel srcKey=\"node\" srcID=\"" +
							 n.hashCode() + "\" destKey=\"node\" destID=\""
							 + n.getHydroData().getInToNodeFunctNode().hashCode()
							 + "\" />");
				bw.write( " </o> </table>" );
			}
			// Relation node2nodeOverflow (overflow to traget node)
			if( n.getHydroData() != null && n.getHydroData().isOverFlowExisting() > 0 )
			{
				bw.write( " <table key=\"node2nodeOverflow\"> <o ID=\"" + KeyGenerator.getKey()
							 + "\"> <sp value=\"" + n.getHydroData().getOverflowVal() + "\"/>");
				bw.write( " <rel srcKey=\"node\" srcID=\"" +
							 n.hashCode() + "\" destKey=\"node\" destID=\""
							 + n.getHydroData().getOverflowNode().hashCode()
							 + "\" />");
				bw.write( " </o> </table>" );
			}
			// Relation node2nodePercent (percent outflow to traget node)
			if( n.getHydroData() != null && n.getHydroData().isPercentExtractExisting() > 0 )
			{
				bw.write( " <table key=\"node2nodePercent\"> <o ID=\"" + KeyGenerator.getKey()
							 + "\"> <sp value=\"" + n.getHydroData().getPercentExtractVal() + "\"/>");
				bw.write( " <rel srcKey=\"node\" srcID=\"" +
							 n.hashCode() + "\" destKey=\"node\" destID=\""
							 + n.getHydroData().getPercentExtractNode().hashCode()
							 + "\" />");
				bw.write( " </o> </table>" );
			}
		}// while it
	}
} // class NodeTable

