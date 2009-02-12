package de.tuhh.kalypso.data;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.StreamTokenizer;
import java.text.DateFormat;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Locale;
import java.util.Vector;

import de.tuhh.kalypso.data.node.HydroData;
import de.tuhh.kalypso.data.node.Node;
import de.tuhh.kalypso.data.riverbasin.Rb;
import de.tuhh.kalypso.data.strand.Rhb;
import de.tuhh.kalypso.data.strand.Rht;
import de.tuhh.kalypso.data.strand.Strand;
import de.tuhh.kalypso.data.strand.StrandData;
import de.tuhh.kalypso.util.LogFile;
import de.tuhh.kalypso.util.Printf;
import de.tuhh.kalypso.util.ErrorHandler.KalypsoFilterException;
import de.tuhh.kalypso.util.xmlParser.GisTransferManager;

/** This class WcState.java contains a @link StrandTable, a @link RbTable and a
 * @link WcTable. It is the level where alle information of a net is contained.
 * Different states can conatain the same strands and river basins.
 *
 * @author <A HREF="mailto:c.kuepferle@tuhh.de">Christoph Küpferle</A>
 * @version State.java,v 1.0 2002/07/01
 */

public class State //implements I_Filter
{
	/** The StrandTable containing all strands of this state.
	 * @param m_strands */
	protected StrandTable m_strands = null;
	/** The RbTable containing all rb of this state.
	 * @param m_rbs */
	protected RbTable m_rbs = null;
	/** The WcTable containing all watecourse of this state.
	 * @param m_wcList */
	protected WcTable m_wcList = null;
	/** Returns the strands in a state.
	 * @return m_strands*/
	public StrandTable getStrands() { return m_strands; };
	/** Retruns the riverbasins in a state.
	 * @return m_rbs*/

	public RbTable getRibs() { return m_rbs; }
	/** Retruns all watercourses in a state.
	 * @return m_wcList*/

	public WcTable getWcList() { return m_wcList; }
	/** This method returns a node from a strand where the node is an outNode.
	 * @param nr Node number.
	 * @return node Node wher nodeNr == nr.
	 */
	public Node getNodeFromNr( int nr )
	{
		Strand s = m_strands.getStrandWithOutNode( nr );
		if( s != null )
			return s.getNodeOut();
		return null;
	}//getNodeFromNr
	/** Returns a strand from StrandTable with node n == nodeOut.
	 * If the node is at an intersection it returns the strand which is the
	 * mainWc.
	 * @param n The node where the strand must contain it as outNode.
	 * @return s The strand with n == outNode.
	 */
	public Strand getStrandFromNodeWc( Node n )
	{
		StrandTable sT = m_strands.getStrandsFromNode( n.getNodeNr() );
		if ( sT.size() == 1 )
		{
			Iterator it = sT.iterator();
			Strand s = (Strand) it.next();
			return s;
		}// if
		else
		{
			Wc wc = n.getMainWcAtNode();
			Strand s = sT.getFirstStrandWithWc( wc );
			if( s != null )
				return s;
			
		}// else
		Iterator it = sT.iterator();
		Strand s = (Strand) it.next();
		return s;
	}// getStrandFromNodeWc
	/** This method creates a new state.
	 * @param netFile The name of the net file ( to read from ).
	 * @param rbFiel The name of the rb file ( to read from ).
	 * @return state A new instance of state.
	 */
	static public State create( File netFile, File rbFile, File channelFile, File rhbFile, File rhtFile ) throws Exception
	{
		State state = new State();
		state.readNet( netFile, rbFile, channelFile, rhbFile, rhtFile );
		return state;
	} // create
	
	/** Reads a net file of a specific format.
	 * @param netFile Name of the net file to be read.
	 * @param rbFile Name of the rb file to be read.
	 */
	protected void readNet( File netFile, File rbFile, File channelFile, File rhbFile, File rhtFile ) throws Exception
	{
		m_strands = new StrandTable();
		m_rbs = new RbTable();
		m_wcList = new WcTable();
		
		if ( netFile != null )
		{
			LogFile.log( "Read file " + netFile );
			readNetFile( netFile );
		}
		else LogFile.log( "No net file selected, file was not read!");
		
		// Sets State id for all Strands
		//m_strands.setStateId( getStateId() );
		if ( rbFile != null )
		{
			LogFile.log( "Read file " + rbFile);
			readRbFile( rbFile );
		}
		else LogFile.log( "No rb file selected, file was not read!");
		
		if ( channelFile != null )
		{
			LogFile.log( "Read file " + channelFile);
			readChannelFile( channelFile );
		}
		else LogFile.log( "No channel file selected, file was not read!");
		
		if ( rhbFile != null )
		{
			LogFile.log( "Read file " + rhbFile);
			readRhbFile( rhbFile );
		}
		else LogFile.log( "No rhb file selected, file was not read!");
		
		if ( rhtFile != null )
		{
			LogFile.log( "Read file " + rhtFile);
			readRhtFile( rhtFile );
		}
		else LogFile.log( "No rht file selected, file was not read!");
		//m_strands.strandStatsToLogFile();
	}// readNet
	
	/** This method generates a new State for a patial grid. It roots the Grid form
	 * a rootNode and chooses all attached strands up stream and writes the strands
	 * and nodes to a grid file in the format of the input grid file for the Kalypso
	 * model.
	 * @param nodeNr The node number of the root node in the grid.
	 */
	public State createSubState( Node n )
	{
		State newState = null;
		LogFile.log( "Root node of the system is nodeNr: " + n.getNodeNr() );
		
		StrandTable sT = getSubStrandTable( n );
		
		RbTable rbT = m_rbs.getSubRbTable( sT );
		
		WcTable wcT = m_wcList.getSubWcTable( sT );
		
		newState = new State();
		newState.m_strands = sT;
		newState.m_rbs = rbT;
		newState.m_wcList = wcT;
		
		return newState;
	} // createSubState
	
	/** Returns the upper most Strand in a net taking into account all four
	 * dependencies. watercourse net, aquifer net ( connected rbs ), inflow from
	 * other nodes and the low aquifer net. ( to be implemented: the dependencies
	 * of the rhb and rht ). ( recursive! )
	 * @param rootStrand The strand where to get the upper most strand from.
	 * @param sT A strandTable containing all strands allready found (tracker)
	 * @param rootStrand returns the upper most stand
	 */
	protected Strand getUpperMostStrand( Strand rootStrand, StrandTable sT ) throws Exception
	{
		
		if( sT.contains( rootStrand ) && sT.size() > 0 )
		{
			throw new KalypsoFilterException ("Circuar reference at Strand " + rootStrand.getStrandNr() );
		}// if
		
		sT.add( rootStrand );
		
		StrandTable upperStrands = getAllUpperStrandsFromStrand( rootStrand );
		
		Strand retStrand = upperStrands.getFirstStrandWithWc( rootStrand.getWcIndex() );
		if( retStrand != null )
			return getUpperMostStrand( retStrand, sT );
		
		Iterator it = upperStrands.iterator();
		if( it.hasNext() )
			return getUpperMostStrand( (Strand)it.next(), sT );
		
		return rootStrand;
	} // getUpperMostStrand
	/** Writes the state to the net file. Strands and riverbaisins are written to
	 * the net file.
	 * @param filename File name of the new net file.
	 * @param strandNr Strand number of the possible first strand.
	 * @exception Throws IOException if buffered writer is corrupted.
	 */
	protected void writeStateToNetFile( String filename, int strandNr ) throws Exception
	{
		NodeTable theNodes = m_strands.getNodes();
		File netFile = new File( filename + ".ntz" );
		BufferedWriter bw = new BufferedWriter( new FileWriter( netFile ) );
		try
		{
			java.util.Date date = new java.util.Date();
			DateFormat df;
			df = DateFormat.getTimeInstance(DateFormat.LONG, Locale.GERMANY );
			
			bw.write("\\ Kalypso\t" + df.format( date ) );
			writeStrandTable( bw, strandNr );
			theNodes.writeNodeTable( bw );
			bw.write("\\ end of net file " );
			bw.close();
		}// try
		catch( IOException e )
		{
			
		}// catch
		catch( Exception e )
		{
			bw.newLine();
			bw.write( "Error while writing net file: " + e.getMessage() );
			bw.close();
			if( netFile.length() == 0 )
				netFile.delete();
			throw e;
		}
	}// writeStateToNetFile
	
	/** Writes all the riverbasins of a state to the rb file.
	 * @param filename File name of the new rb file.
	 * @exception Throws IOException if buffered writer is corrupted.
	 */
	protected void writeStateToRbFile( String filename ) throws Exception
	{
		NodeTable theNodes = m_strands.getNodes();
		File rbFile = new File( filename + ".geb" );
		BufferedWriter bw = new BufferedWriter( new FileWriter( rbFile ) );
		try
		{
			m_rbs.writeRbToFile( bw );
			bw.close();
		}// try
		catch( IOException e )
		{
			bw.write( "Error while writing river basin file: " + e.getMessage() );
			throw e;
		}// catch
		finally
		{
			bw.close();
			if( rbFile.length() == 0 )
				rbFile.delete();
		}// finally
	}// writeStateToRbFile
	
	/** Writes the strands, the riverbasins and the channel data of a state
	 * in a XML file.
	 * @param stateName The state name used as filename.
	 * @param strand The strand to write to the file.
	 * @exception Throws IOException if one of the callsed method can not write to file.
	 */
	public void writeStateToXMLFile( File target ) throws IOException
	{
		NodeTable theNodes = m_strands.getNodes();
		BufferedWriter bw = new BufferedWriter( new FileWriter( target ) );
		//		m_strands.assignAdjStrands();
		try
		{
			bw.write( "<?xml version=\"1.0\" encoding=\"UTF-8\" ?> <theme>" );
			m_wcList.writeWcToXML( bw );
			m_strands.writeStrandsToXML( bw );
			m_rbs.writeRbToXML( bw );
			theNodes.writeNodesToXML( bw );
			m_strands.writeNodesRelWcToXML ( theNodes, bw );
			bw.write( " </theme>" );
			bw.close();
		}
		catch( IOException e )
		{
			bw.write( "Error while writing river basin file: " + e.getMessage() );
			throw e;
		}
		
	}// writeStateToXMLFile
	/** Writes the strands, the riverbasins and the channel data to the accoring
	 * files. It writes the net file, the channel file, the rb file, and the rht
	 * and/or rhb.
	 * @param stateName The state name used as filename.
	 * @param strand The strand to write to the file.
	 * @exception Throws IOException if one of the callsed method can not write to file.
	 */
	public void writeStateToFile( String stateName, Strand strand ) throws Exception
	{
		writeStateToNetFile( stateName , strand.getStrandNr() );
		writeStateToRbFile( stateName );
		m_strands.writeChannelToFiles( stateName );
		
	}// writeStateToFile
	
	/** Writes the net file ( strands and riverbasins ) in the right order ( for the calulation kernel of
	 * Kalypso ). If it is possible it follows the main wc and tries to write
	 * as many strands as possible one after another.
	 * @param bw BufferedWriter of the net file.
	 * @param strandNr The potential strand nummer to start writing.
	 * */
	protected void writeStrandTable( BufferedWriter bw, int strandNr ) throws Exception
	{
		StrandTable cpStrands = new StrandTable();
		// Makes a working copy of the strand table for this state
		cpStrands.addAll( m_strands );
		
		Strand firstStrand = m_strands.getStrand( strandNr );;
		
		while( m_strands.size() >  0 )
		{
			Iterator it = m_strands.iterator();
			
			if( firstStrand == null )
				firstStrand = (Strand)it.next();
			
			Strand lastStrand = getUpperMostStrand( firstStrand, new StrandTable() );
			RbTable rbAttachedToStrand = m_rbs.getAllRbFromStrand( lastStrand );
			writeStrandsToNetFile( bw, lastStrand, rbAttachedToStrand.size() );
			while( rbAttachedToStrand.size() > 0 )
			{
				Rb rb = rbAttachedToStrand.getOrderOfRbAtSameStrand();
				if( rb == null )
				{
					throw new KalypsoFilterException( "Circular reference of river basins at Strand "
														 + lastStrand.getStrandNr() + "/" + lastStrand.getWcIndex(),
													 KalypsoFilterException.CIRCULAR_REFFERENCE );
				}// if
				writeRbToNetFile( bw, rb );
				rbAttachedToStrand.remove( rb );
			}// while
			firstStrand = null;
			StrandTable sT = m_strands.getLowerStrands( lastStrand );
			if( sT != null )
				firstStrand = sT.getFirstStrandWithWc( lastStrand.getWcIndex() );
			if( firstStrand == null )
			{
				Iterator itr = sT.iterator();
				if( itr.hasNext() )
					firstStrand = (Strand)itr.next();
			}// if
			
			m_strands.remove( lastStrand );
		} // while
		bw.newLine();
		bw.write( Printf.format("%8.0d", "99999" ) );
		
		m_strands = cpStrands;
	} // writeStrandTable
	
	/** This method returns all rootStrands form river basins discharging water
	 * in the deep aquifer net.
	 * @param rootStrand The strand to get the contribution of the deep aquifer.
	 * @return rbsInflow A StrandTable containing all strands form the Rb that contribute to the rootStand.
	 */
	protected StrandTable getDeepAquifStrandsFromRb( Strand rootStrand )
	{
		StrandTable deepAquifStrands = new StrandTable();
		Iterator it = m_rbs.iterator();
		while( it.hasNext() )
		{
			Rb rb = ( Rb ) it.next();
			if( rb.getAquifTargetStrandDeepAquif() != null && rb.getAquifSplitAquiferDeep() < 1.0 )
			{
				Strand targetStrand = rb.getAquifTargetStrandDeepAquif();
				if ( rootStrand == targetStrand )
					deepAquifStrands.add( rb.getStrandRb() );
			}// if
		} // while
		return deepAquifStrands;
	}// getDeepAquifStrandsFromRb
	
	/** Gets a strand table of all connected nodes above the rootNode.
	 * @param rootNode The root node of the sub strand table.
	 */
	
	protected StrandTable getSubStrandTable( Node rootNode )
	{
		StrandTable st = new StrandTable();
		StrandTable rootStrands = m_strands.getUpperStrandsFromNode( rootNode );
		//Hier fängt der rooting algorithmus an, und hier springt er nach der rekursion wieder zurück
		// addUpperStrands ist der erste aufruf der rekursiven funktion.
		Iterator it = rootStrands.iterator();
		while( it.hasNext() )
		{
			Strand rootStrand = (Strand) it.next();
			addUpperStrands( rootStrand, st );
		}// while
		return st;
	}// getSubStrandTable
	
	/** Adds the rootstrand and all strands above to the strand table sT ( recursive ).
	 * @param rootStrand The base Strand to get the upper strand from.
	 * @param st StrandTable to add all upper strands to.
	 */
	protected void addUpperStrands( Strand rootStrand, StrandTable st )
	{
		if( st.contains( rootStrand ) )
			return;
		
		st.add( rootStrand );
		
		StrandTable upperStrands = getAllUpperStrandsFromStrand( rootStrand );
		Iterator it = upperStrands.iterator();
		while( it.hasNext() )
		{
			Strand s = ( Strand ) it.next();
			addUpperStrands( s, st );
		}// while
	}// addUpperStrands
	
	
	/** Finds all strands which lay directly above this rootStrand, meaning the
	 * strands which have to be calcualted before the rootStrand
	 * @param rootStrand The base strand to get all upper strands from.
	 */
	protected StrandTable getAllUpperStrandsFromStrand( Strand rootStrand )
	{
		StrandTable sT = new StrandTable();
		
		sT.addAll( m_strands.getUpperStrandsFromStrand( rootStrand ) );
		sT.addAll( m_rbs.getAquifStrandFromStrand( rootStrand ) );
		sT.addAll( m_strands.getConStrandsFromStrand( rootStrand ) );
		sT.addAll( getDeepAquifStrandsFromRb( rootStrand ) );
		// TO DO: rhb rsp. rht Überlaufknoten an einen anderen Strang abschlagen
		// implementieren
		
		sT.intersect( m_strands );
		
		return sT;
	}// getAllUpperStrandsFromStrand
	
	/** Add upper strands which are connected through an aquifer ( aquifer net )
	 * to a strand table. ( recursive )
	 * @param rootStrand The strand to get upper strand.
	 * @param st The strand table to add the rootStrand to.
	 */
	/*	protected void addUpperStrandsAquifer( Strand rootStrand, StrandTable st )
	 {
	 st.add( rootStrand );
	 Rb rootRb = m_rbs.getRb( rootStrand );
	 RbTable conRb = m_rbs.getRbsContributingToRb( rootRb );
	 StrandTable rootStrandConRb = conRb.getStrandsRb();
	 Iterator it = rootStrandConRb.iterator();
	 while( it.hasNext() )
	 {
	 Strand s = ( Strand ) it.next();
	 addUpperStrandsAquifer( s, st );
	 }// while
	 }// addUpperStrandsAquifer*/
	
	/** Returns all strand where the attached river basins discharge in a specific
	 * strand. It can contain itself.
	 * @param rootStrand The strand to get the attached strands from ( aquifer net).
	 * @return returnStrands The strands which dischage in the rootStrand.
	 */
	protected StrandTable getStrandsInflowAquiferFromRb( Strand rootStrand )
	{
		Rb rb = m_rbs.getRbFromStrand( rootStrand );
		
		RbTable rbTable = m_rbs.getConRbFromRb( rb );
		
		StrandTable newRootStrands = rbTable.getStrandsRb();
		
		// nur die Stränge zurückgeben, welche in m_strands sind
		StrandTable returnStrands = new StrandTable();
		Iterator it = newRootStrands.iterator();
		while( it.hasNext() )
		{
			Strand s = (Strand)it.next();
			if( m_strands.contains( s ) )
				returnStrands.add( s );
		}// while
		
		return returnStrands;
	}// getStrandsInflowAquiferFromRb
	/** Writes the strands to the net file.
	 * @param bw BufferedWriter of the net file.
	 * @param strand Strand to be written to file.
	 * @param nrOfRbs Number of attached riverbasins to the strand.
	 * @exception Throws IOException if buffered writer is corruped.
	 */
	protected void writeStrandsToNetFile( BufferedWriter bw, Strand strand, int nrOfRbs ) throws IOException
	{
		try
		{
			bw.newLine();
			String array[] = strand.strandToArray();
			array[3] = String.valueOf( nrOfRbs );
			String s = Printf.format( "%8.0d%8.0d%8.0d%4.0d %8.0s", array );
			bw.write( s );
			
		}
		catch( IOException  ef )
		{
			LogFile.log("Error, while writing strands to grid file!!" + ef);
			System.out.println( " Error, while writing strands to grid file!!" );
			ef.printStackTrace();
		}
	} // class WriteFile
	/** Writes the riverbasins to the net file.
	 * @param bw BufferedWriter of the net file.
	 * @param rb The riverbasin to write to net file.
	 * @exception Throws IOException if buffered writer is corruped.
	 */
	protected void writeRbToNetFile( BufferedWriter bw, Rb rb ) throws IOException
	{
		bw.newLine();
		String rbSt = Printf.format( "%8.0d", String.valueOf( rb.getNumberRb() ));
		bw.write( rbSt );
	}
	
	/** Reads a net file and adds all strands, riverbasins and nodes to the state.
	 * @param inputFile The name of the net file ( incl. path ).
	 */
	protected void readNetFile( File inputFile ) throws Exception
	{
		
		try
		{
			StreamTokenizer st = new StreamTokenizer(new InputStreamReader(new FileInputStream(inputFile)));
			st.parseNumbers(); // sets the tonenizer to parse for numbers
			st.commentChar('\\'); // defines the comment character
			st.wordChars( 'a', 'z' );
			st.wordChars( 'A', 'Z' );
			st.wordChars( '_', '_' );
			st.wordChars( '.', '.' );
			st.wordChars( '/', '/' );
			readStrandsFromNetFile( st );
			LogFile.log( "A total of " + getStrands().size() + " strands have been imported from " + inputFile.getName() + " file" );
			readNodesFromNetFile( st );
			LogFile.log( "A total of " + getStrands().getNodes().size() + " nodes have been imported from " + inputFile.getName() + " file" );
		}
		catch( Exception e )
		{
			javax.swing.JOptionPane.showMessageDialog( null, "Error while reading net-file" +inputFile.getName() );
			System.out.println(" Error Message: " + e);
			e.printStackTrace();
		}
	}
	/** This method reads the strands, nodes and river basins from the net file.
	 * First the Wc-Object of the strand is generated, then the nodeIn and
	 * nodeOut form typ Node are generated. The strand is just an aggrication of
	 * the nodes and the wc with the appropreate strandNr.
	 * @param st The current StreamTokanizer of net file beeing parsed.
	 * @exception Throws IOException if the stream tokanizer is corruped.
	 */
	
	protected void readStrandsFromNetFile( StreamTokenizer st ) throws IOException
	{
		Strand strandError = null;
		try
		    {
			while( st.nextToken() != StreamTokenizer.TT_EOF && ((int)st.nval != 9999 ^ (int)st.nval !=99999 ^ (int)st.nval != 999999) )
			    {
				/* strandNumber nodeInNr nodeOutNr nrOfRiverbasins wcIndex
				 * Rb 1
				 * ..
				 * Rb n
				 * Example:
				 *
				 * 100        100      101            1        abc34C7
				 * 100
				 * 101
				 */
				int number = (int)st.nval;
				st.nextToken();
				int nodeIn = (int)st.nval;
				st.nextToken();
				int nodeOut = (int)st.nval;
				st.nextToken();
				int nLines = (int)st.nval;
				st.nextToken();
				String wcIndex = null;
				if( st.sval != null )
				    wcIndex = st.sval;
				else
				    wcIndex = "dummy";
				System.out.println("wcIndex:"+wcIndex);
				Wc wc = m_wcList.addNew( wcIndex );
				
				
				Node inNode = m_strands.getNodeFromStrand( nodeIn );
				Node outNode = m_strands.getNodeFromStrand( nodeOut );
				// checking if the node already exists
				if ( inNode != null )
				    inNode = inNode; // ??
				else // if not generate it
				    {
					Node in = new Node( nodeIn );
					inNode = in;
				    }
				// checking if the node already exists
				if ( outNode != null )
				    outNode = outNode; // ??
				else // if not generate it
				    {
					Node out = new Node( nodeOut );
					outNode = out;
				    }
				
				Strand s = new Strand( number, inNode, outNode, wc );
				strandError = s;
				m_strands.add( s );
				if ( nLines > 0 ) // overreads nodes with no Rb attached
				{
				    for( int i = 0; i < nLines; i++ )
					{
					    st.nextToken();
					    Rb rbalt = new Rb( ( int )st.nval, s );
					    m_rbs.add( rbalt );
					}// for i
				}// if
			    } // while
			m_strands.assignAdjStrands();
		    }
		catch ( Exception e )
		{
			LogFile.log( "The parser aborted at " + strandError.toString() );
		}
	}// readStrandsFromNetFile
	
	/** This method reads the hydrological inforamtion for each node form the
	 * net file and fills the object node with this data.
	 * @param st The current StreamTokanizer of net file beeing parsed.
	 * @exception Throws IOException if stream tokanizer is corruped or IllegalAccessException if node is not existing.
	 */
	protected void readNodesFromNetFile( StreamTokenizer st ) throws Exception
	{
		Node nodeError = null;
		try
		{
			while ( st.nextToken() != StreamTokenizer.TT_EOF && ((int)st.nval != 9999 ^ (int)st.nval !=99999 ^ (int)st.nval != 999999) )
			{
				/* Format:
				 * nodeNr         hydrological typs
				 * value OR
				 * value targetNode OR
				 * Index    path
				 *
				 * Example:
				 * 100     0   0   0   0   0     OR
				 *
				 * 100     1   0   0   0   0
				 *   0.018                       OR
				 *
				 * 100     0   1   0   0   0
				 *   0.013   405
				 * 100     0   0   0   1   0
				 * 45Ac3
				 * c:\\work\project\inflow\funct.***
				 */
				Node node = m_strands.getNodeFromStrand( ( int ) st.nval );
				if( node == null )
				{
					throw new KalypsoFilterException( "The Node " + st.nval + " is not existing!",
													 KalypsoFilterException.INVALID_ELEMENT);
				}
				nodeError = node;
				int[] flagsHydro = new int[5];
				// read the hydrological typs of each node, all flags can be set
				for ( int i = 0 ; i < 5 ; i++)
				{
					st.nextToken();
					flagsHydro[i] = (int) st.nval;
				}// for i
				
				for( int i = 0; i < 5; i++ )
				{
					if( flagsHydro[i] > 0  )
					{
						switch( i )
						{
							/* Reading the Data for a constant inflow */
							case 0:
								{
									HydroData hd = node.createHydroData();
									
									if( hd.inflowToNode == null )
									{
										st.nextToken();
										hd.addInToNode( st.nval );
									}
									break;
								}// case 0
								/* Reading the Data for a constant outflow */
							case 1:
								{
									HydroData hd = node.createHydroData();
									
									if( hd.outflowToNode == null )
									{
										st.nextToken();
										double value = st.nval;
										st.nextToken();
										Node targetNode = m_strands.getNodeFromStrand( ( int ) st.nval ) ;
										
										if( targetNode == null )
										{
											throw new KalypsoFilterException( "The Node " + (int) st.nval
																				 + ", recieving const. outflow, is not existing!",
																			 KalypsoFilterException.INVALID_ELEMENT );
										}
										
										hd.addOutToNode( value, targetNode );
									}// if
									break;
								}// case 1
								/* Reading the Data for the case of overflow*/
							case 2:
								{
									HydroData hd = node.createHydroData();
									
									if( hd.overFlow == null )
									{
										st.nextToken();
										double value = st.nval;
										st.nextToken();
										Node targetNode = m_strands.getNodeFromStrand( (int) st.nval );
										if( targetNode == null )
										{
											throw new KalypsoFilterException( "The Node " + (int) st.nval
																				 + ", recieving overflow, is not existing!",
																			 KalypsoFilterException.INVALID_ELEMENT );
										}
										hd.addOverflow( value, targetNode );
									}//if
									break;
								}// case 2
								/* Reading an In- or Output funktion*/
							case 3:
								{
									HydroData hd = node.createHydroData();
									
									if( hd.inflowToNodeFunct == null )
									{
										// If true: assign input function
										if( flagsHydro[i] < 3 )
										{
											st.nextToken();
											Node targetNode = m_strands.getNodeFromStrand( (int) st.nval );
											if( targetNode == null )
											{
												throw new KalypsoFilterException( "The Node " + (int) st.nval
																					 + " (input function) is not existing!",
																				 KalypsoFilterException.INVALID_ELEMENT );
											}
											st.nextToken();
											String path = st.sval;
											hd.addInToNodeFunct( path, targetNode );
											break;
										}// if
									}// if
									
									if( node.getHydroData().outflowToNodeFunct == null )
									{
										// if true: assign output function
										if( flagsHydro[i] > 2 )
										{
											st.nextToken();
											Node targetNode = m_strands.getNodeFromStrand( (int) st.nval );
											if( targetNode == null )
											{
												throw new KalypsoFilterException( "The Node " + (int) st.nval
																					 + " (output function) is not existing!",
																				 KalypsoFilterException.INVALID_ELEMENT );
											}
											
											st.nextToken();
											String path = st.sval;
											hd.addOutToNodeFunct( path, targetNode );
											break;
										}// if
									}// if
									break;
								}// case 3
								/* reading Extraction form node ( units: percent ) */
							case 4:
								{
									HydroData hd = node.createHydroData();
									
									if( node.getHydroData().precentExtract == null )
									{
										st.nextToken();
										double value = st.nval;
										st.nextToken();
										Node targetNode = m_strands.getNodeFromStrand( (int) st.nval );
										if( targetNode == null )
										{
											throw new KalypsoFilterException( "The Node " + (int) st.nval
																				 + ", extracting precentage discharge, is not existing!",
																			 KalypsoFilterException.INVALID_ELEMENT );
										}
										hd.addPercentExtract( value, targetNode );
									}// if
									break;
								}//case 4
						} //swtich
					} // if flagsHydro[]
				} // for i
			}// while read the hydrological data for the nodes
		}
		catch( Exception e )
		{
			LogFile.log( "The parser aborted at " + nodeError.toString() );
			throw e;
		}
	}// readNodesFromNetFile
	
	
	/** This method reads the river basin file ( ***.geb )and fills the already
	 * existing rb objects with data.
	 * @param inputFile The path and the filename of the river basin file.
	 * @exception Throws IllegalAccessException if wc is not existing.
	 */
	protected void readRbFile ( File inputFile ) throws Exception
	{
		try {
			StreamTokenizer st = new StreamTokenizer( new InputStreamReader( new FileInputStream( inputFile ) ) );
			st.parseNumbers();
			// Everything after this character in the actual line is disguarded
			st.commentChar('\\');
			// These characters are word tokens, meaning the sval returns not null
			st.wordChars('a', 'z');
			st.wordChars('A', 'Z');
			st.wordChars( '.','.' );
			st.wordChars( '_','_' );
			//parsing the ***.geb file
			while( st.nextToken() != StreamTokenizer.TT_EOF )
			{
			    while(st.sval!=null)
				{
				    System.out.println("ueberspringe: "+st.sval);
				    st.nextToken();
				}
			    

			        int rbNumber = (int)st.nval;
				System.out.println("rbnumber:"+st.nval);
				st.nextToken();
				int hydTopFlag = (int)st.nval;
				System.out.println("hydTopFlag:"+st.nval);
				st.nextToken();
				String wcIndex = st.sval;
				Wc wc = m_wcList.getWc( wcIndex );
				if( wc == null )
				{
				    LogFile.log( "The watercourse index " + wcIndex + " has not been found. Check Rb file" );
				    throw new KalypsoFilterException( " Watercourse index not found. Check Log file !",
													KalypsoFilterException.FILE_FORMAT_ERROR );
				}// if
				Rb rb = m_rbs.getRb( rbNumber, wc.getWcIndex() );
				if( rb == null )
				{
				    LogFile.log( "River basin is not existing or river basin number and wcIndex do not match."  );
				    throw new KalypsoFilterException( "\nRiver basin NOT FOUND!\nnumber = " + rbNumber + "\nwcindex = " +
														 wcIndex, KalypsoFilterException.INVALID_ELEMENT );
				}// if
				// reads all the comment tokens ( ONLY STRINGS AND NUMBERS ARE ALLOWED!! )
				int lineNoComment = st.lineno();
				boolean isNumberToken = false;
				if( st.nextToken() == StreamTokenizer.TT_NUMBER )
					isNumberToken = true;
				if( st.lineno() == lineNoComment )
				{
					// Checks if the comment token is a number or a string
					if( isNumberToken != true )
						rb.setComment( st.sval );
					else
						rb.setComment( String.valueOf( st.nval ) );
					// Checks if there is still a comment token
					while( lineNoComment == st.lineno() )
					{
						if( st.nextToken() == StreamTokenizer.TT_WORD && ( st.sval != null ) )
							rb.setComment( st.sval );
						else // this works because token in the next line is allways a number
							if ( st.lineno() == lineNoComment )
								rb.setComment ( String.valueOf( st.nval ) );
					}
				}
				double area = st.nval;
				System.out.println("area:"+area);
				rb.setFlagHydrotop( hydTopFlag );
				st.nextToken();
				int lineNr2 = st.lineno();
				rb.setKey( st.sval );
				System.out.println("key:"+st.sval);
				st.nextToken();
				if( st.sval != null )
				{
					if( st.sval.substring( st.sval.lastIndexOf( '.' )).equals(".kz"))
					{
						rb.setFileShortTerm( st.sval );
						System.out.println(".kz-file:"+st.sval);
						st.nextToken();
					}
				}
				if( st.sval != null )
				{
					if( st.sval.substring( st.sval.lastIndexOf( '.' )).equals(".lz"))
					{
						rb.setFileLongTerm( st.sval );
						st.nextToken();
					}
				}
				if( st.sval != null )
				{
					if( st.sval.substring( st.sval.lastIndexOf( '.' )).equals(".kz"))
					{
						rb.setFileShortTerm( st.sval );
						st.nextToken();
					}
				}
				if( st.lineno() == lineNr2 )
				{
					rb.setCorrRain( st.nval );
					st.nextToken();
				}
				rb.setFileClimate( st.sval ); // *.tv
				System.out.println(".tv-file:"+st.sval);

				st.nextToken();
				rb.setFileTimeAreaFunct( st.sval ); // *.zft
				st.nextToken();
				rb.setFileHydrotop( st.sval ); // *.hyd
				System.out.println(".hyd-file:"+st.sval);

				st.nextToken();
				rb.setSnowWaterCont( st.nval );
				st.nextToken();
				rb.setSnowMaxWaterCont( st.nval );
				st.nextToken();
				rb.setSnowMeltingRateTemp( st.nval );
				st.nextToken();
				rb.setSnowMeltingRateRad( st.nval );
				st.nextToken();
				rb.setInitSnowHeight( st.nval );
				System.out.println("initSnowHeight:"+st.nval);
				st.nextToken();
				rb.setCorrTemp( st.nval );
				st.nextToken();
				rb.setCorrEvap( st.nval );
				st.nextToken();
				rb.setSealedAreaRb( st.nval * area );
				System.out.println("versiegelungsgrad:"+st.nval);

				rb.setNatAreaRb( ( 1 - st.nval ) * area );
				st.nextToken();
				int nLayers = (int)st.nval;
				System.out.println("nLayers:"+nLayers);
				st.nextToken();
				rb.setCorrMaxInter( st.nval );
				st.nextToken();
				rb.setCorrInitInter( st.nval );
				// read soil correction factors for each soil layer
				for ( int j = 0 ; j < nLayers ; j ++ )
				{
					Vector v1 = new Vector(); // Vector to add correction Factors
					for ( int i = 0 ; i < 8 ; i++ )
					{
						st.nextToken();
						v1.addElement( new Double ( st.nval ) );
					}// for i
					rb.setCorrFactSoil( v1 );
				}// for j
				st.nextToken();
				rb.setCorrUsage( st.nval );
				st.nextToken();

				rb.setInitInterception( st.nval );
				st.nextToken();
				rb.setInitAquif( st.nval );
				st.nextToken();
				// setfInit(); method is empty
				st.nextToken();
				//setfTra(); method is emtpy
				st.nextToken();
				rb.setRetSealedArea( st.nval );
				st.nextToken();
				rb.setRetOverlandFlow( st.nval );
				st.nextToken();
				rb.setRetInterflow( st.nval );
				st.nextToken();
				rb.setRetAquif( st.nval );
				st.nextToken();
				rb.setRetAquifNeighbour( st.nval );
				st.nextToken();

				double debug1= st.nval ;
				System.out.println("RetOutflowAqufDeep:"+debug1);
				rb.setRetOutflowAquifDeep( debug1 );
				st.nextToken();
				// read amount of connected river basins
				int nrOfOutConRb = ( int ) st.nval;
				//read attached Ribs
				//Rb 1    Rb 2  ...  Rb n    wcIndex 1   wcIndex 2 .... wcIndex n
				//value 1 value 2    value n
				if( nrOfOutConRb != 0 )
				{
					rb.createOutConRb();
					Vector vRb = new Vector();
					Vector vWcRb = new Vector();
					for ( int i = 0 ; i < 2*nrOfOutConRb ; i++ )
					{
						if(i < nrOfOutConRb )
						{
							st.nextToken();
							double rbConNr = st.nval;
							vRb.addElement( new Double ( rbConNr ) );
						}// else
							
						else
						{
							st.nextToken();
							String index = st.sval;
							if( index == null )
							{
								errorAtElement( rb, "Error while reading connected riverbasin from file!" );
								throw new KalypsoFilterException( "Error while reading connected riverbasin watercourse index "
																	 + rb.getNumberRb(), KalypsoFilterException.INVALID_ELEMENT );
							}
							vWcRb.addElement( new String  ( index ) );
						}// if
						
					}// for i
					
					// read the value of discharg to each connnected rib
					Vector vVal = new Vector();
					for ( int j = 0 ; j < nrOfOutConRb ; j++ )
					{
						st.nextToken();
						double value = st.nval;
						vVal.addElement( new Double ( value ) );
					}// for j
					// assigns the connected rb to this rb and sets the discharge values
					m_rbs.addConRb( vRb, vWcRb, vVal ,rb);
				}// if
				st.nextToken();
				rb.setAquifMinHeightChannel( st.nval );
				st.nextToken();
				rb.setAquifMaxHeightChannel( st.nval );
				st.nextToken();
				rb.setAquifSplitAquiferDeep( st.nval );
				st.nextToken();
				rb.setAquifPorosity( st.nval );
				st.nextToken();
				rb.setAquifExtract( st.nval );
				int lineNr = st.lineno();
				st.nextToken();
				rb.setAquifInflowDeepAquifVal( st.nval );
				st.nextToken();
				int targetNodeDeepAquif = ( int ) st.nval;
				Strand s = m_strands.getStrandWithInNode( targetNodeDeepAquif );
				/* The targetNodeDeepAquifer is a memeber of the deep aquifer strand.
				 * A deep aquifer Strand has always the strand type id 2!
				 */
				if( s == null && targetNodeDeepAquif != 0 )
				{
				    LogFile.log( "Deep Aquifer Strand is not existing."  );
					throw new KalypsoFilterException( "targetNodeDeepAquifer " +  targetNodeDeepAquif + " is not existing.",
													 KalypsoFilterException.INVALID_ELEMENT );
				}// if
				if( s != null )
				{
					st.nextToken();
					if( lineNr == st.lineno() && st.sval.equals( s.getWcIndex().getWcIndex() ))
					{
						rb.setAquifTargetStrandDeepAquif( s );
					}
				}

				//				st.nextToken(); // ende
				//				st.nextToken(); // gebietsdatei

			}//while reading a river basin block
			//getRibs().writeConRbToRbToFile( "c:\\listOfRbConRb.lst" );
		}// try
		catch( Exception e )
		{
			System.out.println("Error Message: " + e);
			e.printStackTrace();
			throw e;
		}
		
	} // readRbFile
	
	/** This method reads the channel file and fills the strand object with the
	 * according data.
	 * @param inputFile The file to be parsed.
	 * @exception Throws IllegalAccessException if a strand is not found.
	 */
	public void readChannelFile ( File inputFile ) throws Exception
	{
		try {
			StreamTokenizer st = new StreamTokenizer(new InputStreamReader(new FileInputStream(inputFile)));
			System.out.println("reading file:"+inputFile);
			st.parseNumbers();
			st.commentChar('\\');
			st.wordChars('a', 'z');
			st.wordChars('A', 'Z');
			
			
			while( st.nextToken() != StreamTokenizer.TT_EOF )
			{
				//read all existing strand data from the ***.ger file
				Strand s = m_strands.getStrand( (int)st.nval );
				System.out.println("strand: "+st.nval);
				if( s == null )
				{
					LogFile.log ( "Error while reading rhb or rht file !!! " );
					throw new KalypsoFilterException( "Strand " + (int)st.nval + " is not existing.",
													 KalypsoFilterException.INVALID_ELEMENT );
				}
				
				
				st.nextToken();
				int strandType = ( int ) st.nval;
				// if the this Token (Strand Type) is zero there is no Data to be assigned to the strand
				
				StrandData sd = s.setType( strandType );
				sd.readChannelFromFile( st );
			}// while
		}
		catch( Exception e )
		{
			System.out.println(" Error Message: " + e);
			e.printStackTrace();
			throw e;
		}
	}// read ChannelFile
	
	/** This method reads the rht file and fills the strand ( Type 3 ) object
	 * with the according data.
	 * @param inputFile The file to be parsed.
	 * @exception Throws IOException if stream tokanizer is corruped.
	 * @exception Throws IllegalAccessException if at strand was not found, this causes the program to terminate.
	 */
	public void readRhtFile ( File inputFile ) throws Exception
	{
		try {
			StreamTokenizer st = new StreamTokenizer(new InputStreamReader(new FileInputStream(inputFile)));
			st.parseNumbers();
			st.commentChar('\\');
			st.wordChars('a', 'z');
			st.wordChars('A', 'Z');
			st.wordChars( '.','.' );
			st.wordChars( '_','_' );
			
			while( st.nextToken() != StreamTokenizer.TT_EOF )
			{
				//read all existing strand data from the ***.rht file
				st.nextToken();
				Strand s = m_strands.getStrand( (int)st.nval );
				
				int type = s.getType();
				if( type == 0 )
					throw new KalypsoFilterException( "Strand " + s.getStrandNr() + " was already declared a fictional strand",
													 KalypsoFilterException.FILE_FORMAT_ERROR );
				if( type == 1 )
					throw new KalypsoFilterException( "Strand " + s.getStrandNr() + " was already declared a Gerinne strand",
													 KalypsoFilterException.FILE_FORMAT_ERROR );
				if( type == 2 )
					throw new KalypsoFilterException( "Strand " + s.getStrandNr() + " was already declared a rhb strand",
													 KalypsoFilterException.FILE_FORMAT_ERROR );
				
				if( type == 3 )
				{
					Rht rht = (Rht) s.getData();
					st.nextToken();
					Node targetNode = m_strands.getNodeFromStrand( (int) st.nval );
					if(targetNode==null)
					    System.out.println("there is no strand, that is connected to the node "+st.nval);
					else
					    System.out.println("targetNode: "+st.nval);
					rht.setTargetNode( targetNode );
					st.nextToken();
					rht.setCFact( st.nval );
					st.nextToken();
					int nrOfSets = (int) st.nval;
					st.nextToken();
					rht.setEvapFile( st.sval );
					st.nextToken();
					rht.setLongTermSimFile( st.sval );
					
					rht.readRhtFromFile( st, nrOfSets );
				}
				st.nextToken();
				
			}// while
		}
		catch( Exception e )
		{
			System.out.println(" Error Message: " + e);
			e.printStackTrace();
			throw e;
		}
	}// readRhtFile
	/** This method reads the rhb file and fills the strand ( Type 2 ) object
	 * with the according data.
	 * @param inputFile The file to be parsed.
	 */
	public void readRhbFile ( File inputFile ) throws Exception
	{
		try {
			StreamTokenizer st = new StreamTokenizer(new InputStreamReader(new FileInputStream(inputFile)));
			System.out.println("reading "+inputFile);
			st.parseNumbers();
			st.commentChar('#');
			st.wordChars('a', 'z');
			st.wordChars('A', 'Z');
			
			
			while( st.nextToken() != StreamTokenizer.TT_EOF )
			{
				//read all existing strand data from the ***.rht file
				st.nextToken();
				Strand s = m_strands.getStrand( (int)st.nval );
				System.out.println("\n\nSrandNr:"+st.nval);
				int type = s.getType();
				if( type == 0 )
				    throw new KalypsoFilterException( "Strand " + s.getStrandNr() + " was already declared a fictional strand",
								      KalypsoFilterException.FILE_FORMAT_ERROR );				
				if( type == 1 )
				    throw new KalypsoFilterException( "Strand " + s.getStrandNr() + " was already declared a Gerinne strand",
								      KalypsoFilterException.FILE_FORMAT_ERROR );				
				if( type == 2 )
				{
					Rhb	rhb = (Rhb) s.getData();
					st.nextToken();
					Node targetNode = m_strands.getNodeFromStrand( (int) st.nval );
					rhb.setTargetNode((int)st.nval);
					System.out.println("rhb targetNode: "+st.nval);
					rhb.setTargetNode( targetNode );
					st.nextToken();
					rhb.setCFact( st.nval );
					// Komentarzeile eventuel zeilen weise tokenizen
					rhb.readRhbFromFile( st );
				}
				if( type == 3 )// noch nicht richtig da erst von oben kopiert
					throw new KalypsoFilterException( "Strand " + s.getStrandNr() + " was already declared a rhb strand",
													 KalypsoFilterException.FILE_FORMAT_ERROR );
			}// while
		}
		catch( Exception e )
		{
			System.out.println(" Error Message: " + e);
			e.printStackTrace();
			throw e;
		}
	}// readRhtFile
	public void xmlParser( String xmlFileName ) throws Exception
	{
		try
		{
			m_strands = new StrandTable();
			m_wcList = new WcTable();
			m_rbs = new RbTable();
			GisTransferManager gisHashMap = new GisTransferManager( xmlFileName );
			LogFile.log( "Parsing of the file " + xmlFileName + " has been successful!" );
			gisHashMap.mapGisObjects( m_strands, m_wcList, m_rbs );
			LogFile.log( "Mapping of objects has been successful." );
			gisHashMap.nrOfObjectsToLogFile();
			System.out.println( "end of import KalypsoXml ..." );
		}
		catch( Exception e )
		{
			System.out.println( "Error Message: " + e );
			e.printStackTrace();
			throw e;
		}
	}
	public void errorAtElement( Object o, String message ) throws Exception
	{
		if( o instanceof Strand )
		{
			Strand s = (Strand) o;
			LogFile.log( "Error at Strand" + s.getStrandNr() );
			LogFile.log( message );
			throw new Exception( "Error at\nStrand number = " + s.getStrandNr() + "\nCheck Log file." );
		}
		if( o instanceof Node )
		{
			Node n = (Node) o;
			LogFile.log( "Error at Node" + n.getNodeNr() );
			LogFile.log( message );
			throw new Exception( "Error at\nNode number = " + n.getNodeNr() + "\nCheck Log file." );
		}
		if( o instanceof Wc )
		{
			Wc wc = (Wc) o;
			LogFile.log( "Error at watercourse " + wc.getWcName() + " with wcIndex = " + wc.getWcIndex() );
			LogFile.log( message );
			throw new Exception( "Error at\nwatercourse " + wc.getWcIndex() + "\nCheck Log file." );
		}
		if( o instanceof Rb )
		{
			Rb rb = (Rb) o;
			LogFile.log( "Error at Riverbasin" + rb.getNumberRb() );
			LogFile.log( message );
			throw new Exception( "Error at\nNode number = " + rb.getNumberRb() + "\nCheck Log file." );
		}
	}
	private void init()
	{
		StrandTable st = new StrandTable();
		WcTable wcT = new WcTable();
		RbTable rbT = new RbTable();
		m_strands = st;
		m_wcList = wcT;
		m_rbs = rbT;
	}
	public void stateStatsToLogFile()
	{
		m_strands.strandStatsToLogFile();
		m_rbs.rbStatsToLogFile();
	}

    public HashSet getAllFileClimate()
    {
	RbTable ribs=getRibs();
	if(ribs!=null)
	    return ribs.getAllFileClimate();
	return new HashSet();
    }

    public HashSet getAllFileShortTerm()
    {
	RbTable ribs=getRibs();
	if(ribs!=null)
	    return ribs.getAllFileShortTerm();
	return new HashSet();
    }
    public HashSet getAllFileLongTerm()
    {
	RbTable ribs=getRibs();
	if(ribs!=null)
	    return ribs.getAllFileLongTerm();
	return new HashSet();
    }
} // WcState

