package de.tuhh.kalypso.data;
import de.tuhh.kalypso.data.node.Node;

import de.tuhh.kalypso.data.riverbasin.Rb;
import de.tuhh.kalypso.data.riverbasin.Rb.RbOutConnected;

import de.tuhh.kalypso.data.strand.Strand;

import de.tuhh.kalypso.util.LogFile;
import de.tuhh.kalypso.util.ErrorHandler.KalypsoFilterException;
import de.tuhh.kalypso.util.KeyGenerator;
import de.tuhh.kalypso.util.Printf;

import java.util.TreeSet;
import java.util.Iterator;
import java.util.Vector;
import java.util.Locale;
import java.util.HashMap;
import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;
import java.util.HashSet;

/** This class RbTable.java containts a river basin collections (TreeSet). The
 * TreeSet ensures that the River basins are listetd in assending order from smallest
 * river basin number to the largest and only one object with a hashcode is existing.
 *
 * @author <A HREF="mailto:c.kuepferle@tuhh.de">Christoph Küpferle</A>
 * @version RbTable.java,v 1.0 2002/07/01
 */

public class RbTable extends TreeSet
{
	
    /** Finds the river basin which has node as the in node of its rootStrand.
     * @param node The node to be the inNode of the rootStrand of the rb.
     * @return rb Where the rootstrand of the rb has node as inNode.
     */
    public Rb getRbFromNode( Node node )
    {
	Iterator itRbs = iterator();
	while( itRbs.hasNext() )
	    {
		Rb rb = ( Rb )itRbs.next();
		if( rb.getStrandRb().getNodeIn() == node )
		    return rb;
	    }// while
	return null;
    } // getRbFromNode
	
    /** Gets the RiverBasis with where s equals the rootStrand of rb.
     * @param s The strand where rb is attached
     * @return rb The river basin with rootStrand s, otherwies null.
     */
    public Rb getRbFromStrand( Strand s )
    {
	Iterator itRbs = iterator();
	while( itRbs.hasNext() )
	    {
		Rb rb = ( Rb )itRbs.next();
		if( rb.getStrandRb() == s )
		    return rb;
	    }// while
		
	return null;
    } // getRbFromStrand
    /** Returns the rb with rbNummer equals riverbasin number and wc equals the wc
     * of the rootStrand.
     * @param rbNumber River basin number.
     * @param wc The wcIndex of the rb to find.
     * @return rb The rb where the above parameters match, otherwise null.*/
    public Rb getRb( int rbNumber, String wc )
    {
	Iterator it = iterator();
	while( it.hasNext() )
	    {
		Rb rb = ( Rb ) it.next();
		int number = rb.getNumberRb();
		Wc index = rb.getWcIndexRb();
		if ( rb.getNumberRb() == rbNumber  && rb.getWcIndexRb().getWcIndex().equals( wc ) )
		    return rb;
	    }// while
	return null;
    }// getRb
    /** Returns an rb with a specific rbNumber.
     * @param rbNumber The number of the rb to be found.
     * @return rb The rb where its number equals rbNumber, otherwise null.*/
    public Rb getRb( int rbNumber )
    {
	Iterator it = iterator();
	while( it.hasNext() )
	    {
		Rb rb = ( Rb ) it.next();
		if ( rb.getNumberRb() == rbNumber )
		    return rb;
	    }// while
	return null;
    }
    /** This method finds all contributing Rb ( Aquifer Net ) to a specified Rb.
     * @param findRb The specified river basin to recieve water throw Aquifers.
     * @return rbsInflow A table of river basins which contribute groundwater to findRb.
     */
    public RbTable getConRbFromRb( Rb findRb )
    {
	RbTable rbsInflow = new RbTable();
	Iterator it = iterator();
	while( it.hasNext() )
	    {
		Rb rb = ( Rb ) it.next();
		if( rb.getOutConRbVect() != null )
		    {
			RbTable conRb = rb.getOutConRbRbTable();
			if( conRb != null )
			    {
				Iterator itconRb = conRb.iterator();
				while( itconRb.hasNext() )
				    {
					Rb rbInCon = ( Rb ) itconRb.next();
					if( rbInCon == findRb )
					    rbsInflow.add( rb );
				    } // while
			    }//if
		    }// if
	    } // while
	return rbsInflow;
    }// getConRbFromRb
	
	
    /** This method gets the strand where the Rb is assigned to. If there are more
     * then one Rb, all strands where the Rbs are assigned to will be returned.
     * @return rootStrands A StrandTable conntaining all the strands of the Rb where they are assigned to.
     */
    public StrandTable getStrandsRb()
    {
	StrandTable rootStrands = new StrandTable();
	Iterator it = iterator();
	while( it.hasNext() )
	    {
		Rb rb = ( Rb ) it.next();
		rootStrands.add( rb.getStrandRb() );
	    }// while
	return rootStrands;
    }// getStrandsRb
    /** Returns the river basin with the rootStrand s.
     * @param s The rootStrand to be found.
     * @return rb The rb with the strand s as rootStrand, otherwise null.*/
    public Rb getRb( Strand s )
    {
	Iterator it = iterator();
	while( it.hasNext() )
	    {
		Rb rb = ( Rb ) it.next();
		if( rb.getStrandRb() == s )
		    return rb;
	    }// while
	return null;
    }// getRbWithStrand
	
    /** This method fills the class RbOutConnected of a river basin.
     * @param vRb Vector containing the connected river basins where the parent rb discharges its water to.
     * @param vVal Vector containing the discharge of the parent river basin to the conncected the river basin.
     * @param rb Parent river basin.
     * @exception Throws IllegalAccessException if the river basin does not exist.
     */
    public void addConRb( Vector vRb, Vector vWcRb , Vector vVal, Rb rb ) throws Exception
    {
	for( int i = 0 ; i < vRb.size() ; i++ )
	    {
		int rbConNr = ( int ) ( ( Double ) vRb.elementAt( i ) ).doubleValue();
		double value = ( ( Double ) vVal.elementAt( i ) ).doubleValue();
		String wcIndex = ( ( String ) vWcRb.elementAt( i ) );
		Rb rbCon = getRb( rbConNr, wcIndex );
		if( rbCon == null )
		    {
			LogFile.log( "River basin is not existing or river basin number and wcIndex do not match."  );
			String message= "River basin " + rbConNr + "\nwcindex = " +wcIndex + " is not existing.";
			LogFile.log(message);
				
			rbStatsToLogFile(); // debug
			throw new KalypsoFilterException(message,KalypsoFilterException.INVALID_ELEMENT);
		    }// if
		rb.createRbOutConnected( rbCon, value );
	    }// for
    }// addConRb
	
    /** This method gets all contributing Rbs to a specific Rb.
     * @param rb The Rb where the other rbs contribute water.
     * @retrun result The RbTable containing the contributing Rbs.
     */
    public RbTable getRbsContributingToRb( Rb rb )
    {
	Vector rbList = getListRbAquiferRb();
	RbTable result = new RbTable();
	Iterator it = rbList.iterator();
	while( it.hasNext() )
	    {
		Vector vect = ( Vector ) it.next();
		Rb rb1 =( Rb ) ( vect.firstElement() );
		if( rb1 == rb )
		    {
			Iterator it2 = vect.iterator();
			it2.next();
			while( it2.hasNext() )
			    {
				Rb rbCon = ( Rb ) it2.next();
				result.add( rbCon );
			    }// while it2
			return result;
		    }//if
	    }// while it
	return null;
    }// getRbsContributingToRb
    /** This method gets a subset of Rb's attached to a set of Strands.
     * @param sT The strand table to get the attached rbs from.
     * @return subSetRb A RbTable containing all attached rbs to the strand set.
     */
    public RbTable getSubRbTable( StrandTable sT )
    {
	RbTable subSetRb = new RbTable();
	Iterator it = iterator();
	while( it.hasNext() )
	    {
		Rb rb = ( Rb ) it.next();
		Strand s = rb.getStrandRb();
		Iterator it2 = sT.iterator();
		while( it2.hasNext() )
		    {
			Strand strand = ( Strand ) it2.next();
			if( s == strand )
			    subSetRb.add( rb );
		    }//while it2
	    }// while it
	return subSetRb;
    }// getSubRbTable
	
    /** This method returns all Rb attached to the rootStrand.
     * @param rootStrand The Strand to find all the attached Rbs.
     * @return subSetRb The RbTable containing all attached Rb's.
     */
    public RbTable getAllRbFromStrand( Strand rootStrand )
    {
	RbTable attachedRbs = new RbTable();
	Iterator it = iterator();
	while( it.hasNext() )
	    {
		Rb rb = ( Rb ) it.next();
		Strand s = rb.getStrandRb();
		if( s == rootStrand )
		    attachedRbs.add( rb );
	    }// while it
	return attachedRbs;
    }// getAllRbFromStrand
	
    /** This method returns all nodes which contribute water to the node n via
     * groundwater ( aquifer net )
     * @param node The node which recieves the water
     * @return nT NodeTable contining all nodes that contribute water to node*/
    /*	public NodeTable getAquifierFromNode( Node node )
	{
	Rb rb = getRbFromNode( node );
	 
	NodeTable nT = new NodeTable();
	 
	RbTable rbTable = getConRbFromRb( rb );
	Iterator it = rbTable.iterator();
	while( it.hasNext() )
	{
	Rb upRb = (Rb)it.next();
	 
	nT.add( upRb.getStrandRb().getNodeOut() );
	} // while it
	 
	return nT;
	} // getAquifierFromNode*/
    /** This method returns the most upper strand ( just 1. level ) which is connected
     * through aquifer net ( connected river basins ). It removes itself at the end.
     * @param rootStrand The strand where the upper strands contibute water to.
     * @return sT StrandTable containing all upper strands ( 1st level only ).
     */
    public StrandTable getAquifStrandFromStrand( Strand rootStrand )
    {
	RbTable rbConToStrand = getAllRbFromStrand( rootStrand );
	Iterator it = rbConToStrand.iterator();
	RbTable rbTable = new RbTable();
	while( it.hasNext() )
	    {
		Rb rb = ( Rb ) it.next();
		rbTable.addAll( getConRbFromRb( rb ) );
	    }// while
	StrandTable sT = rbTable.getStrandsRb();
		
	sT.remove( rootStrand );
		
	return sT;
    } // getAquifStrandFromStrand
    /** This method determines the order of Rbs at a Strand. The Strand can have
     * in general n Rbs attached ( for Kalypso not more then 4 ). The method is
     * returning the Rb that does not get any water from another attached Rb. If
     * method returns null, the Rbs are referenced in a circle.
     * @return rb
     */
    public Rb getOrderOfRbAtSameStrand()
    {
		
	RbTable rbTable = new RbTable();
	rbTable.addAll( this );
	if( rbTable.size() == 1 )
	    {
		Iterator it = rbTable.iterator();
		Rb rb = (Rb) it.next();
		return rb;
	    }// if
	Iterator it1 = iterator();
	while( it1.hasNext() )
	    {
		Rb rb1 = (Rb) it1.next();
		Iterator it2 = iterator();
		while( it2.hasNext() )
		    {
			Rb rb2 = (Rb) it2.next();
			if( rb1 == rb2 )
			    continue;
			RbTable rbT = rb2.getOutConRbRbTable();
			if(rbT == null )
			    continue;
			Iterator it3 = rbT.iterator();
			while( it3.hasNext() )
			    {
				Rb rb3 = (Rb) it3.next();
				if( rb1.getNumberRb() == rb3.getNumberRb() && rb1.getWcIndexRb().getWcIndex().equals( rb3.getWcIndexRb().getWcIndex() ) )
				    rbTable.remove( rb1 );
				if( rbTable.size() == 0 )
				    return null;
			    }// while it3
		    }//while it2
	    }//while it1
	Iterator it = rbTable.iterator();
	Rb rb = (Rb) it.next();
	return rb;
    }// getOrderOfRbAtSameStrand
    /** This method dumps a RbTable to the console ( System.out ).*/
    public void dump()
    {
	if( size() > 1 ^ size() == 0 )
	    System.out.println( "Rb Table with " + size() + " Rbs" );
	else System.out.println( "Rb Table with " + size() + " Rb" );
		
	Iterator it = iterator();
	while( it.hasNext() )
	    {
		Rb rb = ( Rb ) it.next();
		System.out.println( " dump Rb: " + rb.toString() );
	    }// while
    }// dump
    /** This method writes a riverbasin table to the rb-file.
     * @param bw The BufferedWriter of the rb-file.
     * @exception throws IOException if bw is corrupted. */
    public void writeRbToFile( BufferedWriter bw ) throws Exception
    {
	Printf.setLocale( Locale.ENGLISH );
	try
	    {
		Iterator it = iterator();
		while( it.hasNext() )
		    {
			Rb rb = (Rb) it.next();
			if( rb.getFlagHydrotop() == true )
			    {
				String array[] = {String.valueOf( rb.getNumberRb() ), String.valueOf( "7" ) };
				bw.write( Printf.format("%16.0d%7d", array ) );
			    }// if
			else
			    bw.write( Printf.format("%16.0d", String.valueOf( rb.getNumberRb() ) ) );
			bw.newLine();
			if( rb.getComment() != null )
			    bw.write( rb.getStrandRb().getWcIndex().getWcIndex()+ " " + rb.getComment());
			else
			    bw.write( rb.getStrandRb().getWcIndex().getWcIndex() );
			bw.newLine();
			bw.write( Printf.format("%11.3f", String.valueOf( rb.getTotAreaRb() ) ) );
			bw.newLine();
			bw.write( rb.getKey() );
			if( ( rb.getFileShortTerm() != null ) && ( rb.getFileLongTerm() != null ) )
			    {
				bw.write( Printf.format( " %s %s", new String[] { String.valueOf( rb.getFileLongTerm() ), String.valueOf( rb.getFileShortTerm() ) } ) );
				if( rb.getCorrRain() > 0 )
				    {
					int length = rb.getFileLongTerm().getName().length() +
					    rb.getFileShortTerm().getName().length() + 3;
					bw.write( Printf.format( " %" + length + ".2f", String.valueOf( rb.getCorrRain() )));
				    }
			    }
			if( ( rb.getFileShortTerm() != null ) && ( rb.getFileLongTerm() == null ) )
			    {
				bw.write( Printf.format( " %s", new String[] { String.valueOf( rb.getFileShortTerm() )}));
				if( rb.getCorrRain() > 0 )
				    {
					int length = rb.getFileShortTerm().getName().length() + 2;
					bw.write( Printf.format( " %" + length + ".2f", String.valueOf( rb.getCorrRain() )));
				    }
			    }
			if( ( rb.getFileLongTerm() != null ) && ( rb.getFileShortTerm() == null ) )
			    {
				bw.write( Printf.format( " %s", new String[] { String.valueOf( rb.getFileLongTerm() )}));
				if( rb.getCorrRain() > 0 )
				    {
					int length = rb.getFileLongTerm().getName().length() + 2;
					bw.write( Printf.format( " %" + length + ".2f", String.valueOf( rb.getCorrRain() )));
				    }
			    }
			bw.newLine();
			bw.write( rb.getFileClimate().getName() );
			bw.newLine();
			bw.write( rb.getFileTimeAreaFunct().getName() );
			bw.newLine();
			bw.write( rb.getFileHydrotop().getName() );
			bw.newLine();
			bw.write( Printf.format("%6.2f%9.2f%9.2f%9.2f%9.0d%9.2f%9.2f", rb.snowDataToArray() ) );
			bw.newLine();
			bw.write(Printf.format("%5.3f%5.0d%10.1f%10.1f", rb.interceptionToArray() ) );
			bw.newLine();
			for( int i = 0 ; i < rb.getNrOfLayerCorrFactSoil() ; i++ )
			    {
				bw.write( Printf.format("%11.1f %5.1f %5.1f %5.2f %5.1f %5.1f %5.1f %5.1f", rb.corrFactSoilLayerToArray( i ) ) );
				//bw.write( Printf.format("%11.1f%6.1f%6.1f%6.1f%6.1f%6.1f%6.1f%6.1f", rb.corrFactSoilLayerToArray( i ) ) );
				bw.newLine();
			    }// for
				
			bw.write( Printf.format("%4.1f%7.1f%8.1f%6.2f%7.2f", rb.initialValToArray() ) );
			bw.newLine();
			bw.write( Printf.format("%10.2f%10.2f%10.2f%10.2f%10.2f%10.2f", rb.retConstToArray() ) );
			bw.newLine();
			bw.write( Printf.format("%4.0d", String.valueOf( rb.getNrOfConRb() ) ) );
			if( rb.getNrOfConRb() > 0 )
			    {
				bw.newLine();
				String format = "%8.0d";
				String format1 = " %8.0s";
				for( int j = 0 ; j < rb.getNrOfConRb() ; j++ )
				    format = format + format;
				for( int j = 0 ; j < rb.getNrOfConRb() ; j++ )
				    format1 = format1 + format1;
				bw.write( Printf.format( format, rb.conRbToArrayNr()) );
				bw.write( Printf.format( format1, rb.conRbToArrayWcIndex() ) );
				bw.newLine();
				format = "%8.2f";
				for( int j = 0 ; j < rb.getNrOfConRb() ; j++ )
				    format = format + format;
				bw.write( Printf.format(format, rb.conRbValToArray() ) );
			    }// if
			bw.newLine();
			bw.write( Printf.format("%8.2f%8.2f%8.2f%8.2f%13.8f%6.2f%8.0d%20s", rb.aquifDataToArray() ) );
			bw.newLine();
			bw.write("\\end of data block river basin " + rb.getNumberRb() );
			bw.newLine();
		    }// while
		Printf.setLocale( Locale.getDefault() );
	    }// try
	catch( Exception e )
	    {
		LogFile.log("Error while writing river basin to file! " + e.getMessage() );
		throw e;
	    }// catch
	finally
	    {
		Printf.setLocale( Locale.getDefault() );
	    }// finally
    }// rbToFile
    /** This method writes a riverbasin table to an XML file.
     * @param bw The BufferedWriter of the XML-file.
     * @exception throws IOException if bw is corrupted. */
    public void writeRbToXML( BufferedWriter bw ) throws IOException
    {
	try
	    {
		Iterator it = iterator();
		while( it.hasNext() )
		    {
			Rb rb = (Rb) it.next();
			bw.write( " <table key=\"rb\">" );
			rb.writeRbToXML( bw );
			bw.write( " </o> </table>" );
			// Relation rb2strand
			if( rb.getStrandRb() != null )
			    {
					
				bw.write( " <table key=\"rb2strand\"> <o ID=\"" + KeyGenerator.getKey()
					  + "\">" );// <sp />");
				bw.write( " <rel srcKey=\"rb\" srcID=\"" +
					  rb.hashCode() + "\" destKey=\"" + rb.getStrandRb().getTypeName() + "\" destID=\""
					  + rb.getStrandRb().hashCode()
					  + "\" /> </o>");
				bw.write( " </table>" );
			    }// if
			// Relation rb2deepAquif
			if( rb.getAquifTargetStrandDeepAquif() != null )
			    {
				bw.write( " <table key=\"rb2deepAquif\"> <o ID=\"" + KeyGenerator.getKey()
					  + "\"> <sp value=\""
					  + rb.getAquifInflowDeepAquifVal() + "\" />" );
				bw.write( " <rel srcKey=\"rb\" srcID=\"" +
					  rb.hashCode() + "\" destKey=\"" + rb.getStrandRb().getTypeName() + "\" destID=\""
					  + rb.getAquifTargetStrandDeepAquif().hashCode()
					  + "\" /> </o>");
				bw.write( " </table>" );
			    }
			// Relation wc2objects
			if( rb.getStrandRb() != null )
			    {
				bw.write( " <table key=\"wc2objects\"> <o ID=\"" + KeyGenerator.getKey()
					  + "\"> <sp />");
				bw.write( " <rel srcKey=\"wc\" srcID=\"" +
					  rb.getStrandRb().getWcIndex().hashCode()
					  + "\" destKey=\"rb\" destID=\""
					  + rb.hashCode()
					  + "\" />");
				bw.write( " </o> </table>" );
			    }// if
			// Relation rb2rb river basins connected
			if( rb.getOutConRbVect() != null )
			    {
				bw.write( " <table key=\"rb2rb\">" );
				Iterator itcon = rb.getOutConRbVect().iterator();
				while( itcon.hasNext() )
				    {
					RbOutConnected rbConOut = ( RbOutConnected ) itcon.next();
					bw.write( " <o ID=\"" + KeyGenerator.getKey() + "\"> <sp value=\""
						  + rbConOut.getConRbValue() + "\" />");
					bw.write( " <rel srcKey=\"rb\" srcID=\"" +
						  rb.hashCode() + "\" destKey=\"rb\" destID=\""
						  + rbConOut.getConRb().hashCode()
						  + "\" /> </o>");
				    }// while
				bw.write( " </table>" );
			    }// if
		    }// while
	    }// try
	catch( IOException ioe )
	    {
		javax.swing.JOptionPane.showMessageDialog(null, "Error while writing river basin to file!","Error writing file", javax.swing.JOptionPane.ERROR_MESSAGE );
		LogFile.log("Error while writing river basin to file!");
	    }// catch
	finally
	    {
		LogFile.close();
	    }// finally
    }// rbToFile
    public void writeConRbToRbToFile( String filename ) throws IOException
    {
	Vector list = getListRbAquiferRb();
		
		
	BufferedWriter bw = new BufferedWriter( new FileWriter( filename ) );
	for( int i = 0; i < list.size(); i++ )
	    {
		Vector vect = (Vector) list.get( i );
		if( vect.size() > 1 )
		    {
			Rb rbFrom = (Rb) vect.elementAt( 0 );
			bw.write( "RbNr " + rbFrom + " recieves water from" );
			bw.newLine();
			for( int j = 1; j < vect.size(); j++ )
			    {
				Rb rbTo = (Rb) vect.elementAt( j );
				bw.write( "Rb :\t" + rbTo.toString() );
				bw.newLine();
			    }
			bw.newLine();
		    }// if
	    }
	bw.close();
    }
    /** This method gets a list of Rbs and their contributing riverbasins ( Aquifer )
     * The resulting Vector containes Vectors of the following format.
     * [ rb  ( recieving  from ), rb 1, ... rb n ]
     * @return rbList A Vector conatining Vectors with Rb in the above format.
     */
    public Vector getListRbAquiferRb()
    {
	Vector rbList = new  Vector();
	Iterator it = iterator();
	while( it.hasNext() )
	    {
		Vector vect = new Vector();
		Rb rb = ( Rb ) it.next();
		vect.add( rb );
		Iterator it2 = iterator();
		while ( it2.hasNext() )
		    {
			Rb rbGetCon = ( Rb ) it2.next();
			if( rb == rbGetCon)
			    continue;
			RbTable rbTable = rbGetCon.getOutConRbRbTable();
			if( rbTable != null )
			    {
				Iterator it3 = rbTable.iterator();
				while( it3.hasNext() )
				    {
					Rb rb3 = ( Rb ) it3.next();
					if( rb3 == rb )
					    vect.add( rbGetCon );
				    }
			    }// if
		    }// while it 2
		rbList.add( vect );
	    }// while it
	return rbList;
    }// getListRbAquiferRb
    public void rbStatsToLogFile()
    {
	 
	HashMap map = getNrOfRbPerWcIndex();
	Iterator set = map.keySet().iterator();
	LogFile.log("");
	LogFile.log( "Total number of riverbasins in this river system: " + this.size() );
	while( set.hasNext() )
	    {
		String key = (String) set.next();
		LogFile.log( "A total of " + (String) map.get( key ) +  " riverbasins belong to the wcIndex " + key );
	    }
    }
    public HashMap getNrOfRbPerWcIndex()
    {
	HashMap results = new HashMap();
	Iterator itS = iterator();
	while( itS.hasNext() )
	    {
		String c = null;
		Rb r = (Rb) itS.next();
		c = (String) results.get( r.getStrandRb().getWcIndex().getWcIndex() );
		if( c != null )
		    c = String.valueOf( Integer.parseInt( c ) + 1 );
		else c = String.valueOf( 1 );
		results.put( r.getStrandRb().getWcIndex().getWcIndex(), c );
	    }
	return results;
    }


    public HashSet getAllFileClimate()
    {
	HashSet results=new HashSet();
	Iterator itRbs = iterator();
	while( itRbs.hasNext() )
	    {
		Rb rb = ( Rb )itRbs.next();
		results.add(rb.getFileClimate().toString());
	    }// while
	return results;
    }

    public HashSet getAllFileShortTerm()
    {
	HashSet results=new HashSet();
	Iterator itRbs = iterator();
	while( itRbs.hasNext() )
	    {
		Rb rb = ( Rb )itRbs.next();
		results.add(rb.getFileShortTerm().toString());
	    }// while
	return results;
    }
    
} // class RbTable
