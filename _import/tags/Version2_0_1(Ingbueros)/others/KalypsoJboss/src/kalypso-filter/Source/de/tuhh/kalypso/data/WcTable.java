package de.tuhh.kalypso.data;

import java.io.BufferedWriter;
import java.io.IOException;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Vector;

import de.tuhh.kalypso.data.strand.Strand;

/** The class WcTable.java contains all existing watercourses in this program. The
 * implementation of the HashSet asures that the a watercourse only exists once.
 *
 * @author <A HREF="mailto:c.kuepferle@tuhh.de">Christoph Küpferle</A>
 * @version WcTable.java,v 1.0 2002/07/01
 */
public class WcTable extends HashSet
{
	public WcTable(){}
	/** This method creates a new Wc object and returns it. If it is existing it
	 * returns the existing Wc object, if it not existing it creates a new Wc
	 * object and returns it.
	 * @param wcIndex The wcIndex to be created ( String )
	 * @return wc The new Wc object or the existing Wc object.
	 */
	public Wc addNew( String wcIndex )
	{
		Iterator it = iterator();
		while( it.hasNext() )
		{
			Wc w = ( Wc ) it.next();
			if( w.getWcIndex().equals( wcIndex ) )
				return w;
		}// while
		
		Wc wc = new Wc( wcIndex );
		add( wc );
		return wc;
	}
	public Wc addNew( String name, String wcIndex )
	{
		Iterator it = iterator();
		while( it.hasNext() )
		{
			Wc w = ( Wc ) it.next();
			if( w.getWcIndex().equals( wcIndex ) )
				return w;
		}// while
		
		Wc wc = new Wc( name, wcIndex );
		add( wc );
		return wc;
	}
	/** This method gets a subset of Wc's from a set of Strands.
	 * @param sT The strand table to get the Wc's from.
	 * @return subSetWc A WcTable containing all Wc's occuring in the strand set.
	 */
	public WcTable getSubWcTable( StrandTable sT )
	{
		WcTable subSetWc = new WcTable();
		Iterator it = iterator();
		while( it.hasNext() )
		{
			Wc wc = ( Wc ) it.next();
			Iterator it2 = sT.iterator();
			while( it2.hasNext() )
			{
				Strand strand = ( Strand ) it2.next();
			if( wc == strand.getWcIndex() )
					subSetWc.add( wc );
			}//while
		}// while
		return subSetWc;
	}// getSubWcTable
	/** This method returns the object Wc with the desired wcIndex.
	 * @param wcIndex The wcIndex of the Wc to be returned.
	 * @return wc The Wc with the wcIndex.*/
	public Wc getWc( String wcIndex )
	{
		Iterator it = iterator();
		while( it.hasNext() )
		{
			Wc wc = ( Wc ) it.next();
			if( wc.getWcIndex().equals( wcIndex  ) )
				return wc;
		}// while
		return null;
	}// getWc
	public void writeWcToXML( BufferedWriter bw ) throws IOException
	{
		Iterator it = iterator();
		bw.write( " <table key=\"wc\">" );
		while( it.hasNext() )
		{
			Wc wc = (Wc) it.next();
			// Simple Properties
					bw.write( " <o ID=\"" + wc.hashCode() + "\"> <sp m_wcIndex=\""
								 + wc.getWcIndex() + "\" m_wcName=\""
								 + wc.getWcName() + "\" /> </o>");
		}// while
		bw.write( " </table>" );
	}// writeWcToXML
	public Vector getWcIndexList()
	{
		Vector vect = new Vector();
		Iterator it = this.iterator();
		while( it.hasNext() )
		{
			Wc wc = (Wc) it.next();
			vect.add( wc.getWcIndex() );
		}
		return vect;
	}
} // class WcTable
