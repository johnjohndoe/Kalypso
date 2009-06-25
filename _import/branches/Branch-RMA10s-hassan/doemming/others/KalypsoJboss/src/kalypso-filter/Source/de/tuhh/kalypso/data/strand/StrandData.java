package de.tuhh.kalypso.data.strand;

import java.io.StreamTokenizer;
import java.io.IOException;
import java.io.BufferedWriter;

import de.tuhh.kalypso.util.xmlParser.GisTransferObject;

/** This abstract class StrandData.java orgainizes the data for the following
 * four different possible strand types.
 *<UL>
 *<LI> Null channel, no retention, only transport from inNode to outNode of strand.
 *<LI> Channel, using n elements of liniar storage cascade. ( Kalinin-Miliuokov )
 *<LI> Rhb, a strand defined as a reservoir with a finite volume and a specific volume discharge function.
 *<LI> Rht, a strand defined as a reservoir which accounts also for evapoartion, and supports reservoir control funcitons for different seasons.
 *</UL>
 *
 * @author <A HREF="mailto:c.kuepferle@tuhh.de">Christoph Küpferle</A>
 * @version StrandData.java,v 1.0 2002/07/01
 */
public abstract class StrandData
{
    abstract int getType();


	/** Creates a one of the four possible data objects for a strand and returns
	 * the created object. The type number determines which object is generated.
	 * @param type The type number a integer form 0 to 3. if the number lays outside of the intervall it returns null.*/
	static StrandData createData( int type )
	{
		switch( type )
		{
			case 0:
				return new NullChannel();
			case 1:
				return new Channel();
			case 2:
				return new Rhb();
			case 3:
				return new Rht();
				
			default:
				return null;
		}// switch
	}// createData
	abstract public StrandData getStrandData();
	/** This method reads data form the channel file.
	 * @param st StreamTokanizer of the file stream of the channel file.
	 * @throws Throws IOException when stream is corrupted. */
	public	void readChannelFromFile( StreamTokenizer sT ) throws Exception {};
	/** This method reads a data triplet from the rhb file.
	 * @param st StreamTokanizer form the rhb file beeing parsed
	 * @exception Throws an IOException if the stream tokanizer is corrupted*/
	public	void readRhbFromFile( StreamTokenizer sT ) throws Exception {};
	/** This method reads a complete sesional function for this reservoir.
	 * @param st StreamTokanizer form the rhb file beeing parsed.
	 * @param nrOfSets The number of sesional functions for this reservoir.
	 * @exception Throws an IOException if the stream tokanizer is corrupted.*/
	public	void readRhtFromFile( StreamTokenizer sT ) throws Exception {};
	/** This method writes the Channel data to the channel file.
	 * @param bw BufferedWriter of the channel file.
	 * @throws Throws IOException if the buffered writer is corrupted.*/
	public	void writeChannelToFile( BufferedWriter bwChannel ) throws Exception {};
	/** This method writes the Channel data to the channel file.
	 * @param bw BufferedWriter of the channel file.
	 * @throws Throws IOException if the buffered writer is corrupted.*/
	public	void writeRhbToFile( BufferedWriter bwRhb ) throws IOException {};
	/** Writes all rows contained in a sesional function to the rht file.
	 * @param bw BufferedWriter of the rht file.
	 * @exception Throws IOException if bufferd writer is corrupted.*/
	public	void writeRhtToFile( BufferedWriter bwRht ) throws IOException {};
	public void mapStrandData( GisTransferObject  gisTransferObject ) throws Exception {};
}// Class StrandData

