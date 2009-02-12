package de.tuhh.kalypso.data.strand;

import java.io.IOException;
import java.io.StreamTokenizer;
import java.util.Vector;
/** The class NasimData.java represents an other data foramt for channel data. It
 * is not directly used for calcualtion in Kalypso. If the nasim data is assigned
 * all fields have to be filled with data.
 *
 * @author <A HREF="mailto:c.kuepferle@tuhh.de">Christoph Küpferle</A>
 * @version Strand.java,v 1.0 2002/07/01
 */

public class Nasim
{
	/** The mean velocity in the crosssection of the channel.
	 * @param m_channelVelocity Units m/s, default value: -1.0*/
	private double m_channelVelocity = -1.0;
	/** The mean Lenght of the channel section.
	 * @param m_channelLength Units m, default value: -1.0*/
	private double m_channelLength = -1.0;
	/** The mean slope of the channel section.
	 * @param m_channelSlope Units m/m, default value: -1.0*/
	private double m_channelSlope = -1.0;
	/** Vector holding the data of the crosssections in this channel section
	 * @param m_nasimData The default state is an empty vector*/
	private Vector m_nasimData = new Vector();
	
	public Nasim( double velocity, double length, double slope )
	{
		this.m_channelVelocity = velocity;
		this.m_channelLength = length;
		this.m_channelSlope = slope;
	}
	
	/** Sets the mean slope of the channel section.
	 * @param val The slope of the channel*/
	public void setChannelSlope( double val ) { m_channelSlope = val; }
	/** Returns the mean slope of this channel.
	 * @return m_channelSlope*/
	public double getChannelSlope() { return m_channelSlope; }
	/** Sets the mean velocity of the channel section.
	 * @param val The Velocity in m/s in the channel*/
	public void setChannelVelocity( double val ) { m_channelVelocity = val; }
	/** Returns the mean velocity in this channel section.
	 * @return m_channelSlope*/
	public double getChannelVelocity() { return m_channelVelocity; }
	/** Sets the mean length of the channel section.
	 * @param val Length of the channel*/
	public void setChannelLength( double val ) { m_channelLength = val; }
	/** Returns the length of this channel section.
	 * @return m_channelLength in m*/
	public double getChannelLength() { return m_channelLength; }
	/** Returns the vector holding the nasim data sets for the staions in the
	 * channel section.
	 * @retrun m_nasimData */
	public Vector getNasim() { return m_nasimData; }
	/** Returns the i-th element of the vector holding the nasim data for this
	 * channel section.
	 * @param i The index of the element to get.
	 * @return NasimData*/
	public NasimData getNasimData( int i ) { return (NasimData) m_nasimData.elementAt( i ); }
	/** This method adds a new set of NasimData to the nasimData vector of this
	 *  strand.
	 * @param st The stream tokenizer of the parsed channel file.
	 *  @exception Throws IOException if the stream tokenizer is corrupted.
	 */
	public void addNewNasimData( StreamTokenizer st ) throws IOException
	{
		NasimData nd = new NasimData();
		st.nextToken();
		nd.nasimWidth = st.nval;
		System.out.println("nasimWidth:"+st.nval);

		st.nextToken();
		nd.nasimHeight = st.nval;
		System.out.println("nasimHeight:"+st.nval);

		m_nasimData.add( nd );
	}// addNewNasimData
	
	/** The inner class NasimData representing a set of data including the width
	 *  and height of the channel cross section in this channel section.*/
	public	class NasimData
	{
		/** The field holding the width of the cross section
		 * @param nasimWidth Default value: -1.0*/
		public double nasimWidth = -1.0;
		/** The field holding the height of the cross section
		 * @param nasimHeight Default value: -1.0*/
		public double nasimHeight = -1.0;
	}// inner class NasimData
}// class Nasim
