package de.tuhh.kalypso.data.strand;

import java.io.BufferedWriter;
import java.io.IOException;
import java.io.StreamTokenizer;
import java.util.Iterator;
import java.util.Vector;

import de.tuhh.kalypso.data.strand.Nasim.NasimData;
import de.tuhh.kalypso.util.Printf;
import de.tuhh.kalypso.util.xmlParser.GisTransferObject;
import de.tuhh.kalypso.util.xmlParser.VectorSet;

/**
 * This class Channel.java holds all data to describe a natural Channel. The member
 * m_channelData is a Vector containing Objects of the class ChannelData. The member
 * m_nasim holds data form an external software package <b>Nasim</b>.
 
 * @param m_distribFact A number between zero and one describing how much water flows in the channal and how much on the reparian land.
 
 * @param m_masim The member holding the nasim data format. @see de.tuhh.kalypso.data.strand.Nasim.
 *
 * @author <A HREF="mailto:c.kuepferle@tuhh.de">Christoph Küpferle</A>
 * @version Channel.java,v 1.0 2002/07/01
 */
public class Channel extends StrandData
{
	/** The member holding the boardfull discharge of the channel section.
	 @param m_qBoardfull m³/s ( Default value: -1 )*/
	private double m_qBoardfull = -1.0;
	/** The member holding the paramter to distribut the discharge to the main channel and the reparian land.
	 @param m_distibFact A number between zero and one.*/
	private double m_distribFact = -1.0;
	
	/** Holding Kalinin-Miliuokove parameter for a strand.
	 * @param m_channelData The member ( Vector ) holding rows of data.
	 * For the format @see de.tuhh.kalypso.data.strand.Channel.
	 *  ( this Vector must always contain 5 Elements !!! model constraint)*/
	private Vector m_channelData = new Vector();
	/** Holding the nasim data for a Channel, beeing the ave. velocity, length and
	 * the slope of the Channel. @see de.tuhh.kalypso.data.strand.Nasim
	 * @param m_nasim Data in nasim format. ( default value null )*/
	private Nasim m_nasim = null;
	
	public void mapStrandData( GisTransferObject gto )
	{
		setQBoardFull( Double.parseDouble( gto.getSimpleProperty( "m_qBoardfull" )));
		setDistFact( Double.parseDouble( gto.getSimpleProperty( "m_distribFact" )));
		
		Vector data = getChannelData();
		
		VectorSet vs = gto.getVectorSet( "m_channelData" );
		if( vs != null )
		{
			for( int row = 0; row < vs.size(); row++ )
			{
				double q = Double.parseDouble( vs.getSimpleProperty( "v_q", row ) );
				double kRiver = Double.parseDouble( vs.getSimpleProperty( "v_kRiver", row ) );
				double nRiver = Double.parseDouble( vs.getSimpleProperty( "v_nRiver", row ) );
				double kLand = Double.parseDouble( vs.getSimpleProperty( "v_kLand", row ) );
				double nLand = Double.parseDouble( vs.getSimpleProperty( "v_nLand", row ) );
				ChannelData ch = new ChannelData(q, kRiver, nRiver, kLand, nLand);
				data.add( ch );
			}
		}
	}
	
	public void setQBoardFull( double val ) { m_qBoardfull = val; }
	public double getQBoardFull() { return m_qBoardfull; }
	public void setDistFact( double val )  { m_distribFact = val; }
	public double getDistFact() { return m_distribFact; }
	public Nasim getNasim() { return m_nasim; }
	public int getType() { return 1; }
	public Vector getChannelData() { return m_channelData; }
	public int getNumberOfStorageElements()
	{
		int size = m_channelData.size();
		return size;
	}// getNumberOfStorageElements
	/** This inner class contains one linaer channel storage element of a Strand.
	 * Kalinin Miliuokov Paramter.
	 */

	public class ChannelData
	{
		/** The discharge in the channel.
		 * @param discharge The discharge m³/s */
		public double discharge = -1.0;
		/** Retention konstant ( Kalinin Miliuokov ) of the main watercourse.
		 * @param kRiver*/
		public double kRiver = -1.0;
		/** Number of linear storage elements to model the retetntion of a channel.
		 * @param nRiver Number of linear storage elemnts of the river.*/
		public double nRiver = -1.0;
		/** Retention konstant ( Kalinin Miliuokov ) of the overland flow.
		 @param kLand*/
		public double kLand = -1.0;
		/** Number of linear storage elements to model the retetntion on the reparian land.
		 @param nLand Number of linear storage elements for the reparien area*/
		public double nLand = -1.0;
		
		ChannelData( double q, double kR, double nR, double kL, double nL )
		{
			this.discharge = q;
			this.kRiver = kR;
			this.nRiver = nR;
			this.kLand = kL;
			this.nLand = nL;
		}
		/** This constructor reads a row of channel data ( optionaly nasim data ).
		 * @param st points to the first element of the row in the file. Once this
		 * section has parsed the data it must point to the next element ( Token )
		 * @exception Throws an IOException if the stream tokanizer is corrupetd.
		 */
		ChannelData( StreamTokenizer st ) throws IOException
		{
			discharge = st.nval;
			System.out.println("discharge:"+st.nval);
			st.nextToken();
			
			kRiver = st.nval;
			System.out.println("kRiver:"+st.nval);
			st.nextToken();
			
			nRiver = st.nval;
			System.out.println("nRiver:"+st.nval);
			st.nextToken();
			
			kLand = st.nval;
			System.out.println("kLand:"+st.nval);
			st.nextToken();
			
			nLand = st.nval;
			System.out.println("nLand:"+st.nval);
			
			Nasim nd = getNasim();
			if( nd != null )
				nd.addNewNasimData( st );
			
		}// end of constuctor
	}// inner class ChannelData
	/** Returns the type of this Object.
	 * @param g  Channel object ( this ).*/
	public StrandData getStrandData()
	{
		Channel g = this;
		return g;
	}// getStrandData
	
	/** This method reads data form the channel file.
	 * @param st StreamTokanizer of the file stream of the channel file.
	 * @throws Throws IOException when stream is corrupted.
	 */
	public	void readChannelFromFile( StreamTokenizer st ) throws IOException
	{
		// the Channel data is to be read
		// q         c      OR with Nasim data    q       c       v        l       s
		// 0.05231  0.5287                    0.05231  0.5287  0.658    14.1253  0.0012598
		int sameLine = st.lineno() + 1;
		st.nextToken();
		setQBoardFull( st.nval );
		System.out.println("qboardfull:"+st.nval);
		st.nextToken();
		setDistFact( st.nval );
		System.out.println("distfact:"+st.nval);
		st.nextToken();
		if( st.lineno() == sameLine)// check if there is optional data channel velocity
		{
			
			double velocity = st.nval;
			System.out.println("velocity:"+st.nval);
			st.nextToken();
			
			double length = st.nval;
			System.out.println("length:"+st.nval);
			st.nextToken();
			
			double slope = st.nval;
			System.out.println("slope:"+st.nval);			
			
			Nasim nasim = new Nasim( velocity, length, slope );
			
			m_nasim = nasim;
			st.nextToken();
		}//if
		
		// Read 5 channel data rows
		// q         kRiver       nRiver        kLand          nLand      OR with nasim
		// 0.02568  0.043630     30.000000      0.000000      0.000000
		// q         kRiver       nRiver        kLand          nLand      nasimWidth  nasimHeight
		// 0.02568  0.043630     30.000000      0.000000      0.000000     0.867987   141.923658
		int j = 0;
		do{
			m_channelData.add( new ChannelData( st ) );
			j = j + 1;
			//Dieses if muss hier stehen damit für den nächsten block das Token an der richtigen Stelle steht.
			if( j < 5 )
				st.nextToken();
			
		} while( j < 5 );
	}// readChannelFromFile
	/** This method returns the head of the channel data as an arry of Strings.
	 * Format: qBoardfull, distFact OR <BR>
	 * qBoardfull, distFact, velocity, lenght, slope ( with nasim ) depending if
	 * nasim data exists.
	 * @return array An array containing the Strings to be written to the file.
	 */
	protected String[] headOfChannelToArray()
	{
		Nasim nd = getNasim();
		if( nd == null )
		{
			String array[] = { String.valueOf( getQBoardFull() ), String.valueOf( getDistFact() ) };
			return array;
		}// if
		else
		{
			
			String array[] = { String.valueOf( getQBoardFull() ), String.valueOf( getDistFact() ),
					String.valueOf( nd.getChannelVelocity() ), String.valueOf( nd.getChannelLength() ),
					String.valueOf( nd.getChannelSlope() ) 	};
			return array;
		}// else
	}// headOfChannelToArray
	/** This method returns a row of the channel data as an arry of Strings.
	 * Format: discharge, kRiver, nRiver, kLand, nLand OR <BR>
	 * discharge, kRiver, nRiver, kLand, nLand, nasim width, nasim height.
	 * if nasim data exists.
	 * @return array An array containing the Strings to be written to the file.
	 */
	protected String[] dataOfChannelToArray( int i )
	{
		Nasim nasim = getNasim();
		if( nasim == null )
		{
			ChannelData cd = (ChannelData) getChannelData().elementAt( i );
			String array[] = { String.valueOf( cd.discharge ), String.valueOf( cd.kRiver ),
					String.valueOf( cd.nRiver ), String.valueOf( cd.kLand ), String.valueOf( cd.nLand ) };
			return array;
		}// if
		else
		{
			NasimData nd = nasim.getNasimData( i );
			ChannelData cd = (ChannelData) getChannelData().elementAt( i );
			String array[] = { String.valueOf( cd.discharge ), String.valueOf( cd.kRiver ),
					String.valueOf( cd.nRiver ), String.valueOf( cd.kLand ), String.valueOf( cd.nLand ),
					String.valueOf( nd.nasimWidth ), String.valueOf( nd.nasimHeight ) };
			return array;
		}// else
	}// dataOfChannelToArray
	/** This method writes the Channel data to the channel file.
	 * @param bw BufferedWriter of the channel file.
	 * @throws Throws IOException if the buffered writer is corrupted.*/
	public	void writeChannelToFile( BufferedWriter bw ) throws IOException
	{
		if( getNasim() == null )
		{
			bw.write( Printf.format("%14.6f%12.6f", headOfChannelToArray() ) );
			bw.newLine();
			for( int i = 0 ; i < getChannelData().size() ; i ++ )
			{
				bw.write( Printf.format("%14.6f%14.6f%14.6f%14.6f%14.6f", dataOfChannelToArray( i ) ) );
				bw.newLine();
			}// for i
		}// if == null
		else
		{
			bw.write( Printf.format("%14.6f%12.6f%12.6f%12.6f%12.6f", headOfChannelToArray() ) );
			bw.newLine();
			for( int i = 0 ; i < getChannelData().size() ; i ++ )
			{
				bw.write( Printf.format("%14.6f%14.6f%14.6f%14.6f%14.6f%14.6f%14.6f", dataOfChannelToArray( i ) ) );
				bw.newLine();
			}// for i
			
		}// else
	}// writeChannelToFile
	public void writeChannelToXML( BufferedWriter bw ) throws IOException
	{
		bw.write( " m_qBoardfull=\"" + getQBoardFull() + "\" m_distribFact=\""
					 + getDistFact() + "\" /> <v key=\"m_channelData\">" );
		Vector data = getChannelData();
		Iterator it = data.iterator();
		while( it.hasNext() )
		{
			ChannelData ch = (ChannelData) it.next();
			bw.write( " <v_row v_q=\"" + ch.discharge + "\" v_kRiver=\"" + ch.kRiver
						 + "\" v_nRiver=\"" + ch.nRiver + "\" v_kLand=\"" + ch.kLand
						 + "\" v_nLand=\"" + ch.nLand + "\" />" );
		}
		bw.write( " </v>" );
	}// writeChannelToXML
} // class Gerinne

