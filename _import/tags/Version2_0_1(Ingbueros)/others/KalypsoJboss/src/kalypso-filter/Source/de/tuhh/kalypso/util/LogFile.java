package de.tuhh.kalypso.util;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;

import de.tuhh.kalypso.util.ErrorHandler.KalypsoFilterException;


/** The class LogFile.java is the file where the objects write exceptions and other
 * messages to. It is a file where the user can see if a run successfully completed,
 * or where there were errors in the process. This logFile can only be crated once,
 * it is initialized at the beginning of each run. To make sure this file existst only
 * once the Singelton Pattern has been implemented.
 *
 * @author <A HREF="mailto:c.kuepferle@tuhh.de">Christoph Küpferle</A>
 * @version LogFile.java,v 1.0 2002/07/01
 */


public class LogFile
{
	/** The only instance of the LogFile
	 * @param theLogFile The only log file for this run.*/
	private static LogFile theLogFile=null;
	
	/** The log stream.
	 * @param m_bw Buffered writer of the log file.*/
	private BufferedWriter m_bw;
	
	private LogFile() {}
	/** This method creates the log file if it is not existing or returns the only
	 * log file existing in this program.
	 * @return theLogFile */
	private static LogFile getInstance()
	{
		if( theLogFile == null )
			theLogFile = new LogFile();
		
		return theLogFile;
	} // getInstance
	/** This method initializes the log file ( just once at the beginning of the
	 * main ).
	 * @param filename Filename and path where the log is saved to.*/
	public static void init( String filename, String suffix ) throws Exception
	{
		try
		{
		    getInstance().m_bw = new BufferedWriter( new FileWriter( File.createTempFile( filename, suffix ) ) );
		}
		catch( Exception e )
		{
			System.out.println( "Logfile has not been opened!" );
			throw new KalypsoFilterException("Logfile has not been opened!");
		}
	}
	/** This method writes the string logString to the log file.
	 * @param logString The String that the user wants to be written to the log file.*/
	public static void log( String logString )
	{
		BufferedWriter bw  = getInstance().m_bw;
		if( bw != null )
		{
			try
			{
				bw.write( logString );
				bw.newLine();
			}
			catch( Exception e ) {}
		} // if bw
	}
	/** This method closes the only log file in the program.*/
	public static void close()
	{
		BufferedWriter bw = getInstance().m_bw;
		if( bw != null )
		{
			try
			{
				bw.close();
			}
			catch( Exception e ) {};
		} // if m_bw
	} // close

} // class LogFile

