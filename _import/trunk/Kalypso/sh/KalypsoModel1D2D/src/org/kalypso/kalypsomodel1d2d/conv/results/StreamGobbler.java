package org.kalypso.kalypsomodel1d2d.conv.results;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;

import org.kalypso.contribs.eclipse.core.runtime.Debug;

/**
 * TODO: move this to a contribution plug-in.
 * 
 * This class can handle one stream from an external process.
 * 
 * @author Holger Albert
 */
class StreamGobbler extends Thread
{
  private final InputStream m_is;

  private final String m_type;

  private final Debug m_debug;

  /**
   * The constructor.
   * 
   * @param is
   *            The input stream to handle.
   * @param type
   *            The type of the stream.
   * @param debug
   *            If the stream should be written to some console other then System.out, use this parameter.
   */
  public StreamGobbler( final InputStream is, final String type, final Debug debug )
  {
    m_is = is;
    m_type = type;
    m_debug = debug;
  }

  /**
   * @see java.lang.Thread#run()
   */
  @Override
  public void run( )
  {
    try
    {
      final InputStreamReader isr = new InputStreamReader( m_is );
      final BufferedReader br = new BufferedReader( isr );
      String line = null;
      while( (line = br.readLine()) != null )
      {
        if( m_debug == null )
          System.out.println( m_type + ": " + line );
        else
          m_debug.printf( m_type + ": " + line + "\n" );
      }
    }
    catch( final IOException ioe )
    {
      ioe.printStackTrace();
    }
  }
}