package org.kalypso.java.io;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.Reader;
import java.io.Writer;

import org.apache.commons.io.CopyUtils;
import org.apache.commons.io.IOUtils;
import org.apache.commons.io.output.NullOutputStream;

/**
 * Wraps a process and allows to wait until execution is terminated or has been
 * cancelled.
 * <p>
 * Clients may inherit from this class and perform specific business in
 * processCanceled() and/or processTerminated().
 * 
 * @author schlienger
 */
public abstract class ProcessWraper
{
  private final Process m_proc;

  private Writer m_logWriter;

  private boolean m_canceled = false;

  private int m_ms;

  /**
   * Construct a wraper over the given process with the given logWriter
   * 
   * @param proc
   * @param logWriter
   *          [optional, can be null]
   */
  public ProcessWraper( Process proc, Writer logWriter )
  {
    m_proc = proc;
    m_logWriter = logWriter;
  }

  /**
   * Sets the sleep time in milliseconds for a loop cycle
   * 
   * @param ms
   */
  public synchronized void setCycleSleepTime( final int ms )
  {
    m_ms = ms;
  }

  /**
   * Force the underlying process to stop
   */
  public synchronized void cancel( )
  {
    m_canceled = true;
  }

  /**
   * Starts to wait for the given process
   * 
   * @throws IOException
   * @throws InterruptedException
   */
  public synchronized void waitForProcess( ) throws IOException,
      InterruptedException
  {
    final OutputStream nul_dev = new NullOutputStream();
    final Writer log;
    if( m_logWriter == null )
      log = new BufferedWriter( new OutputStreamWriter( new NullOutputStream() ) );
    else
      log = new BufferedWriter( m_logWriter );

    try
    {
      final Reader inStream = new BufferedReader( new InputStreamReader( m_proc
          .getInputStream() ) );
      final Reader errStream = new BufferedReader( new InputStreamReader( m_proc
          .getErrorStream() ) );

      while( true )
      {
        CopyUtils.copy( inStream, log );
        CopyUtils.copy( errStream, nul_dev );

        try
        {
          int rc = m_proc.exitValue();

          processTerminated( rc );

          return;
        }
        catch( final IllegalThreadStateException e )
        {
          // noch nicht fertig
        }

        if( m_canceled )
        {
          m_proc.destroy();

          processCanceled();

          return;
        }

        Thread.sleep( m_ms );
      }
    }
    finally
    {
      IOUtils.closeQuietly( nul_dev );
      if( m_logWriter == null )
        IOUtils.closeQuietly( log );
    }
  }

  /**
   * Called after the process has been told to be cancelled
   */
  public abstract void processCanceled( );

  /**
   * Called after the process has stopped execution
   * 
   * @param returnCode
   *          the return code of the process
   */
  public abstract void processTerminated( int returnCode );
}