package org.kalypso.eclipse.util;

import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.PipedInputStream;
import java.io.PipedOutputStream;
import java.io.Writer;

import org.apache.commons.io.IOUtils;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.kalypso.java.lang.CatchRunnable;
import org.kalypso.ui.KalypsoGisPlugin;

/**
 * Helper-Klasse für {@link org.eclipse.core.resources.IFile}. This is an
 * abstract class, you must implement the <code>write</code> method.
 * 
 * @author belger
 */
public abstract class SetContentHelper
{
  private String m_newCharset;

  /**
   * @param file
   * @param force
   * @param keepHistory
   * @param monitor
   * @throws CoreException
   */
  public void setFileContents( final IFile file, 
      final boolean force, final boolean keepHistory,
      final IProgressMonitor monitor ) throws CoreException 
  {
    setFileContents( file, force, keepHistory, monitor, null );
  }

  /**
   * @param file
   * @param force
   * @param keepHistory
   * @param monitor
   * @param charset
   * @throws CoreException
   */
  public void setFileContents( final IFile file, final boolean force,
      final boolean keepHistory, final IProgressMonitor monitor, final String charset )
      throws CoreException
  {
    if( charset == null )
    {
      if( file.exists() )
        m_newCharset = file.getCharset();
      else
        m_newCharset = file.getParent().getDefaultCharset();
    }
    else
      m_newCharset = charset;

    PipedInputStream m_pis = null;
    try
    {
      monitor.beginTask( "Schreibe Datei", 2000 );
      
      final PipedOutputStream m_pos = new PipedOutputStream();
      m_pis = new PipedInputStream( m_pos );

      final CatchRunnable innerRunnable = new CatchRunnable()
      {
        /**
         * @see org.kalypso.java.lang.CatchRunnable#runIntern()
         */
        protected void runIntern( ) throws Throwable
        {
          Writer outputStreamWriter = null;
          try
          {
            outputStreamWriter = new OutputStreamWriter( m_pos, getCharset() );
            write( outputStreamWriter );
          }
          finally
          {
            IOUtils.closeQuietly( outputStreamWriter );
          }
        }
      };
      final Thread innerThread = new Thread( innerRunnable );
      innerThread.start();

      // set file contents
      if( file.exists() )
        file.setContents( m_pis, force, keepHistory, new SubProgressMonitor( monitor, 1000 ) );
      else
        file.create( m_pis, force, new SubProgressMonitor( monitor, 1000 ) );

      final Throwable thrown = innerRunnable.getThrown();
      if( thrown != null )
        throw new CoreException( KalypsoGisPlugin
            .createErrorStatus( "", thrown ) );
    }
    catch( IOException e )
    {
      e.printStackTrace();
      throw new CoreException( KalypsoGisPlugin.createErrorStatus( "", e ) );
    }
    finally
    {
      IOUtils.closeQuietly( m_pis );
    }

    file.setCharset( m_newCharset, new SubProgressMonitor( monitor, 1000 ) );

    // enclose in finally?
    monitor.done();
  }

  /**
   * Override this method to provide your business. The writer is closed once
   * write returns.
   * 
   * @param writer
   * @throws Throwable
   */
  protected abstract void write( final Writer writer ) throws Throwable;

  /**
   * @return the charset used for encoding the file
   */
  protected String getCharset( )
  {
    return m_newCharset;
  }
}