package org.kalypso.eclipse.util;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStreamWriter;
import java.io.PipedInputStream;
import java.io.PipedOutputStream;
import java.io.Writer;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.java.lang.CatchRunnable;
import org.kalypso.java.lang.CatchThread;
import org.kalypso.ui.KalypsoGisPlugin;

/**
 * Helper-Klasse für {@link org.eclipse.core.resources.IFile}
 * 
 * TODO check resource closing (use IOUtils.closeQuietly in finally blocks)
 * 
 * @author belger
 */
public abstract class SetContentThread extends CatchThread
{
  private final PipedOutputStream m_pos = new PipedOutputStream();

  private final PipedInputStream m_pis;

  private CatchRunnable m_catchRunnable;

  private final IFile m_file;

  private String m_charset;

  private Thread m_thread;

  public SetContentThread( final IFile file, final boolean create,
      final boolean force, final boolean keepHistory,
      final IProgressMonitor monitor ) throws CoreException
  {
    this( file, create, force, keepHistory, monitor, null );
  }

  public SetContentThread( final IFile file, final boolean create,
      final boolean force, final boolean keepHistory,
      final IProgressMonitor monitor, final String charset )
      throws CoreException
  {
    m_file = file;
    try
    {
      m_pis = new PipedInputStream( m_pos );

      if( charset == null )
      {
        if( !create )
          m_charset = m_file.getCharset();
        else
          m_charset = m_file.getParent().getDefaultCharset();
      }
      else
        m_charset = charset;

      final InputStream is = m_pis;
      m_catchRunnable = new CatchRunnable()
      {
        protected void runIntern( ) throws Throwable
        {
          if( create )
            file.create( is, force, monitor );
          else
            file.setContents( is, force, keepHistory, monitor );
          is.close();

          file.setCharset( charset, monitor );
        }
      };
      m_thread = new Thread( m_catchRunnable );
      m_thread.start();
    }
    catch( final IOException e )
    {
      e.printStackTrace();
      throw new CoreException( new Status( IStatus.ERROR, KalypsoGisPlugin
          .getId(), 0, "Fehler beim Erzeugen der Kontrolldatei: "
          + e.getLocalizedMessage(), e ) );
    }
  }

  protected abstract void write( final Writer writer ) throws Throwable;

  protected String getCharset()
  {
    return m_charset;
  }
  
  /** 
   * @return Exceptions, welche beim Schreiben des Files entstehen
   */
  public CoreException getFileException( )
  {
    // zum Glück wissen wir, dass es nur eine CoreException sein kann
    return (CoreException) m_catchRunnable.getThrown();
  }

  protected void runIntern( ) throws Throwable
  {
    Writer outputStreamWriter = null;
    try
    {
      outputStreamWriter = new OutputStreamWriter( m_pos, m_charset );
      write( outputStreamWriter );
    }
    finally
    {
      if( outputStreamWriter != null )
        outputStreamWriter.close();

      // TODO: ugly! better to switch contents of this method and 'runIntern' of
      // inner thread
      // to be shure, that file is written when this thread exits
      if( m_thread.isAlive() ) // this if statement inserted by Marc to test if hanging-problem can be solved this way
        m_thread.join();
    }
  }

}