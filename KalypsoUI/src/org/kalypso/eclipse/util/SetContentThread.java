package org.kalypso.eclipse.util;

import java.io.IOException;
import java.io.OutputStream;
import java.io.PipedInputStream;
import java.io.PipedOutputStream;

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
 * @author belger
 */
public abstract class SetContentThread extends CatchThread
{
  private final PipedOutputStream m_pos;

  private CatchRunnable m_catchRunnable;

  public SetContentThread( final IFile file, final boolean create, final boolean force,
      final boolean keepHistory, final IProgressMonitor monitor ) throws CoreException
  {
    m_pos = new PipedOutputStream();
    try
    {
      final PipedInputStream m_pis = new PipedInputStream( m_pos );

      m_catchRunnable = new CatchRunnable()
            {
              protected void runIntern() throws Throwable
              {
                if( create )
                  file.create( m_pis, force, monitor );
                else
                  file.setContents( m_pis, force, keepHistory, monitor );
              }
            };
      new Thread( m_catchRunnable ).start();
    }
    catch( final IOException e )
    {
      e.printStackTrace();
      throw new CoreException( new Status( IStatus.ERROR, KalypsoGisPlugin.getId(), 0,
          "Fehler beim Erzeugen der Kontrolldatei: " + e.getLocalizedMessage(), e ) );
    }
  }

  protected OutputStream getOutputStream()
  {
    return m_pos;
  }

  protected abstract void writeStream() throws Throwable;

  /** Exceptions, welche beim Schreiben des Files entstehen */
  public CoreException getFileException()
  {
    // zum Glück wissen wir, dass es nur eine CoreException sein kann
    return (CoreException)m_catchRunnable.getThrown();
  }

  protected void runIntern() throws Throwable
  {
    try
    {
      writeStream();
    }
    finally
    {
      m_pos.close();
    }
  }

}