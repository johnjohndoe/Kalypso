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
import org.kalypso.java.lang.CatchThread;
import org.kalypso.ui.KalypsoGisPlugin;

/**
 * Helper-Klasse für {@link org.eclipse.core.resources.IFile}
 * 
 * @author belger
 */
public abstract class SetContentThread extends CatchThread
{
  private final PipedInputStream m_pis;
  private final PipedOutputStream m_pos;

  public SetContentThread( final IFile file, boolean force, boolean keepHistory, final IProgressMonitor monitor ) throws CoreException
  {
    m_pos = new PipedOutputStream( );
    try
    {
      m_pis = new PipedInputStream( m_pos );
      file.setContents( m_pis, force, keepHistory, monitor );
    
      start();
    }
    catch( final IOException e )
    {
      e.printStackTrace();
      throw new CoreException( new Status( IStatus.ERROR, KalypsoGisPlugin.getId(), 0, "Fehler beim Erzeugen der Kontrolldatei: " + e.getLocalizedMessage(), e  ) );
    }
    
  }

  protected OutputStream getOutputStream()
  {
    return m_pos;
  }
  
  protected abstract void writeStream() throws Throwable;

  protected void runIntern() throws Throwable
  {
    writeStream();
    m_pos.close();
  }
}
