package org.kalypso.util.transformation;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.eclipse.core.runtime.LogStatus;
import org.kalypso.ui.KalypsoGisPlugin;

/**
 * TransformationResult
 * 
 * @author schlienger
 */
public class TransformationResult
{
  public final static TransformationResult OK_RESULT = new TransformationResult( "", null );
  
  private final String m_summary;
  private final IFile m_logFile;

  public TransformationResult( final String summary, final IFile logFile )
  {
    m_summary = summary;
    m_logFile = logFile;
  }

  /**
   * @return true if there are some messages for the user
   */
  public boolean hasMessages()
  {
    return m_summary.length() > 0;
  }
  
  /**
   * @return Returns the logFile.
   */
  public IFile getLogFile( )
  {
    return m_logFile;
  }
  
  /**
   * @return an IStatus representation of this result.
   */
  public IStatus toStatus()
  {
    if( !hasMessages() )
      return Status.OK_STATUS;
    
    final String msg = m_summary + "\n" + "Siehe Logdatei: " + m_logFile.getFullPath().toOSString();
    
    return new LogStatus( IStatus.WARNING, KalypsoGisPlugin.getId(), 0, msg, null, m_logFile );
  }
}
