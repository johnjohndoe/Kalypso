package org.kalypso.eclipse.core.runtime;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.util.ArrayList;

import org.apache.commons.io.IOUtils;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;

/**
 * LogStatus, a status that gets its children from a log file. Can be used
 * within an ErrorDialog, when the user clicks on 'Details', the contents of the
 * log file are displayed.
 * 
 * @author schlienger
 */
public class LogStatus extends Status
{
  private final IFile m_logFile;

  private IStatus[] m_children = null;

  /**
   * @param severity
   * @param pluginId
   * @param code
   * @param message
   * @param exception
   * @param logFile
   */
  public LogStatus( final int severity, final String pluginId, final int code,
      final String message, final Throwable exception, final IFile logFile )
  {
    super( severity, pluginId, code, message, exception );
    m_logFile = logFile;
  }

  /**
   * @see org.eclipse.core.runtime.Status#isMultiStatus()
   */
  public boolean isMultiStatus( )
  {
    return true;
  }
  
  /**
   * @see org.eclipse.core.runtime.Status#getChildren()
   */
  public IStatus[] getChildren( )
  {
    if( m_children == null )
    {
      final ArrayList lines = new ArrayList();

      BufferedReader reader = null;
      try
      {
        reader = new BufferedReader( new InputStreamReader( m_logFile
            .getContents() ) );
        String line = reader.readLine();
        while( line != null )
        {
          lines.add( line );
          line = reader.readLine();
        }
      }
      catch( Exception e )
      {
        e.printStackTrace();
      }
      finally
      {
        IOUtils.closeQuietly( reader );
      }

      m_children = new IStatus[lines.size()];
      for( int i = 0; i < m_children.length; i++ )
        m_children[i] = new Status( getSeverity(), getPlugin(),
          getCode(), (String)lines.get(i), null );
      
      lines.clear();
    }

    return m_children;
  }
}