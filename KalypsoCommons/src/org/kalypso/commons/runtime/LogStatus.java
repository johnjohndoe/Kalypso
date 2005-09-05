/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and
 
 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de
 
 ---------------------------------------------------------------------------------------------------*/
package org.kalypso.commons.runtime;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;

import org.apache.commons.io.IOUtils;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;

/**
 * LogStatus, a status that gets its children from a log file. Can be used within an ErrorDialog, when the user clicks
 * on 'Details', the contents of the log file are displayed.
 * 
 * @author schlienger
 */
public class LogStatus extends Status
{
  private final File m_logFile;
  private final String m_charsetName;

  private IStatus[] m_children = null;

  public LogStatus( final int severity, final String pluginId, final int code, final String message,
      final Throwable exception, final File logFile, final String charsetName )
  {
    super( severity, pluginId, code, message, exception );
    
    m_logFile = logFile;
    m_charsetName = charsetName;
  }

  /**
   * @see org.eclipse.core.runtime.Status#isMultiStatus()
   */
  public boolean isMultiStatus()
  {
    return true;
  }

  /**
   * @see org.eclipse.core.runtime.Status#getChildren()
   */
  public IStatus[] getChildren()
  {
    if( m_children == null )
    {
      final ArrayList lines = new ArrayList();

      BufferedReader reader = null;
      try
      {
        reader = new BufferedReader( new InputStreamReader( new FileInputStream( m_logFile ), m_charsetName ) );
        String line = reader.readLine();
        while( line != null )
        {
          lines.add( line );
          line = reader.readLine();
        }
      }
      catch( final IOException e )
      {
        e.printStackTrace();
      }
      finally
      {
        IOUtils.closeQuietly( reader );
      }

      // TODO prüfen ob der encoding der benutzt wird im ErrorDialog
      // mit der encoding der strings passt.
      m_children = new IStatus[lines.size()];
      for( int i = 0; i < m_children.length; i++ )
      {
        final String str = lines.get( i ).toString();
        m_children[i] = new Status( getSeverity(), getPlugin(), getCode(), str, null );
      }

      lines.clear();
    }

    return m_children;
  }
}