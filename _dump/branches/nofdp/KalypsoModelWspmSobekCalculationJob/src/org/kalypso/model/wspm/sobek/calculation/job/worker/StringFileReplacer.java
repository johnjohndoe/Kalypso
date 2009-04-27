/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
 *  21073 Hamburg, Germany
 *  http://www.tuhh.de/wb
 * 
 *  and
 *  
 *  Bjoernsen Consulting Engineers (BCE)
 *  Maria Trost 3
 *  56070 Koblenz, Germany
 *  http://www.bjoernsen.de
 * 
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 * 
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 * 
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 * 
 *  Contact:
 * 
 *  E-Mail:
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *   
 *  ---------------------------------------------------------------------------*/
package org.kalypso.model.wspm.sobek.calculation.job.worker;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.util.Map;
import java.util.Set;
import java.util.Map.Entry;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;

/**
 * @author kuch
 */
public class StringFileReplacer implements ICoreRunnableWithProgress
{
  public static final String LINE_SEPARATOR = System.getProperty( "line.separator", "\n" ); //$NON-NLS-1$ //$NON-NLS-2$

  private final File m_txtFile;

  private final Map<String, String> m_replacements;

  public StringFileReplacer( final File txtFile, final Map<String, String> replacements )
  {
    m_txtFile = txtFile;
    m_replacements = replacements;
  }

  /**
   * @throws CoreException 
   * @see org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress#execute(org.eclipse.core.runtime.IProgressMonitor)
   */
  public IStatus execute( final IProgressMonitor monitor ) throws CoreException 
  {
    try
    {
      final BufferedReader in = new BufferedReader( new InputStreamReader( new FileInputStream( m_txtFile ), "UTF8" ) ); //$NON-NLS-1$
      if( in == null )
        return StatusUtilities.createErrorStatus( "Open InputStream failed" ); //$NON-NLS-1$

      final StringBuffer content = new StringBuffer( "" ); //$NON-NLS-1$
      String line;

      while( (line = in.readLine()) != null )
        content.append( line + LINE_SEPARATOR );

      in.close();

      String text = content.toString();
      final Set<Entry<String, String>> entrySet = m_replacements.entrySet();

      for( final Entry<String, String> entry : entrySet )
        text = text.replaceAll( entry.getKey(), entry.getValue() );

      final BufferedWriter writer = new BufferedWriter( new OutputStreamWriter( new FileOutputStream( m_txtFile ), "UTF8" ) ); //$NON-NLS-1$
      writer.write( text );
      writer.close();
    }
    catch( final Exception e )
    {
     throw new CoreException(StatusUtilities.statusFromThrowable( e ));
    }

    return Status.OK_STATUS;
  }

}
