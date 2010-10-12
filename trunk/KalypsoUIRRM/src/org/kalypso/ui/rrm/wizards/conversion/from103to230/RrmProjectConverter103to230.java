/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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
package org.kalypso.ui.rrm.wizards.conversion.from103to230;

import java.io.File;
import java.io.IOException;
import java.lang.reflect.InvocationTargetException;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.kalypso.ui.rrm.wizards.conversion.IProject2ProjectConverter;


/**
 * @author Gernot Belger
 */
public class RrmProjectConverter103to230 implements IProject2ProjectConverter
{
  private final File m_sourceDir;

  private final File m_targetDir;

  public RrmProjectConverter103to230( final File sourceDir, final File targetDir )
  {
    m_sourceDir = sourceDir;
    m_targetDir = targetDir;
  }

  /**
   * @see org.kalypso.ui.rrm.wizards.conversion.IProjectConverter#getLabel()
   */
  @Override
  public String getLabel( )
  {
    return "1.0.3 - 2.2.0";
  }

  /**
   * @see org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress#execute(org.eclipse.core.runtime.IProgressMonitor)
   */
  @Override
  public IStatus execute( final IProgressMonitor monitor ) throws CoreException, InvocationTargetException
  {
    monitor.beginTask( String.format( "Projekt '%s'", m_sourceDir.getName() ), 100 );

    try
    {
      final BasicModelConverter basicModelConverter = new BasicModelConverter( m_sourceDir, m_targetDir );
      monitor.subTask( "konvertiere Basisdaten..." );
      basicModelConverter.execute( new SubProgressMonitor( monitor, 33 ) );

      monitor.subTask( "konvertiere Rechenvarianten..." );
      final CalcCasesConverter casesConverter = new CalcCasesConverter( m_sourceDir, m_targetDir );
      casesConverter.execute( new SubProgressMonitor( monitor, 67 ) );

      return Status.OK_STATUS;
    }
    catch( final IOException e )
    {
      throw new InvocationTargetException( e );
    }
    finally
    {
      monitor.done();
    }
  }


}
