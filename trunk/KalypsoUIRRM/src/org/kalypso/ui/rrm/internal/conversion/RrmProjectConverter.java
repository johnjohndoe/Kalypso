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
package org.kalypso.ui.rrm.internal.conversion;

import java.io.File;
import java.lang.reflect.InvocationTargetException;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.swt.widgets.Shell;
import org.kalypso.module.IKalypsoModule;
import org.kalypso.module.ModuleExtensions;
import org.kalypso.module.conversion.IProjectConverter;
import org.kalypso.ui.rrm.KalypsoModuleRRM;
import org.kalypso.ui.rrm.internal.KalypsoUIRRMPlugin;
import org.kalypso.ui.rrm.internal.conversion.to12_02.RrmProjectConverter11_06to12_02;
import org.kalypso.ui.rrm.internal.i18n.Messages;
import org.osgi.framework.Version;

/**
 * {@link org.kalypso.module.conversion.IProjectConverter} implementation for KalypsoHydrology projects.
 * 
 * @author Gernot Belger
 */
public class RrmProjectConverter implements IProjectConverter
{
  private final static Version V_10_10 = new Version( 10, 10, 0 );

  // private final static Version V_11_06 = new Version( 11, 6, 0 );

  private final File m_sourceDir;

  private final File m_targetDir;

  private final Version m_sourceVersion;

  private IProjectConverter m_converter;

  public RrmProjectConverter( final Version sourceVersion, final File sourceDir, final File targetDir )
  {
    m_sourceVersion = sourceVersion;
    m_sourceDir = sourceDir;
    m_targetDir = targetDir;
  }

  @Override
  public String getLabel( )
  {
    return String.format( Messages.getString( "RrmProjectConverter.0" ), m_sourceDir.getName() ); //$NON-NLS-1$
  }

  @Override
  public IStatus preConversion( final Shell shell )
  {
    final IKalypsoModule rrmModule = ModuleExtensions.getKalypsoModule( KalypsoModuleRRM.ID );
    final Version rrmVersion = rrmModule.getVersion();
    m_converter = createConverter( rrmVersion );
    if( m_converter == null )
    {
      final String msg = String.format( Messages.getString( "RrmProjectConverter_0" ), m_sourceVersion, rrmVersion ); //$NON-NLS-1$
      return new Status( IStatus.ERROR, KalypsoUIRRMPlugin.getID(), msg );
    }

    return m_converter.preConversion( shell );
  }

  @Override
  public IStatus execute( final IProgressMonitor monitor ) throws CoreException, InvocationTargetException, InterruptedException
  {
    return m_converter.execute( monitor );
  }

  private IProjectConverter createConverter( final Version targetVersion )
  {
    /* Everything above 10.10 */
    if( m_sourceVersion != null && m_sourceVersion.compareTo( V_10_10 ) >= 0 )
      return new RrmProjectConverter11_06to12_02( m_sourceDir, m_targetDir, targetVersion );

    return null;

    // TODO: implement x -> 10.10 -> 12.02

    /* everything that is below 10.10 */
    // if( m_sourceVersion == null || m_sourceVersion.compareTo( V_10_10 ) < 0 )

    // For the moment, just everything, the converter should work well for all projects.
    // If we ever need another converter from some version on, we should start to check the version number.

    // return new RrmProjectConverterXto10_10( m_sourceDir, m_targetDir, targetVersion );

    // TODO: we should implement a converter for 11.xxx;
    // First ideas what the converter needs to do (in comparison to the 10.10 converter):
    // - keep geologie.gml data (gets lost with the old converter)

// return null;
  }
}
