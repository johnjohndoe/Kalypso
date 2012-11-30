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
package org.kalypso.model.wspm.tuhh.ui.export.sobek;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.contribs.eclipse.core.runtime.IStatusCollector;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollector;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.model.wspm.core.KalypsoModelWspmCorePlugin;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.tuhh.ui.KalypsoModelWspmTuhhUIPlugin;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;

/**
 * @author Gernot Belger
 */
public abstract class AbstractSobekExportOperation implements ISobekProfileExportOperation
{
  private final IStatusCollector m_stati = new StatusCollector( KalypsoModelWspmTuhhUIPlugin.getID() );

  private final SobekExportInfo m_info;

  private final String m_operationLabel;

  public AbstractSobekExportOperation( final SobekExportInfo info, final String operationLabel )
  {
    m_info = info;
    m_operationLabel = operationLabel;
  }

  protected SobekExportInfo getInfo( )
  {
    return m_info;
  }

  protected IStatusCollector getLog( )
  {
    return m_stati;
  }

  @Override
  public IStatus execute( final IProgressMonitor monitor ) throws CoreException
  {
    try
    {
      final IProfileFeature[] profiles = getProfiles();

      monitor.beginTask( Messages.getString( "AbstractSobekProfileExportOperation_0" ), profiles.length ); //$NON-NLS-1$

      initTargetFile();

      for( final IProfileFeature profil : profiles )
      {
        monitor.subTask( String.format( "%s (%s)", profil.getName(), profil.getBigStation() ) ); //$NON-NLS-1$

        writeProfile( profil );
        ProgressUtilities.worked( monitor, 1 );
      }

      close();

      return m_stati.asMultiStatusOrOK( m_operationLabel );
    }
    catch( final Exception e )
    {
      final String message = String.format( Messages.getString("AbstractSobekExportOperation.0"), m_operationLabel ); //$NON-NLS-1$
      final IStatus status = new Status( IStatus.ERROR, KalypsoModelWspmCorePlugin.getID(), message, e );
      throw new CoreException( status );
    }
    finally
    {
      closeQuiet();

      monitor.done();
    }
  }

  protected IProfileFeature[] getProfiles( )
  {
    return m_info.getProfiles();
  }

  protected abstract void initTargetFile( ) throws Exception;

  protected abstract void writeProfile( IProfileFeature profil ) throws Exception;

  protected abstract void close( ) throws Exception;

  protected abstract void closeQuiet( );

}
