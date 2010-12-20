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
package org.kalypso.kalypsomodel1d2d.ui.wizard.profileImport;

import java.io.File;
import java.text.DateFormat;
import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.wizard.IWizard;
import org.eclipse.jface.wizard.Wizard;
import org.kalypso.contribs.eclipse.jface.operation.RunnableContextHelper;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRiverProfileNetwork;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRiverProfileNetworkCollection;
import org.kalypso.model.wspm.ui.profil.wizard.importProfile.ImportProfilePage;
import org.kalypsodeegree.model.feature.Feature;

/**
 * A wizard to import profile data (right now just as trippel) into a 1D2D Terrain Model.
 * 
 * @author Thomas Jung
 */
public class ImportTrippleWizard extends Wizard implements IWizard
{
  static final DateFormat DF = DateFormat.getDateTimeInstance( DateFormat.MEDIUM, DateFormat.SHORT );

  private final List<Feature> m_terrainModelAdds = new ArrayList<Feature>();

  private final IRiverProfileNetworkCollection m_networkModel;

  protected ImportProfilePage m_profilePage;

  private IRiverProfileNetwork m_network;

  public ImportTrippleWizard( final IRiverProfileNetworkCollection networkModel )
  {
    m_networkModel = networkModel;
    setWindowTitle( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.wizard.profileImport.ImportTrippelWizard.0" ) ); //$NON-NLS-1$
    setNeedsProgressMonitor( true );

    /* Choose profile data */
    m_profilePage = new ImportProfilePage( "chooseProfileData", Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.wizard.profileImport.ImportTrippelWizard.2" ), null ); //$NON-NLS-1$ //$NON-NLS-2$
    m_profilePage.setDescription( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.wizard.profileImport.ImportTrippelWizard.3" ) ); //$NON-NLS-1$

    addPage( m_profilePage );
  }

  /**
   * @see org.eclipse.jface.wizard.Wizard#performFinish()
   */
  @Override
  public boolean performFinish( )
  {
    /* Collect page data */
    final IRiverProfileNetworkCollection profNetworkColl = m_networkModel;
    final List<Feature> terrainModelAdds = m_terrainModelAdds;

    /* get file name from wizard */
    final File trippelFile = m_profilePage.getFile();
    final String separator = m_profilePage.getSeparator();
    final String crs = m_profilePage.getCoordinateSystem();

    /* Do import */
    final TrippleImportOperation op = new TrippleImportOperation( trippelFile, separator, crs, profNetworkColl, terrainModelAdds );

    final IStatus status = RunnableContextHelper.execute( getContainer(), true, false, op );
    if( !status.isOK() )
      KalypsoModel1D2DPlugin.getDefault().getLog().log( status );

    m_network = op.getNetwork();

    ErrorDialog.openError( getShell(), getWindowTitle(), Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.wizard.profileImport.ImportTrippelWizard.17" ), status ); //$NON-NLS-1$

    return !status.matches( IStatus.ERROR );
  }

  public Feature[] getTerrainModelAdds( )
  {
    return m_terrainModelAdds.toArray( new Feature[m_terrainModelAdds.size()] );
  }

  public IRiverProfileNetwork getAddedRiverNetwork( )
  {
    return m_network;

  }

}
