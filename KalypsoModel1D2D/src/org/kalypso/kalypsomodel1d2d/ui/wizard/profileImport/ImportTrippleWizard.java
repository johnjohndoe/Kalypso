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
import java.util.Date;
import java.util.List;

import javax.xml.namespace.QName;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.wizard.IWizard;
import org.eclipse.jface.wizard.Wizard;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.eclipse.jface.operation.RunnableContextHelper;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRiverProfileNetwork;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRiverProfileNetworkCollection;
import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.core.gml.ProfileFeatureFactory;
import org.kalypso.model.wspm.core.imports.ImportTrippleHelper;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.ui.wizard.ImportProfilePage;
import org.kalypsodeegree.KalypsoDeegreePlugin;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureVisitor;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.event.FeatureStructureChangeModellEvent;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;
import org.kalypsodeegree_impl.model.feature.visitors.TransformVisitor;

/**
 * A wizard to import profile data (right now just as trippel) into a 1D2D Terrain Model.
 * 
 * @author Thomas Jung
 */
public class ImportTrippleWizard extends Wizard implements IWizard
{
  private static final DateFormat DF = DateFormat.getDateTimeInstance( DateFormat.MEDIUM, DateFormat.SHORT );

  private final List<Feature> m_terrainModelAdds = new ArrayList<Feature>();

  private final IRiverProfileNetworkCollection m_networkModel;

  protected ImportProfilePage m_ProfilePage;

  private IRiverProfileNetwork m_network;

  public ImportTrippleWizard( final IRiverProfileNetworkCollection networkModel )
  {

    m_networkModel = networkModel;
    setWindowTitle( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.wizard.profileImport.ImportTrippelWizard.0" ) ); //$NON-NLS-1$
    setNeedsProgressMonitor( true );
  }

  /**
   * @see org.eclipse.jface.wizard.Wizard#addPages()
   */
  @Override
  public void addPages( )
  {
    /* Choose profile data */
    m_ProfilePage = new ImportProfilePage( "chooseProfileData", Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.wizard.profileImport.ImportTrippelWizard.2" ), null ); //$NON-NLS-1$ //$NON-NLS-2$
    m_ProfilePage.setDescription( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.wizard.profileImport.ImportTrippelWizard.3" ) ); //$NON-NLS-1$

    addPage( m_ProfilePage );
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

    /* Do import */
    final ICoreRunnableWithProgress op = new ICoreRunnableWithProgress()
    {
      public IStatus execute( final IProgressMonitor monitor )
      {
        monitor.beginTask( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.wizard.profileImport.ImportTrippelWizard.13" ), 2 ); //$NON-NLS-1$

        try
        {
          /* Import Trippel Data */
          monitor.subTask( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.wizard.profileImport.ImportTrippelWizard.14" ) ); //$NON-NLS-1$

          /* get file name from wizard */
          final File trippelFile = m_ProfilePage.getFile();
          final String separator = m_ProfilePage.getSeparator();

          List<IProfil> profiles = ImportTrippleHelper.importTrippelData( trippelFile, separator, IWspmTuhhConstants.PROFIL_TYPE_PASCHE );

          monitor.worked( 1 );

          /* Convert Trippel Data */
          monitor.subTask( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.wizard.profileImport.ImportTrippelWizard.16" ) ); //$NON-NLS-1$

          final IStatus status = doImportNetwork( profNetworkColl, terrainModelAdds, profiles );

          monitor.worked( 1 );

          return status;
        }
        catch( final Exception e )
        {
          return StatusUtilities.statusFromThrowable( e, Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.wizard.profileImport.ImportTrippelWizard.15" ) ); //$NON-NLS-1$
        }

        finally
        {
          monitor.done();
        }
      }
    };

    final IStatus status = RunnableContextHelper.execute( getContainer(), true, false, op );
    if( !status.isOK() )
      KalypsoModel1D2DPlugin.getDefault().getLog().log( status );
    ErrorDialog.openError( getShell(), getWindowTitle(), Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.wizard.profileImport.ImportTrippelWizard.17" ), status ); //$NON-NLS-1$

    return !status.matches( IStatus.ERROR );
  }

  /**
   * Converts the profiles in GML (-> terrain model).
   * 
   * @param networkCollection
   *          the GML river network, in which the profiles will be stored
   * @param addedFeatures
   */
  protected IStatus doImportNetwork( final IRiverProfileNetworkCollection networkCollection, final List<Feature> addedFeatures, final List<IProfil> profiles ) throws Exception
  {
    final IRiverProfileNetwork network = networkCollection.addNew( IRiverProfileNetwork.QNAME );
    final Feature networkFeature = network.getFeature();
    addedFeatures.add( networkFeature );

    /* Set user friendly name and description */
    final String desc = Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.wizard.profileImport.ImportTrippelWizard.19", m_ProfilePage.getFileName(), ImportTrippleWizard.DF.format( new Date() ), m_ProfilePage.getFilePath() ); //$NON-NLS-1$
    network.setName( FileUtilities.nameWithoutExtension( m_ProfilePage.getFileName() ) );
    network.setDescription( desc );
    final String crs = m_ProfilePage.getCoordinateSystem();

    final GMLWorkspace workspace = networkFeature.getWorkspace();

    final String coordinatesSystem = KalypsoDeegreePlugin.getDefault().getCoordinateSystem();
    workspace.accept( new TransformVisitor( coordinatesSystem ), networkFeature, FeatureVisitor.DEPTH_INFINITE );

    for( final IProfil profile : profiles )
    {
      final IProfileFeature profileFeature = (IProfileFeature) FeatureHelper.addFeature( network.getFeature(), IRiverProfileNetwork.QNAME_PROP_RIVER_PROFILE, new QName( IWspmConstants.NS_WSPMPROF, "Profile"  ) ); //$NON-NLS-1$ //$NON-NLS-1$
      profileFeature.invalidEnvelope();
      ProfileFeatureFactory.toFeature( profile, profileFeature );
      profileFeature.setSrsName( crs );
      addedFeatures.add( profileFeature );
    }

    final GMLWorkspace workspace2 = network.getFeature().getWorkspace();
    workspace.fireModellEvent( new FeatureStructureChangeModellEvent( workspace2, network.getFeature(), networkFeature, FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_ADD ) );

    m_network = network;

    return Status.OK_STATUS;
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
