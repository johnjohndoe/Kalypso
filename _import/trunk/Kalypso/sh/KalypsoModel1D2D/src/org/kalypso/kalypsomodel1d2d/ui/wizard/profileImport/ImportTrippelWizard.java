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
import java.lang.reflect.InvocationTargetException;
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
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRiverProfileNetwork;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRiverProfileNetworkCollection;
import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.gml.ProfileFeatureFactory;
import org.kalypso.model.wspm.core.gml.WspmProfile;
import org.kalypso.model.wspm.core.imports.ImportTrippleHelper;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureVisitor;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.event.FeatureStructureChangeModellEvent;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;
import org.kalypsodeegree_impl.model.feature.visitors.TransformVisitor;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * A wizard to import profile data (right now jus as trippel) into a 1D2D Terrain Model.
 * 
 * @author Thomas Jung
 */
public class ImportTrippelWizard extends Wizard implements IWizard
{
  private static final DateFormat DF = DateFormat.getDateTimeInstance( DateFormat.MEDIUM, DateFormat.SHORT );

  private final List<Feature> m_terrainModelAdds = new ArrayList<Feature>();

  private final IRiverProfileNetworkCollection m_networkModel;

  protected ImportProfilePage m_ProfilePage;

  private IRiverProfileNetwork m_network;

  public ImportTrippelWizard( final IRiverProfileNetworkCollection networkModel )
  {

    m_networkModel = networkModel;

    setWindowTitle( Messages.getString( "ImportTrippelWizard.0" ) ); //$NON-NLS-1$

    setNeedsProgressMonitor( true );
  }

  /**
   * @see org.eclipse.jface.wizard.Wizard#addPages()
   */
  @Override
  public void addPages( )
  {
    /* Choose profile data */
    m_ProfilePage = new ImportProfilePage( "chooseProfileData", Messages.getString( "ImportTrippelWizard.2" ), null ); //$NON-NLS-1$ //$NON-NLS-2$
    m_ProfilePage.setDescription( Messages.getString( "ImportTrippelWizard.3" ) ); //$NON-NLS-1$

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
      public IStatus execute( final IProgressMonitor monitor ) throws InvocationTargetException
      {
        monitor.beginTask( Messages.getString( "ImportTrippelWizard.13" ), 2 ); //$NON-NLS-1$

        try
        {
          /* Import Trippel Data */
          monitor.subTask( Messages.getString( "ImportTrippelWizard.14" ) ); //$NON-NLS-1$

          /* get file name from wizard */
          final File trippelFile = m_ProfilePage.getFile();
          List<IProfil> profiles = ImportTrippleHelper.importTrippelData( trippelFile, IWspmTuhhConstants.PROFIL_TYPE_PASCHE );

          monitor.worked( 1 );

          /* Convert Trippel Data */
          monitor.subTask( Messages.getString( "ImportTrippelWizard.16" ) ); //$NON-NLS-1$

          final IStatus status = doImportNetwork( profNetworkColl, terrainModelAdds, profiles );

          monitor.worked( 1 );

          return status;
        }
        catch( final Exception e )
        {
          return StatusUtilities.statusFromThrowable( e, Messages.getString( "ImportTrippelWizard.15" ) ); //$NON-NLS-1$
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
    ErrorDialog.openError( getShell(), getWindowTitle(), Messages.getString( "ImportTrippelWizard.17" ), status ); //$NON-NLS-1$

    return !status.matches( IStatus.ERROR );
  }

  /**
   * Converts the profiles in GML (-> terrain model).
   * 
   * @param networkCollection
   *            the GML river network, in which the profiles will be stored
   * @param addedFeatures
   */
  @SuppressWarnings("deprecation")//$NON-NLS-1$
  protected IStatus doImportNetwork( final IRiverProfileNetworkCollection networkCollection, final List<Feature> addedFeatures, final List<IProfil> profiles ) throws Exception
  {
    final IRiverProfileNetwork network = networkCollection.addNew( IRiverProfileNetwork.QNAME );
    final Feature networkFeature = network.getWrappedFeature();
    addedFeatures.add( networkFeature );

    /* Set user friendly name and descrption */
    final String desc = String.format( Messages.getString( "ImportTrippelWizard.19" ), m_ProfilePage.getFileName(), ImportTrippelWizard.DF.format( new Date() ), m_ProfilePage.getFilePath() ); //$NON-NLS-1$
    network.setName( FileUtilities.nameWithoutExtension( m_ProfilePage.getFileName() ) );
    network.setDescription( desc );
    final CS_CoordinateSystem crs = m_ProfilePage.getCoordinateSystem();

    final GMLWorkspace workspace = networkFeature.getWorkspace();

    final CS_CoordinateSystem coordinatesSystem = KalypsoCorePlugin.getDefault().getCoordinatesSystem();
    workspace.accept( new TransformVisitor( coordinatesSystem ), networkFeature, FeatureVisitor.DEPTH_INFINITE );

    for( final IProfil profile : profiles )
    {
      profile.getPointProperty( desc );
      final Feature profileFeature = FeatureHelper.addFeature( network.getWrappedFeature(), IRiverProfileNetwork.QNAME_PROP_RIVER_PROFILE, new QName( IWspmConstants.NS_WSPMPROF, Messages.getString( "ImportTrippelWizard.20" ) ) ); //$NON-NLS-1$
      ProfileFeatureFactory.toFeature( profile, profileFeature );
      new WspmProfile( profileFeature ).setSrsName( crs.getName() );
      profileFeature.invalidEnvelope();
      addedFeatures.add( profileFeature );
    }

    final GMLWorkspace workspace2 = network.getWrappedFeature().getWorkspace();
    workspace.fireModellEvent( new FeatureStructureChangeModellEvent( workspace2, network.getWrappedFeature(), networkFeature, FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_ADD ) );

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
