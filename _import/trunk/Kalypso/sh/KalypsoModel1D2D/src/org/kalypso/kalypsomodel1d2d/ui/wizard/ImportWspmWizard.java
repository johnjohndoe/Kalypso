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
package org.kalypso.kalypsomodel1d2d.ui.wizard;

import java.io.OutputStreamWriter;
import java.net.URL;

import javax.xml.namespace.QName;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.IWizardPage;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.ui.IImportWizard;
import org.eclipse.ui.IWorkbench;
import org.kalypso.commons.resources.SetContentHelper;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.eclipse.jface.operation.RunnableContextHelper;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRiverProfile;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRiverProfileNetwork;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRiverProfileNetworkCollection;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.IWspmRiverProfileWrapper;
import org.kalypso.model.wspm.core.gml.WspmProfile;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhReach;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhReachProfileSegment;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ui.wizard.gml.GmlFileImportPage;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;

/**
 * A wizard to import WSPM-Models into a 1D2D Model.
 * 
 * @author Gernot Belger
 */
public class ImportWspmWizard extends Wizard implements IImportWizard
{
//  private FE1D2DDiscretisationModel m_model;

  private GmlFileImportPage m_wspmGmlPage;

  private GmlFileImportPage m_profileCollectionGmlPage;

  public ImportWspmWizard( )
  {
    setWindowTitle( "Kalypso Wspm Import" );

    setNeedsProgressMonitor( true );
  }

  /**
   * Excepts a selection containing a feature of type namespace:FE1D2DDiscretisationModel
   * 
   * @see org.eclipse.ui.IWorkbenchWizard#init(org.eclipse.ui.IWorkbench,
   *      org.eclipse.jface.viewers.IStructuredSelection)
   */
  public void init( final IWorkbench workbench, final IStructuredSelection selection )
  {
//    final Feature modelFeature = (Feature) selection.getFirstElement();

//    m_model = new FE1D2DDiscretisationModel( modelFeature );
  }

  /**
   * @see org.eclipse.jface.wizard.Wizard#addPages()
   */
  @Override
  public void addPages( )
  {
    /* Choose wspm-reach */
    m_wspmGmlPage = new GmlFileImportPage( "chooseWspmGml", "Gew‰sserstrang", null );
    m_wspmGmlPage.setDescription( "Bitte w‰hlen Sie einen Gew‰sserstrang aus" );
    m_wspmGmlPage.setValidQNames( new QName[] { TuhhReach.QNAME_TUHH_REACH } );
    m_wspmGmlPage.setValidKind( true, false );

    /* Choose network collection */
    m_profileCollectionGmlPage = new GmlFileImportPage( "chooseProfileNetworkGml", "Profile Network Collection", null );
    m_profileCollectionGmlPage.setDescription( "Bitte w‰hlen Sie eine Profile Network Collection aus" );
    m_profileCollectionGmlPage.setValidQNames( new QName[] { IRiverProfileNetworkCollection.QNAME } );
    m_wspmGmlPage.setValidKind( true, false );

    // maybe choose results

    addPage( m_wspmGmlPage );
    addPage( m_profileCollectionGmlPage );
  }

  /**
   * @see org.eclipse.jface.wizard.Wizard#getNextPage(org.eclipse.jface.wizard.IWizardPage)
   */
  @Override
  public IWizardPage getNextPage( final IWizardPage page )
  {
    final IWizardPage wizardPage = super.getNextPage( page );

    // TODO: if next page is result page, set choosen model to it

    return wizardPage;
  }

  /**
   * @see org.eclipse.jface.wizard.Wizard#performFinish()
   */
  @Override
  public boolean performFinish( )
  {
    /* Collect page data */
//    final GMLWorkspace wspmWorkspace = m_wspmGmlPage.getWorkspace();
    final IStructuredSelection wspmSelection = m_wspmGmlPage.getSelection();

    final GMLWorkspace profCollWorkspace = m_profileCollectionGmlPage.getWorkspace();
    final IStructuredSelection profCollSelection = m_profileCollectionGmlPage.getSelection();

    /* Do import */
    final ICoreRunnableWithProgress op = new ICoreRunnableWithProgress()
    {
      public IStatus execute( final IProgressMonitor monitor ) throws CoreException
      {
        monitor.beginTask( "1D-Modell wird importiert", 2 );

        /* Prepare input data */
        final TuhhReach reach = new TuhhReach( (Feature) wspmSelection.getFirstElement() );
        final IRiverProfileNetworkCollection networkCollection = (IRiverProfileNetworkCollection) ((Feature) profCollSelection.getFirstElement()).getAdapter( IRiverProfileNetworkCollection.class );

        /* Import reach into profile collection */
        monitor.subTask( " ... kopiere Profile" );
        try
        {
          doImportNetwork( reach, networkCollection );
        }
        catch( final Exception e )
        {
          return StatusUtilities.statusFromThrowable( e ,"Failed to copy profiles" );
        }
        monitor.worked( 1 );

        /* Create 1D-Network */

        /* Create 1D-Network Association */

        /* Save everything */
        final URL gmlURL = profCollWorkspace.getContext();

        // ists im Workspace?
        final IFile file = ResourceUtilities.findFileFromURL( gmlURL );
        final SetContentHelper thread = new SetContentHelper()
        {
          @Override
          protected void write( final OutputStreamWriter writer ) throws Throwable
          {
            GmlSerializer.serializeWorkspace( writer, profCollWorkspace );
          }
        };

        thread.setFileContents( file, false, true, new SubProgressMonitor( monitor, 1 ) );

        monitor.done();
        return Status.OK_STATUS;
      }
    };

    final IStatus status = RunnableContextHelper.execute( getContainer(), true, false, op );
    if( !status.isOK() )
      KalypsoModel1D2DPlugin.getDefault().getLog().log( status );
    ErrorDialog.openError( getShell(), getWindowTitle(), "Profile konnten nicht importiert werden", status );

    return status.isOK();
  }

  protected void doImportNetwork( final TuhhReach reach, final IRiverProfileNetworkCollection networkCollection ) throws Exception
  {
    final IRiverProfileNetwork network = networkCollection.addNew( IRiverProfileNetwork.QNAME );
    // network.setName( reach.getName() );
    // network.setDescription( reach.getDescription() );

    final TuhhReachProfileSegment[] reachProfileSegments = reach.getReachProfileSegments();
    for( final TuhhReachProfileSegment segment : reachProfileSegments )
    {
      final WspmProfile profileMember = segment.getProfileMember();

      final IRiverProfile profileWrapper = network.addNew( IWspmRiverProfileWrapper.QNAME );
      final IWspmRiverProfileWrapper wspmProfileWrapper = (IWspmRiverProfileWrapper) profileWrapper.getWrappedFeature().getAdapter( IWspmRiverProfileWrapper.class );
      
      final Feature wrappedFeature = profileWrapper.getWrappedFeature();
      final IRelationType wspmRelation = (IRelationType) wrappedFeature.getFeatureType().getProperty( IWspmRiverProfileWrapper.QNAME_PROP_WSPM_RIVER_PROFILE );
      final Feature clonedProfileFeature = FeatureHelper.cloneFeature( wrappedFeature, wspmRelation, profileMember.getFeature() );
      
      // profile.setName?
      wspmProfileWrapper.setWspmRiverProfile( new WspmProfile( clonedProfileFeature ) );
    }
  }
}
