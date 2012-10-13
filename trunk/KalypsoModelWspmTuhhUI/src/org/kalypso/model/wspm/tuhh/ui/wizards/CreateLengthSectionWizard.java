/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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
package org.kalypso.model.wspm.tuhh.ui.wizards;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;

import org.apache.commons.io.IOUtils;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.ui.IViewPart;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWizard;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.ide.IDE;
import org.eclipse.ui.part.FileEditorInput;
import org.kalypso.chart.ui.view.ChartView;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.core.status.StatusDialog;
import org.kalypso.gmlschema.GMLSchemaException;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.core.gml.WspmWaterBody;
import org.kalypso.model.wspm.core.profil.IProfile;
import org.kalypso.model.wspm.tuhh.core.profile.LengthSectionCreator;
import org.kalypso.model.wspm.tuhh.ui.KalypsoModelWspmTuhhUIPlugin;
import org.kalypso.model.wspm.tuhh.ui.export.wspwin.ProfileFeatureSorter;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;
import org.kalypso.model.wspm.ui.action.ProfilesSelection;
import org.kalypso.model.wspm.ui.profil.wizard.ProfileHandlerUtils;
import org.kalypso.model.wspm.ui.profil.wizard.ProfilesChooserPage;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.ogc.gml.om.IObservationFeature;
import org.kalypso.ogc.gml.om.ObservationFeatureFactory;
import org.kalypso.ogc.gml.serialize.GmlSerializeException;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ogc.gml.serialize.GmlSerializerFeatureProviderFactory;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;

/**
 * @author Kim Werner
 */
public class CreateLengthSectionWizard extends Wizard implements IWorkbenchWizard
{
  private ProfilesChooserPage m_profileChooserPage;

  private ProfilesSelection m_profileSelection;

  public CreateLengthSectionWizard( )
  {
    setWindowTitle( Messages.getString( "CreateLengthSectionHandler_0" ) ); //$NON-NLS-1$
    setNeedsProgressMonitor( true );
  }

  @Override
  public void init( final IWorkbench workbench, final IStructuredSelection selection )
  {
    m_profileSelection = ProfileHandlerUtils.getSelectionChecked( selection );

    final String description = Messages.getString( "org.kalypso.model.wspm.tuhh.ui.wizardsCreateLengthSectionWizard.2" ); //$NON-NLS-1$
    m_profileChooserPage = new ProfilesChooserPage( description, m_profileSelection, false, 2 );

    addPage( m_profileChooserPage );
  }

  @Override
  public boolean performFinish( )
  {
    final Object[] profilFeatures = m_profileChooserPage.getChoosen();

    try
    {
      final URL context = m_profileSelection.getWorkspace().getContext();
      final IProject wspmProjekt = ResourceUtilities.findProjectFromURL( context );
      final IFolder parentFolder = wspmProjekt.getFolder( "Längsschnitte" ); //$NON-NLS-1$

      final IFile kodFile = doExport( profilFeatures, context, parentFolder );
      openKod( kodFile );
    }
    catch( final Throwable t )
    {
      final String message = String.format( Messages.getString( "CreateLengthSectionWizard.0" ), t.getLocalizedMessage() ); //$NON-NLS-1$
      final IStatus status = new Status( IStatus.ERROR, KalypsoModelWspmTuhhUIPlugin.getID(), message, t );
      KalypsoModelWspmTuhhUIPlugin.getDefault().getLog().log( status );
      new StatusDialog( getShell(), status, getWindowTitle() ).open();
    }
    return true;
  }

  private final String getAxisDirection( final Object[] profilFeatures )
  {
    if( profilFeatures.length > 0 && profilFeatures[0] instanceof IProfileFeature )
    {
      final WspmWaterBody waterBody = ((IProfileFeature) profilFeatures[0]).getWater();
      if( waterBody != null && waterBody.isDirectionUpstreams() == false )
      {
        return "POSITIVE"; //$NON-NLS-1$
      }
    }
    return "NEGATIVE"; //$NON-NLS-1$
  }

  private IFile doExport( final Object[] profilFeatures, final URL context, final IFolder parentFolder ) throws CoreException, GMLSchemaException, IOException, GmlSerializeException
  {
    if( !parentFolder.exists() )
      parentFolder.create( false, true, new NullProgressMonitor() );

    final IProfile[] profiles = ProfileFeatureSorter.extractProfiles( profilFeatures, null );

    final String containerName = getContainerName();

    final String fName = String.format( "%s_%.4f-%.4f", containerName, profiles[0].getStation(), profiles[profiles.length - 1].getStation() ); //$NON-NLS-1$
    final String title = String.format( Messages.getString( "CreateLengthSectionWizard.1" ), containerName, profiles[0].getStation(), profiles[profiles.length - 1].getStation() ); //$NON-NLS-1$
    final IFolder targetFolder = parentFolder.getFolder( fName );
    if( !targetFolder.exists() )
      targetFolder.create( false, true, new NullProgressMonitor() );

    final IFile targetFile = targetFolder.getFile( new Path( fName + ".gml" ) ); //$NON-NLS-1$
    final File targetJavaFile = targetFile.getLocation().toFile();

    final GMLWorkspace lsWorkspace = FeatureFactory.createGMLWorkspace( IObservationFeature.FEATURE_OBSERVATION, context, new GmlSerializerFeatureProviderFactory() );
    final Feature rootFeature = lsWorkspace.getRootFeature();

    final LengthSectionCreator lsCreator = new LengthSectionCreator( profiles );
    final IObservation<TupleResult> lengthSection = lsCreator.toLengthSection();

    ObservationFeatureFactory.toFeature( lengthSection, rootFeature );
    GmlSerializer.serializeWorkspace( targetJavaFile, lsWorkspace, "UTF-8" ); //$NON-NLS-1$

    final IFile kodFile = targetFolder.getFile( new Path( fName + ".kod" ) ); //$NON-NLS-1$
    final IFile tableFile = targetFolder.getFile( new Path( fName + ".gft" ) ); //$NON-NLS-1$
    copyResourceFile( "resources/LS_no_result.kod", kodFile, fName, title, getAxisDirection( profilFeatures ) ); //$NON-NLS-1$
    copyResourceFile( "resources/table.gft", tableFile, fName, title, "" ); //$NON-NLS-1$ //$NON-NLS-2$

    return kodFile;
  }

  private String getContainerName( )
  {
    final Feature parent = m_profileSelection.getContainer();
    if( parent == null )
      return null;

    return parent.getName();
  }

  private void copyResourceFile( final String resource, final IFile targetFile, final String fName, final String title, final String direction ) throws IOException, CoreException
  {
    final URL resourceLocation = getClass().getResource( resource );
    String kod = FileUtilities.toString( resourceLocation, "UTF-8" ); //$NON-NLS-1$
    kod = kod.replaceAll( "%GMLFILENAME%", fName + ".gml" ); //$NON-NLS-1$ //$NON-NLS-2$  //$NON-NLS-3$
    kod = kod.replaceAll( "%TITLE%", title ); //$NON-NLS-1$
    kod = kod.replaceAll( "%DESCRIPTION%", fName ); //$NON-NLS-1$
    kod = kod.replaceAll( "%DIRECTIONUPSTREAM%", direction ); //$NON-NLS-1$

    final InputStream inputStream = IOUtils.toInputStream( kod, "UTF-8" ); //$NON-NLS-1$

    if( targetFile.exists() )
      targetFile.setContents( inputStream, false, true, new NullProgressMonitor() );
    else
      targetFile.create( inputStream, true, new NullProgressMonitor() );

    targetFile.getParent().refreshLocal( IResource.DEPTH_ONE, new NullProgressMonitor() );
  }

  private void openKod( final IFile kodFile ) throws PartInitException
  {
    final IWorkbenchPage page = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage();

    if( page.isEditorAreaVisible() )
      IDE.openEditor( page, kodFile, true );
    else
    {
      // Open in chart view if we have no editor area. Else we break the perspective layout.
      final IViewPart chartView = page.showView( ChartView.ID );
      if( chartView instanceof ChartView )
        ((ChartView) chartView).setInput( new FileEditorInput( kodFile ) );
    }
  }
}