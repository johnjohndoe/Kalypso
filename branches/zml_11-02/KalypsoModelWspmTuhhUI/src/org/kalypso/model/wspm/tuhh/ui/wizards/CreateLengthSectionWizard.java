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
package org.kalypso.model.wspm.tuhh.ui.wizards;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.util.SortedMap;
import java.util.TreeMap;

import javax.xml.namespace.QName;

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
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.ide.IDE;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.core.status.StatusDialog;
import org.kalypso.gmlschema.GMLSchemaCatalog;
import org.kalypso.gmlschema.GMLSchemaException;
import org.kalypso.gmlschema.IGMLSchema;
import org.kalypso.gmlschema.KalypsoGMLSchemaPlugin;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.tuhh.core.profile.LengthSectionCreator;
import org.kalypso.model.wspm.tuhh.ui.KalypsoModelWspmTuhhUIPlugin;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;
import org.kalypso.model.wspm.ui.action.ProfileSelection;
import org.kalypso.model.wspm.ui.profil.wizard.ProfilesChooserPage;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.ogc.gml.om.ObservationFeatureFactory;
import org.kalypso.ogc.gml.serialize.GmlSerializeException;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ogc.gml.serialize.GmlSerializerFeatureProviderFactory;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;

/**
 * FIXME: use the LengthSectionExportPage?
 * 
 * @author kimwerner
 */
public class CreateLengthSectionWizard extends Wizard
{
  final private ProfilesChooserPage m_profileChooserPage;

  final private GMLWorkspace m_workspace;

  public CreateLengthSectionWizard( final ProfileSelection profileSelection )
  {
    m_workspace = profileSelection.getWorkspace();

    setNeedsProgressMonitor( true );
    final String description = Messages.getString( "org.kalypso.model.wspm.tuhh.ui.wizardsCreateLengthSectionWizard.2" ); //$NON-NLS-1$
    m_profileChooserPage = new ProfilesChooserPage( description, profileSelection, false );
  }

  /**
   * @see org.eclipse.jface.wizard.Wizard#addPages()
   */
  @Override
  public void addPages( )
  {
    super.addPages();
    addPage( m_profileChooserPage );
  }

  private final IProfil[] extractProfiles( final Object[] profilFeatures )
  {
    final SortedMap<Double, IProfil> profiles = new TreeMap<Double, IProfil>();
    for( final Object objProfileFeature : profilFeatures )
    {
      if( !(objProfileFeature instanceof IProfileFeature) )
        continue;
      final IProfileFeature profileFeature = (IProfileFeature) objProfileFeature;

      final IProfil profil = profileFeature.getProfil();
      final double station = profil.getStation();
      profiles.put( station, profil );
    }

    return profiles.values().toArray( new IProfil[profiles.size()] );
  }

  /**
   * @see org.eclipse.jface.wizard.Wizard#performFinish()
   */
  @Override
  public boolean performFinish( )
  {
    final Object[] profilFeatures = m_profileChooserPage.getChoosen();
    final URL context = m_workspace.getContext();
    final IProject wspmProjekt = ResourceUtilities.findProjectFromURL( context );
    final IFolder parentFolder = wspmProjekt.getFolder( "L‰ngsschnitte" ); //$NON-NLS-1$
    try
    {
      doExport( profilFeatures, context, parentFolder );
    }
    catch( final Throwable t )
    {
      final String message = String.format( Messages.getString("CreateLengthSectionWizard.0"), t.getLocalizedMessage() ); //$NON-NLS-1$
      final IStatus status = new Status( IStatus.ERROR, KalypsoModelWspmTuhhUIPlugin.getID(), message, t );
      KalypsoModelWspmTuhhUIPlugin.getDefault().getLog().log( status );
      new StatusDialog( getShell(), status, getWindowTitle() ).open();
    }
    return true;
  }

  private void doExport( final Object[] profilFeatures, final URL context, final IFolder parentFolder ) throws CoreException, GMLSchemaException, IOException, GmlSerializeException, PartInitException
  {
    if( !parentFolder.exists() )
      parentFolder.create( false, true, new NullProgressMonitor() );

    final String fName = String.format( "station(%.4f)%d", ((IProfileFeature) profilFeatures[0]).getStation(), profilFeatures.length ); //$NON-NLS-1$
    final IFolder targetFolder = parentFolder.getFolder( fName );
    if( !targetFolder.exists() )
      targetFolder.create( false, true, new NullProgressMonitor() );

    final IFile targetFile = targetFolder.getFile( new Path( fName + ".gml" ) ); //$NON-NLS-1$
    final File targetJavaFile = targetFile.getLocation().toFile();

    final String gmlVersion = null;
    final GMLSchemaCatalog schemaCatalog = KalypsoGMLSchemaPlugin.getDefault().getSchemaCatalog();
    final IGMLSchema schema = schemaCatalog.getSchema( new QName( "http://www.opengis.net/om", "Observation" ).getNamespaceURI(), gmlVersion ); //$NON-NLS-1$ //$NON-NLS-2$
    final IFeatureType rootFeatureType = schema.getFeatureType( new QName( "http://www.opengis.net/om", "Observation" ) ); //$NON-NLS-1$ //$NON-NLS-2$
    final Feature rootFeature = FeatureFactory.createFeature( null, null, Messages.getString("CreateLengthSectionWizard.1"), rootFeatureType, true ); //$NON-NLS-1$
    final GMLWorkspace lsWorkspace = FeatureFactory.createGMLWorkspace( schema, rootFeature, context, null, new GmlSerializerFeatureProviderFactory(), null );

    final IProfil[] profiles = extractProfiles( profilFeatures );
    final LengthSectionCreator lsCreator = new LengthSectionCreator( profiles );
    final IObservation<TupleResult> lengthSection = lsCreator.toLengthSection();

    ObservationFeatureFactory.toFeature( lengthSection, rootFeature );
    GmlSerializer.serializeWorkspace( targetJavaFile, lsWorkspace, "UTF-8" ); //$NON-NLS-1$

    final IFile kodFile = targetFolder.getFile( new Path( fName + ".kod" ) ); //$NON-NLS-1$
    final IFile tableFile = targetFolder.getFile( new Path( fName + ".gft" ) ); //$NON-NLS-1$
    copyResourceFile( "resources/LS_no_result.kod", kodFile, fName ); //$NON-NLS-1$
    copyResourceFile( "resources/table.gft", tableFile, fName ); //$NON-NLS-1$

    final IWorkbenchPage page = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage();
    IDE.openEditor( page, kodFile, true );
  }

  private void copyResourceFile( final String resource, final IFile targetFile, final String fName ) throws IOException, CoreException
  {
    if( targetFile.exists() )
      return;

    final URL resourceLocation = getClass().getResource( resource );
    String kod = FileUtilities.toString( resourceLocation, "UTF-8" ); //$NON-NLS-1$
    kod = kod.replaceAll( "%GMLFILENAME%", fName + ".gml" ); //$NON-NLS-1$ //$NON-NLS-2$  //$NON-NLS-3$
    kod = kod.replaceAll( "%TITLE%", fName ); //$NON-NLS-1$
    kod = kod.replaceAll( "%DESCRIPTION%", fName ); //$NON-NLS-1$
    final InputStream inputStream = IOUtils.toInputStream( kod, "UTF-8" ); //$NON-NLS-1$
    targetFile.create( inputStream, true, new NullProgressMonitor() );

    targetFile.getParent().refreshLocal( IResource.DEPTH_ONE, new NullProgressMonitor() );
  }
}
