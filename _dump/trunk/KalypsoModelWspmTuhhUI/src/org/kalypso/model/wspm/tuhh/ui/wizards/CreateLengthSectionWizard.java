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
package org.kalypso.model.wspm.tuhh.ui.wizards;

import java.io.File;
import java.io.InputStream;
import java.net.URL;
import java.util.List;

import javax.xml.namespace.QName;

import org.apache.commons.io.IOUtils;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.eclipse.jface.wizard.Wizard;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.contribs.eclipse.core.runtime.PluginUtilities;
import org.kalypso.contribs.eclipse.jface.wizard.ArrayChooserPage;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.tuhh.core.profile.WspmTuhhProfileHelper;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;
import org.kalypso.model.wspm.ui.KalypsoModelWspmUIPlugin;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.ogc.gml.om.ObservationFeatureFactory;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ogc.gml.serialize.GmlSerializerFeatureProviderFactory;
import org.kalypso.ui.editor.gmleditor.ui.GMLLabelProvider;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;

/**
 * @author kimwerner
 */
public class CreateLengthSectionWizard extends Wizard
{

  final private ArrayChooserPage m_profileChooserPage;

  final private List<Feature> m_profiles;

  final private List<Feature> m_selectedProfiles;

  final private GMLWorkspace m_GMLws;

  public CreateLengthSectionWizard( final GMLWorkspace ws, final List<Feature> profiles, final List<Feature> selection )
  {
    m_GMLws = ws;
    m_profiles = profiles;
    m_selectedProfiles = selection;
    setWindowTitle( Messages.getString( "org.kalypso.model.wspm.tuhh.ui.wizardsCreateLengthSectionWizard.0" ) ); //$NON-NLS-1$
    setNeedsProgressMonitor( true );
    setDialogSettings( PluginUtilities.getDialogSettings( KalypsoModelWspmUIPlugin.getDefault(), getClass().getName() ) );
    m_profileChooserPage = new ArrayChooserPage( m_profiles, new Object[0], m_selectedProfiles.toArray(), 1, "profilesChooserPage", Messages.getString( "org.kalypso.model.wspm.tuhh.ui.wizardsCreateLengthSectionWizard.1" ), null ); //$NON-NLS-1$ //$NON-NLS-2$ 
    m_profileChooserPage.setLabelProvider( new GMLLabelProvider() );
    m_profileChooserPage.setMessage( Messages.getString( "org.kalypso.model.wspm.tuhh.ui.wizardsCreateLengthSectionWizard.2" ) ); //$NON-NLS-1$
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

  /**
   * @see org.eclipse.jface.wizard.Wizard#performFinish()
   */
  @Override
  public boolean performFinish( )
  {
    final Object[] profilFeatures = m_profileChooserPage.getChoosen();
    URL context = m_GMLws.getContext();
    IProject wspmProjekt = ResourceUtilities.findProjectFromURL( context );
    IFolder parentFolder = wspmProjekt.getFolder( "L�ngsschnitte" ); //$NON-NLS-1$
    try
    {
      if( !parentFolder.exists() )
        parentFolder.create( false, true, new NullProgressMonitor() );

      final String fName = String.format( "station(%.4f)%d", ((IProfileFeature) profilFeatures[0]).getStation(), profilFeatures.length ); //$NON-NLS-1$
      IFolder targetFolder = parentFolder.getFolder( fName );
      if( !targetFolder.exists() )
        targetFolder.create( false, true, new NullProgressMonitor() );

      final IFile targetFile = targetFolder.getFile( new Path( fName + ".gml" ) ); //$NON-NLS-1$
      final File targetJavaFile = targetFile.getLocation().toFile();

      final GMLWorkspace lsWorkspace = FeatureFactory.createGMLWorkspace( new QName( "http://www.opengis.net/om", "Observation" ), targetJavaFile.toURI().toURL(), new GmlSerializerFeatureProviderFactory() ); //$NON-NLS-1$ //$NON-NLS-2$
      final IObservation<TupleResult> lengthSection = WspmTuhhProfileHelper.profilesToLengthSection( profilFeatures );
      ObservationFeatureFactory.toFeature( lengthSection, lsWorkspace.getRootFeature() );
      GmlSerializer.serializeWorkspace( targetJavaFile, lsWorkspace, "UTF-8" ); //$NON-NLS-1$

      final IFile kodFile = targetFolder.getFile( new Path( fName + ".kod" ) ); //$NON-NLS-1$
      if( !kodFile.exists() )
      {
        final URL resource = getClass().getResource( "resources/LS_no_result.kod" ); //$NON-NLS-1$
        String kod = FileUtilities.toString( resource, "UTF-8" ).replaceAll( "%localPath%", fName + ".gml" ); //$NON-NLS-1$  //$NON-NLS-3$
        kod = kod.replaceAll( "%title%", fName ); //$NON-NLS-1$
        kod = kod.replaceAll( "%description%", fName ); //$NON-NLS-1$
        final InputStream inputStream = IOUtils.toInputStream( kod, "UTF-8" ); //$NON-NLS-1$
        kodFile.create( inputStream, true, new NullProgressMonitor() );
      }

      targetFolder.refreshLocal( IResource.DEPTH_ONE, new NullProgressMonitor() );
    }
    catch( Exception e )
    {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
    return true;
  }
}
