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
package org.kalypso.model.wspm.pdb.ui.internal.content;

import java.io.File;
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import javax.xml.namespace.QName;

import org.apache.commons.lang.CharEncoding;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.gmlschema.GMLSchemaException;
import org.kalypso.model.wspm.pdb.db.mapping.CrossSection;
import org.kalypso.model.wspm.pdb.db.mapping.State;
import org.kalypso.model.wspm.pdb.db.mapping.WaterBody;
import org.kalypso.model.wspm.pdb.ui.internal.WspmPdbUiPlugin;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhWspmProject;
import org.kalypso.ogc.gml.serialize.GmlSerializeException;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;

/**
 * @author Gernot Belger
 */
public class CheckoutOperation implements ICoreRunnableWithProgress
{
  private final IStructuredSelection m_selection;

  private final Set<CrossSection> m_crossSections = new HashSet<CrossSection>();

  public CheckoutOperation( final IStructuredSelection selection )
  {
    m_selection = selection;
  }

  @Override
  public IStatus execute( final IProgressMonitor monitor ) throws CoreException
  {
    findCrossSections();

    // TODO Preview?

    // TODO Already exists in local workspace ?

    checkoutCrossSections();

    return Status.OK_STATUS;
  }

  private void findCrossSections( )
  {
    final List< ? > list = m_selection.toList();
    addElementsAsCrossSection( list );
  }

  private void addElementsAsCrossSection( final Collection< ? > elements )
  {
    for( final Object element : elements )
      addElementAsCrossSection( element );
  }

  private void addElementAsCrossSection( final Object element )
  {
    if( element instanceof CrossSection )
      m_crossSections.add( (CrossSection) element );
    else if( element instanceof State )
    {
      final State state = (State) element;
      addElementsAsCrossSection( state.getCrossSections() );
    }
    else if( element instanceof WaterBody )
    {
      final WaterBody waterBody = (WaterBody) element;
      addElementsAsCrossSection( waterBody.getCrossSections() );
    }
  }

  private void checkoutCrossSections( ) throws CoreException
  {
    try
    {
      final IPath modelLocation = getProjectLocation();

      // TODO create wspm project
      final TuhhWspmProject wspmProject = initializeProject( modelLocation );
      // TODO add profiles; add water body / state if necessary

      final CrossSectionInserter inserter = new CrossSectionInserter( wspmProject );
      for( final CrossSection crossSection : m_crossSections )
        inserter.insert( crossSection );

      saveProject( modelLocation, wspmProject );
    }
    catch( final MalformedURLException e )
    {
      e.printStackTrace();
      final IStatus status = new Status( IStatus.ERROR, WspmPdbUiPlugin.PLUGIN_ID, "Invalid state location", e );
      throw new CoreException( status );
    }
    catch( final GMLSchemaException e )
    {
      e.printStackTrace();
      final IStatus status = new Status( IStatus.ERROR, WspmPdbUiPlugin.PLUGIN_ID, "Should never happen", e ); //$NON-NLS-1$
      throw new CoreException( status );
    }
    catch( final IOException e )
    {
      e.printStackTrace();
      final IStatus status = new Status( IStatus.ERROR, WspmPdbUiPlugin.PLUGIN_ID, "Failed to save WPSM project", e );
      throw new CoreException( status );
    }
    catch( final GmlSerializeException e )
    {
      e.printStackTrace();
      final IStatus status = new Status( IStatus.ERROR, WspmPdbUiPlugin.PLUGIN_ID, "Failed to save WPSM project", e );
      throw new CoreException( status );
    }
    catch( final GM_Exception e )
    {
      e.printStackTrace();
      final IStatus status = new Status( IStatus.ERROR, WspmPdbUiPlugin.PLUGIN_ID, "Should never happen", e ); //$NON-NLS-1$
      throw new CoreException( status );
    }
  }

  private IPath getProjectLocation( )
  {
    // TODO: move elsewhere...
    final IPath stateLocation = WspmPdbUiPlugin.getDefault().getStateLocation();
    final IPath wspmDataLocation = stateLocation.append( "wspmData" ); //$NON-NLS-1$
    return wspmDataLocation.append( "modell.gml" ); //$NON-NLS-1$
  }

  private TuhhWspmProject initializeProject( final IPath modelLocation ) throws MalformedURLException, GMLSchemaException
  {
    final URL modelURL = modelLocation.toFile().toURI().toURL();

    final QName rootQName = TuhhWspmProject.QNAME;
    final GMLWorkspace workspace = FeatureFactory.createGMLWorkspace( rootQName, modelURL, null );
    return (TuhhWspmProject) workspace.getRootFeature();
  }

  private void saveProject( final IPath modelLocation, final TuhhWspmProject project ) throws IOException, GmlSerializeException
  {
    final File modelFile = modelLocation.toFile();
    modelFile.getParentFile().mkdirs();
    GmlSerializer.serializeWorkspace( modelFile, project.getWorkspace(), CharEncoding.UTF_8 );
  }
}