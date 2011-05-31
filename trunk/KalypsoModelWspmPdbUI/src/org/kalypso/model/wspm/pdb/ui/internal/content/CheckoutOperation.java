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
package org.kalypso.model.wspm.pdb.ui.internal.content;

import java.io.File;
import java.io.IOException;
import java.net.MalformedURLException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.apache.commons.lang.CharEncoding;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.gmlschema.GMLSchemaException;
import org.kalypso.model.wspm.pdb.db.mapping.CrossSection;
import org.kalypso.model.wspm.pdb.db.mapping.State;
import org.kalypso.model.wspm.pdb.db.mapping.WaterBody;
import org.kalypso.model.wspm.pdb.db.utils.ByStationComparator;
import org.kalypso.model.wspm.pdb.ui.internal.WspmPdbUiPlugin;
import org.kalypso.model.wspm.pdb.ui.internal.wspm.PdbWspmUtils;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhWspmProject;
import org.kalypso.ogc.gml.serialize.GmlSerializeException;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypsodeegree.model.geometry.GM_Exception;

/**
 * @author Gernot Belger
 */
public class CheckoutOperation implements ICoreRunnableWithProgress
{
  private final IStructuredSelection m_selection;

  private final Set<CrossSection> m_crossSections = new HashSet<CrossSection>();

  private final TuhhWspmProject m_project;

  public CheckoutOperation( final TuhhWspmProject project, final IStructuredSelection selection )
  {
    m_project = project;
    m_selection = selection;
  }

  @Override
  public IStatus execute( final IProgressMonitor monitor ) throws CoreException
  {
    monitor.beginTask( "Loading data from cross section database", 100 );

    monitor.subTask( "Searching for cross sections to checkout..." );
    findCrossSections();
    ProgressUtilities.worked( monitor, 5 );

    // TODO Preview?

    checkoutCrossSections( new SubProgressMonitor( monitor, 95 ) );

    ProgressUtilities.done( monitor );

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

  private void checkoutCrossSections( final IProgressMonitor monitor ) throws CoreException
  {
    final List<CrossSection> sortedSections = getSortedSections();
    monitor.beginTask( "Reading cross sections from database", sortedSections.size() + 10 );

    try
    {
      /* Initialize WSPM project */
      monitor.subTask( "Initializing WSPM project..." );
      final IPath modelLocation = PdbWspmUtils.getModelLocation();
      ProgressUtilities.worked( monitor, 10 );

      /* Convert the cross sections */
      final CrossSectionInserter inserter = new CrossSectionInserter( m_project );
      for( final CrossSection crossSection : sortedSections )
      {
        monitor.subTask( String.format( "Converting %s", crossSection.getStation() ) );
        inserter.insert( crossSection );
        ProgressUtilities.worked( monitor, 1 );
      }

      saveProject( modelLocation, m_project );
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
    finally
    {
      ProgressUtilities.done( monitor );
    }
  }

  private List<CrossSection> getSortedSections( )
  {
    final ArrayList<CrossSection> list = new ArrayList<CrossSection>( m_crossSections );
    Collections.sort( list, new ByStationComparator() );
    return list;
  }

  private void saveProject( final IPath modelLocation, final TuhhWspmProject project ) throws IOException, GmlSerializeException
  {
    final File modelFile = modelLocation.toFile();
    modelFile.getParentFile().mkdirs();
    GmlSerializer.serializeWorkspace( modelFile, project.getWorkspace(), CharEncoding.UTF_8 );
  }
}