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
package org.kalypso.model.hydrology.project;

import java.util.LinkedHashSet;
import java.util.Set;

import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceVisitor;

/**
 * Collects simulation results folders
 * 
 * @author Dirk Kuch
 */
class HydrologyCalculationFoldersCollector implements IResourceVisitor
{
  private final Set<RrmCalculationResult> m_folders = new LinkedHashSet<>();

  private final RrmSimulation m_simulation;

  public HydrologyCalculationFoldersCollector( final RrmSimulation simulation )
  {
    m_simulation = simulation;
  }

  @Override
  public boolean visit( final IResource resource )
  {
    if( !(resource instanceof IFolder) )
      return true;

    final IFolder folder = (IFolder) resource;
    if( isCalculationCaseFolder( folder ) )
      return true;

    if( isResultFolder( folder ) )
      m_folders.add( new RrmCalculationResult( folder ) );

    return true;
  }

  public static boolean isResultFolder( final IFolder folder )
  {
    final RrmCalculationResult result = new RrmCalculationResult( folder );
    return result.isResultFolder();
  }

  private boolean isCalculationCaseFolder( final IFolder folder )
  {
    return folder.equals( m_simulation.getResultsFolder() );
  }

  public RrmCalculationResult[] getFolders( )
  {
    return m_folders.toArray( new RrmCalculationResult[] {} );
  }
}