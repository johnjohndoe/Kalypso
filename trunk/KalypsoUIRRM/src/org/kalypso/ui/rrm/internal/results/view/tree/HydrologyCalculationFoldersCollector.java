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
package org.kalypso.ui.rrm.internal.results.view.tree;

import java.util.Comparator;
import java.util.Set;
import java.util.TreeSet;

import org.apache.commons.lang3.StringUtils;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceVisitor;

/**
 * Collects simulation results folders
 * 
 * @author Dirk Kuch
 */
public class HydrologyCalculationFoldersCollector implements IResourceVisitor
{
  Set<IFolder> m_folders = new TreeSet<>( new Comparator<IFolder>()
  {

    @Override
    public int compare( final IFolder folder1, final IFolder folder2 )
    {
      if( folder1 == folder2 )
        return 0;

      final String name1 = folder1.getName();
      final String name2 = folder2.getName();

      // FIXME english project template?!?
      if( StringUtils.equalsIgnoreCase( name1, "berechnet" ) )
        return 1;
      else if( StringUtils.equalsIgnoreCase( name2, "berechnet" ) )
        return -1;

      return name1.compareTo( name2 );
    }
  } );

  private final IFolder m_resultBaseFolder;

  public HydrologyCalculationFoldersCollector( final IFolder resultBaseFolder )
  {
    m_resultBaseFolder = resultBaseFolder;
  }

  @Override
  public boolean visit( final IResource resource )
  {
    if( !(resource instanceof IFolder) )
      return true;

    final IFolder folder = (IFolder) resource;
    if( isCalculationCaseFolder( folder ) )
      return true;

    // FIXME rework ignore cases
    final String name = folder.getName();
    if( StringUtils.containsIgnoreCase( name, "Original" ) )
      return true;
    else if( StringUtils.startsWithIgnoreCase( name, "tmp" ) )
      return true;
    else if( StringUtils.equalsIgnoreCase( name, "logs" ) )
      return true;

    m_folders.add( folder );

    return true;
  }

  private boolean isCalculationCaseFolder( final IFolder folder )
  {
    return folder.equals( m_resultBaseFolder );
  }

  public IFolder[] getFolders( )
  {
    return m_folders.toArray( new IFolder[] {} );
  }

}
