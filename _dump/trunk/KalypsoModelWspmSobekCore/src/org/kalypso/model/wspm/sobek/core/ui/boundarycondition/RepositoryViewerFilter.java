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
package org.kalypso.model.wspm.sobek.core.ui.boundarycondition;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerFilter;
import org.kalypso.ogc.sensor.zml.repository.ZmlObservationItem;
import org.kalypso.repository.file.FileItem;

/**
 * @author kuch
 */
public class RepositoryViewerFilter extends ViewerFilter
{

  /**
   * @see org.eclipse.jface.viewers.ViewerFilter#filter(org.eclipse.jface.viewers.Viewer, java.lang.Object,
   *      java.lang.Object[])
   */
  @Override
  public Object[] filter( final Viewer viewer, final Object parent, final Object[] elements )
  {
    /* directories should listed first, sort - zmlobservationitems und fileitems! */
    final List<Object> objects = new ArrayList<Object>();
    final List<FileItem> items = new ArrayList<FileItem>();

    for( final Object object : elements )
      if( object instanceof ZmlObservationItem )
        objects.add( object );
      else if( object instanceof FileItem )
        items.add( (FileItem) object );
      else
        objects.add( object );

    final List<Object> sorted = new ArrayList<Object>();
    sorted.addAll( items );
    sorted.addAll( objects );

    return sorted.toArray();
  }

  /**
   * @see org.eclipse.jface.viewers.ViewerFilter#select(org.eclipse.jface.viewers.Viewer, java.lang.Object,
   *      java.lang.Object)
   */
  @Override
  public boolean select( final Viewer viewer, final Object parentElement, final Object element )
  {
    if( element instanceof FileItem )
    {
      final FileItem item = (FileItem) element;
      if( item.getName().startsWith( "." ) )
        return false;
      else if( "CVS".equals( item.getName() ) )
        return false;
    }

    return true;
  }
}
