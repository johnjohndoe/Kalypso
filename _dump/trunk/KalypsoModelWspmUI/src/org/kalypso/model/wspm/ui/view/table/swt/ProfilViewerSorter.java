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
package org.kalypso.model.wspm.ui.view.table.swt;

import java.util.logging.Level;
import java.util.logging.Logger;

import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerSorter;
import org.kalypso.model.wspm.core.profil.IProfilPoint;
import org.kalypso.model.wspm.core.profil.ProfilDataException;
import org.kalypso.model.wspm.core.profil.IProfilPoint.POINT_PROPERTY;
import org.kalypso.model.wspm.core.profil.impl.ProfilEventManager;


/**
 * @author gernot
 * 
 */
public class ProfilViewerSorter extends ViewerSorter
{
  private final POINT_PROPERTY m_columnKey;

  private final int m_direction;

  public ProfilViewerSorter( final POINT_PROPERTY key, final boolean direction )
  {
    m_columnKey = key;
    m_direction = direction ? -1 : 1;
  }

  /**
   * @see org.eclipse.jface.viewers.ViewerSorter#isSorterProperty(java.lang.Object, java.lang.String)
   */
  @Override
  public boolean isSorterProperty( final Object element, final String property )
  {
    System.out.println( property );

    return super.isSorterProperty( element, property );
  }

  /**
   * @see org.eclipse.jface.viewers.ViewerSorter#compare(org.eclipse.jface.viewers.Viewer, java.lang.Object,
   *      java.lang.Object)
   */
  @Override
  public int compare( final Viewer viewer, final Object e1, final Object e2 )
  {
    final Object input = viewer.getInput();
    if( m_columnKey == null || input == null || !(input instanceof ProfilEventManager) || e1 == null || !(e1 instanceof IProfilPoint) || e2 == null || !(e2 instanceof IProfilPoint) )
      return super.compare( viewer, e1, e2 );

    final IProfilPoint point1 = (IProfilPoint) e1;
    final IProfilPoint point2 = (IProfilPoint) e2;

    try
    {
      final double v1 = point1.getValueFor( m_columnKey );
      final double v2 = point2.getValueFor( m_columnKey );

      if( v1 < v2 )
        return -1 * m_direction;
      else if( v1 > v2 )
        return 1 * m_direction;
    }
    catch( final ProfilDataException e )
    {
      Logger.getLogger( getClass().getName() ).log( Level.SEVERE, "Fehler beim Zugriff auf Punktdaten", e );
    }

    return 0;
  }
}
