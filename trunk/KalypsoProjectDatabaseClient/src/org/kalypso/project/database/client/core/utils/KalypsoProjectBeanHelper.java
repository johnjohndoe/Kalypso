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
package org.kalypso.project.database.client.core.utils;

import java.util.Comparator;
import java.util.Set;
import java.util.TreeSet;

import org.kalypso.project.database.sei.beans.KalypsoProjectBean;

/**
 * @author Dirk Kuch
 */
public class KalypsoProjectBeanHelper
{

  /**
   * @param headBean
   *          Head of {@link IKalypsoProject}
   * @return sorted list of beans of one project [head, head-1, head-2, ... head-n]
   */
  public static KalypsoProjectBean[] getSortedBeans( final KalypsoProjectBean headBean )
  {
    final Set<KalypsoProjectBean> beans = new TreeSet<KalypsoProjectBean>( new Comparator<KalypsoProjectBean>()
    {
      @Override
      public int compare( final KalypsoProjectBean o1, final KalypsoProjectBean o2 )
      {
        return o2.getProjectVersion().compareTo( o1.getProjectVersion() );
      }
    } );
    beans.add( headBean );

    final KalypsoProjectBean[] children = headBean.getChildren();
    for( final KalypsoProjectBean child : children )
    {
      beans.add( child );
    }

    return beans.toArray( new KalypsoProjectBean[] {} );
  }

}
