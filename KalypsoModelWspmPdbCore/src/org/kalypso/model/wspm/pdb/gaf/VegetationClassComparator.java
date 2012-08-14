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
package org.kalypso.model.wspm.pdb.gaf;

import java.util.Comparator;

import org.kalypso.contribs.java.lang.NumberUtils;
import org.kalypso.model.wspm.core.gml.classifications.IVegetationClass;

/**
 * @author Holger Albert
 */
public class VegetationClassComparator implements Comparator<IVegetationClass>
{
  /**
   * The constructor.
   */
  public VegetationClassComparator( )
  {
  }

  /**
   * @see java.util.Comparator#compare(java.lang.Object, java.lang.Object)
   */
  @Override
  public int compare( final IVegetationClass o1, final IVegetationClass o2 )
  {
    final String name1 = o1.getName();
    final String name2 = o2.getName();

    final Integer int1 = NumberUtils.parseQuietInteger( name1 );
    final Integer int2 = NumberUtils.parseQuietInteger( name2 );

    if( int1 != null && int2 != null )
      return int1.compareTo( int2 );

    return name1.compareTo( name2 );
  }
}