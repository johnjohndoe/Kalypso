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
package org.kalypso.ui.rrm.internal.simulations.runnables;

import java.util.Comparator;

import org.kalypso.model.hydrology.binding.control.NAControl;

/**
 * This comparator changes the order of the shortterm and longterm simulations. Longterm simulations should be first.
 * 
 * @author Holger Albert
 */
public class SimulationComparator implements Comparator<NAControl>
{
  /**
   * The constructor.
   */
  public SimulationComparator( )
  {
  }

  /**
   * @see java.util.Comparator#compare(java.lang.Object, java.lang.Object)
   */
  @Override
  public int compare( final NAControl c1, final NAControl c2 )
  {
    final Integer m1 = c1.getMinutesOfTimestep();
    final Integer m2 = c2.getMinutesOfTimestep();

    if( m1 != null && m2 == null )
      return -1;

    if( m1 == null && m2 == null )
      return 0;

    if( m1 == null && m2 != null )
      return 1;

    final int i1 = m1.intValue();
    final int i2 = m2.intValue();

    if( i1 == 1440 && i2 != 1440 )
      return -1;

    if( i1 != 1440 && i2 == 1440 )
      return 1;

    return 0;
  }
}