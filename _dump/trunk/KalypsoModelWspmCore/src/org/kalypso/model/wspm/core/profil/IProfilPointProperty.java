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
package org.kalypso.model.wspm.core.profil;

/**
 * @author kimwerner
 */
public interface IProfilPointProperty
{
  /**
   * @return the smallest doubleValue two values can differ(i.e. 0.00010)
   */
  public double getPrecision( );

  /**
   * @return false if this property is not removeable from profile
   */
  public boolean isOptional( );

  /**
   * @return the doubleValue a new profilePoint should have for this property,
   *         <p>
   *         when added between two other profilePoints
   * @param breite
   *          the absolute position between the two points(i.e  {@link org.kalypso.model.wspm.core.IWspmConstants#POINT_PROPERTY_BREITE})
   * @throws IllegalArgumentException
   *           if {@link org.kalypso.model.wspm.core.IWspmConstants#POINT_PROPERTY_BREITE} is not available in both
   *           points
   */
  public double doInterpolation( final IProfilPoint point1, final IProfilPoint point2, final double breite );

  /**
   * @return all ProfilePointPropertyIds this Property depends on
   */
  public String[] getDependencies( );

  /**
   * the id for this ProfilepointProperty should be the same used in the dictonary {@link IWspmConstants}
   */
  public String getId( );

  /**
   * @return a friendly name for this ProfilepointProperty should be the same used in the dictonary
   *         {@link IWspmConstants}
   */
  public String getLabel( );
}
