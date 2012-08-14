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
package org.kalypso.kalypsosimulationmodel.core.terrainmodel;

import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Point;

/**
 * Interface for classes that provide elevation information. The elevation can be get using
 * {@link #getElevation(GM_Point)}.
 *
 * @author Patrice Congo
 *
 */
public interface IElevationProvider
{
  /**
   * Get the elevation provides by this model for the specified location [(x,y) position].
   *
   * @param location
   *            the location for which an elevation is to be computed
   * @return the elevation if the model covered this position or NaN if not
   */
  double getElevation( GM_Point location );

  /**
   * To get the bounding box of this elevation provider
   *
   * @return the bounding box as {@link GM_Envelope}
   */
  GM_Envelope getBoundingBox( );

  /**
   * To get the minimal elavation in this elevation provider
   *
   * @return the minimal elevation of this provider as double
   */
  double getMinElevation( );

  /**
   * To get the maximal elavation in this elevation provider
   *
   * @return the maximal elevation of this provider as double
   */
  double getMaxElevation( );
}