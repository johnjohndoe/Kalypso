/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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
package org.kalypso.ogc.gml.map.widgets.builders;

import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

/**
 * This class is a geometry builder for a MultiSurface. Currently it only supports building MultiSurfaces with a
 * single Surface geometry.
 * 
 * @author kurzbach
 */
public class MultiPolygonGeometryBuilder extends PolygonGeometryBuilder implements IGeometryBuilder
{
  /**
   * The constructor.
   * 
   * @param cnt_points
   *            If > 2 the the geometry will be finished, if the count of points is reached. If <= 2 no rule regarding
   *            the count of the points will apply, except, that a polygon needs at least 3 points for being created.
   * @param targetCrs
   *            The target coordinate system.
   */
  public MultiPolygonGeometryBuilder( final int cnt_points, final String targetCrs, final IGeometryBuilderExtensionProvider extender )
  {
    super( cnt_points, targetCrs, extender );
  }

  /**
   * The constructor.
   * 
   * @param cnt_points
   *            If > 2 the the geometry will be finished, if the count of points is reached. If <= 2 no rule regarding
   *            the count of the points will apply, except, that a polygon needs at least 3 points for beeing created.
   * @param targetCrs
   *            The target coordinate system.
   */
  public MultiPolygonGeometryBuilder( final int cnt_points, final String targetCrs )
  {
    this( cnt_points, targetCrs, null );
  }

  /**
   * @see org.kalypso.informdss.manager.util.widgets.IGeometryBuilder#finish()
   */
  @SuppressWarnings("unchecked")
  @Override
  public GM_Object finish( ) throws Exception
  {
    final GM_Surface result = (GM_Surface) super.finish();
    if( result != null )
    {
      return GeometryFactory.createGM_MultiSurface( new GM_Surface[] { result }, result.getCoordinateSystem() );
    }
    return null;
  }
}