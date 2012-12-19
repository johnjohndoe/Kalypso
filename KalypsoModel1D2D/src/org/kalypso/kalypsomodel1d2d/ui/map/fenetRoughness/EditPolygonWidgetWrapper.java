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
package org.kalypso.kalypsomodel1d2d.ui.map.fenetRoughness;

import java.awt.Point;
import java.util.ArrayList;
import java.util.List;

import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.command.Handle;
import org.kalypso.ogc.gml.map.widgets.EditGeometryWidget;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

/**
 * @author Stefan Kurzbach
 *
 *         Wraps an {@link EditGeometryWidget} for 0-argument constructor instantiation
 *
 */
public class EditPolygonWidgetWrapper extends EditGeometryWidget
{
  public EditPolygonWidgetWrapper( )
  {
    super( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.fenetRoughness.EditPolygonWidgetWrapper.0" ), "" ); //$NON-NLS-1$ //$NON-NLS-2$
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.EditGeometryWidget#filter(java.util.List, java.awt.Point, java.util.List)
   */
  @Override
  protected List<Handle> filter( final List<Handle> handles, final Point pointOfInterest, List<Handle> collector )
  {
    if( collector == null )
      collector = new ArrayList<>();

    final GeoTransform transform = getMapPanel().getProjection();
    final IKalypsoTheme activeTheme = getActiveTheme();
    if( activeTheme == null || !(activeTheme instanceof IKalypsoFeatureTheme) )
      return collector;

    final double gisX = transform.getSourceX( pointOfInterest.getX() );
    final double gisY = transform.getSourceY( pointOfInterest.getY() );
    final GM_Position positionOfInterest = GeometryFactory.createGM_Position( gisX, gisY );

    // 1. select nearest handle
    Handle nearest = null;
    double minDistance = -1;
    for( final Handle handle : handles )
    {
      final GM_Position handlePosition = handle.getPosition();
      final double pX = transform.getDestX( handlePosition.getX() );
      final double pY = transform.getDestY( handlePosition.getY() );
      // check if handle enclose pointOfInterest
      if( Math.abs( pX - pointOfInterest.getX() ) <= getBoxRadiusDrawnHandle()//
          && //
          Math.abs( pY - pointOfInterest.getY() ) <= getBoxRadiusDrawnHandle()//
      )
      {
        if( !collector.contains( handle ) )
          collector.add( handle );
        final double distance = positionOfInterest.getDistance( handlePosition );
        if( nearest == null || distance < minDistance )
        {
          minDistance = distance;
          nearest = handle;
        }
      }
    }
    return super.filter( handles, pointOfInterest, collector );
  }

}
