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
package org.kalypsodeegree_impl.graphics.displayelements;

import java.awt.Graphics;
import java.awt.Graphics2D;
import java.util.LinkedList;
import java.util.List;

import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree.model.geometry.GM_Triangle;
import org.kalypsodeegree.model.geometry.ISurfacePatchVisitor;
import org.kalypsodeegree_impl.graphics.sld.awt.StrokePainter;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

/**
 * Paints triangles as isolines.
 * 
 * @author Gernot Belger, Thomas Jung
 */
public class SurfacePaintIsolinesVisitor implements ISurfacePatchVisitor<GM_Triangle>
{
  private final Graphics m_gc;

  private final GeoTransform m_projection;

  private final ColorMapConverter m_colorModel;

  private static final double VAL_EPS = 0.0000001;

  public SurfacePaintIsolinesVisitor( final Graphics gc, final GeoTransform projection, final ColorMapConverter colorModel )
  {
    m_gc = gc;
    m_projection = projection;
    m_colorModel = colorModel;
  }

  /**
   * @see org.kalypsodeegree.model.geometry.ISurfacePatchVisitor#visit(org.kalypsodeegree.model.geometry.GM_SurfacePatch,
   *      double)
   */
  public boolean visit( final GM_Triangle triangle, final double elevationSample )
  {
    getTriangleIsoLines( triangle );

    return true;
  }

  private void getTriangleIsoLines( final GM_Triangle triangle )
  {
    // get value range of the triangle
    double minValue = Double.POSITIVE_INFINITY;
    double maxValue = Double.NEGATIVE_INFINITY;

    final GM_Position[] positions = triangle.getExteriorRing();

    for( final GM_Position position : positions )
    {
      if( position.getZ() < minValue )
        minValue = position.getZ();
      if( position.getZ() > maxValue )
        maxValue = position.getZ();
    }

    final int numOfClasses = m_colorModel.getNumOfClasses();

    /* loop over all classes */
    for( int currentClass = 0; currentClass < numOfClasses; currentClass++ )
    {
      /* aktuelles von und bis setzen */
      final double classValue = m_colorModel.getClassValue( currentClass );

      if( minValue <= classValue && classValue <= maxValue == true )
      {
        final List<GM_Position> posList = new LinkedList<GM_Position>();

        for( int j = 0; j < positions.length - 1; j++ )
        {
          GM_Position pos1 = positions[j];
          GM_Position pos2 = positions[j + 1];

          if( Math.abs( pos1.getZ() - classValue ) < VAL_EPS )
          {
            if( pos2.getZ() > classValue )
              pos1 = lowerPoint( pos1 );
          }
          if( Math.abs( pos2.getZ() - classValue ) < VAL_EPS )
          {
            if( pos1.getZ() > classValue )
              pos2 = lowerPoint( pos2 );
          }
          final GM_Position pos = interpolate( pos1, pos2, classValue );
          if( pos != null )
            posList.add( pos );
        }

        if( posList.size() == 2 )
        {

          paintIsoLine( posList.get( 0 ), posList.get( 1 ), currentClass );
        }
      }
    }
  }

  private GM_Position lowerPoint( final GM_Position position )
  {
    final double x = position.getX();
    final double y = position.getY();
    final double z = position.getZ();

    return GeometryFactory.createGM_Position( x, y, z - VAL_EPS );
  }

  private void paintIsoLine( final GM_Position position1, final GM_Position position2, final int currentClass )
  {
    final GM_Position screenPos1 = m_projection.getDestPoint( position1 );
    final GM_Position screenPos2 = m_projection.getDestPoint( position2 );

    final int[][] pos = new int[3][];
    pos[0] = new int[2];
    pos[1] = new int[2];
    pos[2] = new int[1];

    pos[0][0] = (int) screenPos1.getX();
    pos[1][0] = (int) screenPos1.getY();
    pos[0][1] = (int) screenPos2.getX();
    pos[1][1] = (int) screenPos2.getY();
    pos[2][0] = 2;

    final StrokePainter painter = m_colorModel.getLinePainter( currentClass );
    painter.paintPoses( (Graphics2D) m_gc, pos );
  }

  /**
   * Hat die Verbindung c1 - c2 einen Zwischenwert bei value, interpoliere die Zwischencoordinate Voraussetzung ist,
   * dass keine der Koordinaten exakt den Wert annimmt! nur kopiert... JA ich weiss...
   */
  private final GM_Position interpolate( final GM_Position c1, final GM_Position c2, final double value )
  {
    double zFaktor = -1.0;

    double z1 = c1.getZ();
    if( Double.isNaN( z1 ) )
    {
      z1 = -1000.0;
    }

    double z2 = c2.getZ();
    if( Double.isNaN( z2 ) )
    {
      z2 = -1000.0;
    }

    if( (z1 <= value && value < z2) || (z2 <= value && value < z1) )
      zFaktor = (value - z1) / (z2 - z1);

    if( zFaktor < 0 || zFaktor >= 1 )
      return null;

    final double x = zFaktor * (c2.getX() - c1.getX()) + c1.getX();
    final double y = zFaktor * (c2.getY() - c1.getY()) + c1.getY();
    return GeometryFactory.createGM_Position( x, y, value );
  }
}
