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
package org.kalypso.model.wspm.tuhh.ui.chart.layers;

import org.eclipse.swt.graphics.Point;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.core.profile.buildings.Buildings;
import org.kalypso.model.wspm.tuhh.core.profile.buildings.IProfileBuilding;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Envelope;

import de.openali.odysseus.chart.framework.model.data.IDataRange;
import de.openali.odysseus.chart.framework.model.data.impl.DataRange;
import de.openali.odysseus.chart.framework.model.figure.IFigure;
import de.openali.odysseus.chart.framework.model.figure.impl.EllipsisFigure;
import de.openali.odysseus.chart.framework.model.figure.impl.PolygonFigure;
import de.openali.odysseus.chart.framework.model.mapper.ICoordinateMapper;
import de.openali.odysseus.chart.framework.model.style.IAreaStyle;
import de.openali.odysseus.chart.framework.util.FigureUtilities;

/**
 * @author Gernot Belger
 */
class CulvertPainter
{
  private final IProfileBuilding m_tube;

  public CulvertPainter( final IProfileBuilding tube )
  {
    m_tube = tube;
  }

  private double getValue( final String property )
  {
    if( m_tube == null )
      return Double.NaN;

    return Buildings.getDoubleValueFor( property, m_tube );
  }

  private double getX( )
  {
    return getValue( IWspmTuhhConstants.BUILDING_PROPERTY_BEZUGSPUNKT_X );
  }

  private double getY( )
  {
    return getValue( IWspmTuhhConstants.BUILDING_PROPERTY_BEZUGSPUNKT_Y );
  }

  private double getWidth( )
  {
    return getValue( IWspmTuhhConstants.BUILDING_PROPERTY_BREITE );
  }

  private double getHeight( )
  {
    return getValue( IWspmTuhhConstants.BUILDING_PROPERTY_HOEHE );
  }

  private double getSlope( )
  {
    return getValue( IWspmTuhhConstants.BUILDING_PROPERTY_STEIGUNG );
  }

  IFigure<IAreaStyle> createFigure( final ICoordinateMapper cm )
  {
    if( m_tube == null )
      return null;

    final String tubeID = m_tube.getId();

    if( tubeID.equals( IWspmTuhhConstants.BUILDING_TYP_TRAPEZ ) )
      return paintTrapez( cm );

    if( tubeID.equals( IWspmTuhhConstants.BUILDING_TYP_KREIS ) || tubeID.equals( IWspmTuhhConstants.BUILDING_TYP_EI ) || tubeID.equals( IWspmTuhhConstants.BUILDING_TYP_MAUL ) )
      return paintEllipsis( cm );

    return null;
  }

  private IFigure<IAreaStyle> paintTrapez( final ICoordinateMapper cm )
  {
    final Coordinate[] points = calculateTrapezCoordinates();
    if( points == null )
      return null;

    final PolygonFigure polygonFigure = new PolygonFigure();

    final Point[] screenPoints = FigureUtilities.numericToScreen( cm, points );
    polygonFigure.setPoints( screenPoints );

    return polygonFigure;
  }

  private Coordinate[] calculateTrapezCoordinates( )
  {
    final double x = getX();
    final double y = getY();
    final double b = getWidth();
    final double h = getHeight();
    final double m = getSlope();

    if( Double.isNaN( x ) || Double.isNaN( y ) || Double.isNaN( b ) )
      return null;

    if( Double.isNaN( m ) || Double.isNaN( h ) )
      return null;

    final double leftX = x - b / 2;
    final double rightX = x + b / 2;

    final double lowerY = y;
    final double upperY = y + h;

    final double deltaX = m > 0 ? h / m : 0;

    final Coordinate[] points = new Coordinate[4];
    points[0] = new Coordinate( leftX, lowerY );
    points[1] = new Coordinate( leftX - deltaX, upperY );
    points[2] = new Coordinate( rightX + deltaX, upperY );
    points[3] = new Coordinate( rightX, lowerY );
    return points;
  }

  private Coordinate[] calculateEllipsisCoordinates( )
  {
    final double x = getX();
    final double y = getY();
    final double b = getWidth();
    final double h = getHeight();

    if( Double.isNaN( x ) || Double.isNaN( y ) || Double.isNaN( b ) )
      return null;

    final double leftX = x - b / 2;
// // FIXME: comment, why this strange isNaN test here, is this correct?
    final double upperY = y + (Double.isNaN( h ) ? b : h);
    final double rightX = x + b / 2;
    final double lowerY = y;

    final Coordinate[] points = new Coordinate[2];
    points[0] = new Coordinate( leftX, upperY );
    points[1] = new Coordinate( rightX, lowerY );

    return points;
  }

  private IFigure<IAreaStyle> paintEllipsis( final ICoordinateMapper cm )
  {
    final Coordinate[] points = calculateEllipsisCoordinates();
    if( points == null )
      return null;

    final Point[] screenPoints = FigureUtilities.numericToScreen( cm, points );

    final int leftX = screenPoints[0].x;
    final int upperY = screenPoints[0].y;
    final int rightX = screenPoints[1].x;
    final int lowerY = screenPoints[1].y;

    final int width = rightX - leftX;
    final int height = lowerY - upperY;

    return new EllipsisFigure( leftX, upperY, width, height );
  }

  public IDataRange<Number> getDomainRange( )
  {
    final Envelope box = getEnvelope();
    if( box == null )
      return null;

    return new DataRange<Number>( box.getMinX(), box.getMaxX() );
  }

  public IDataRange<Number> getTargetRange( )
  {
    final Envelope box = getEnvelope();
    if( box == null )
      return null;

    return new DataRange<Number>( box.getMinY(), box.getMaxY() );
  }

  private Envelope getEnvelope( )
  {
    final Coordinate[] crds = calculateCoordinates();
    if( crds == null )
      return null;

    final Envelope env = new Envelope();

    for( final Coordinate coordinate : crds )
      env.expandToInclude( coordinate );

    return env;
  }

  private Coordinate[] calculateCoordinates( )
  {
    if( m_tube == null )
      return null;

    final String tubID = m_tube.getId();

    if( tubID.equals( IWspmTuhhConstants.BUILDING_TYP_TRAPEZ ) )
      return calculateTrapezCoordinates();

    if( tubID.equals( IWspmTuhhConstants.BUILDING_TYP_KREIS ) || tubID.equals( IWspmTuhhConstants.BUILDING_TYP_EI ) || tubID.equals( IWspmTuhhConstants.BUILDING_TYP_MAUL ) )
      return calculateEllipsisCoordinates();

    return null;
  }
}