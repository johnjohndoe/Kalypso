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
import org.kalypso.commons.java.lang.Doubles;
import org.kalypso.commons.java.lang.Objects;
import org.kalypso.model.wspm.tuhh.core.profile.profileobjects.building.BuildingEi;
import org.kalypso.model.wspm.tuhh.core.profile.profileobjects.building.BuildingKreis;
import org.kalypso.model.wspm.tuhh.core.profile.profileobjects.building.BuildingMaul;
import org.kalypso.model.wspm.tuhh.core.profile.profileobjects.building.BuildingTrapez;
import org.kalypso.model.wspm.tuhh.core.profile.profileobjects.building.ICulvertBuilding;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Envelope;

import de.openali.odysseus.chart.framework.model.data.DataRange;
import de.openali.odysseus.chart.framework.model.data.IDataRange;
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
  private final ICulvertBuilding m_tube;

  public CulvertPainter( final ICulvertBuilding tube )
  {
    m_tube = tube;
  }

  @SuppressWarnings( "rawtypes" )
  IFigure<IAreaStyle> createFigure( final ICoordinateMapper cm )
  {
    if( m_tube == null )
      return null;

    if( m_tube instanceof BuildingTrapez )
      return paintTrapez( (BuildingTrapez)m_tube, cm );

    if( m_tube instanceof BuildingKreis || m_tube instanceof BuildingEi || m_tube instanceof BuildingMaul )
      return paintEllipsis( m_tube, cm );

    return null;
  }

  @SuppressWarnings( "rawtypes" )
  private IFigure<IAreaStyle> paintTrapez( final BuildingTrapez trapezBuilding, final ICoordinateMapper cm )
  {
    final Coordinate[] points = calculateTrapezCoordinates( trapezBuilding );
    if( points == null )
      return null;

    final PolygonFigure polygonFigure = new PolygonFigure();

    final Point[] screenPoints = FigureUtilities.numericToScreen( cm, points );
    polygonFigure.setPoints( screenPoints );

    return polygonFigure;
  }

  private Coordinate[] calculateTrapezCoordinates( final BuildingTrapez trapezBuilding )
  {
    final Double x = trapezBuilding.getBezugspunktX();
    final Double y = trapezBuilding.getBezugspunktY();
    final Double b = trapezBuilding.getBreite();
    final Double h = trapezBuilding.getHoehe();
    final Double m = trapezBuilding.getSteigung();

    if( Objects.isNull( x, y, b, h, m ) )
      return null;

    if( Doubles.isNaN( x, y, b, h, m ) )
      return null;

    final double leftX = x - b / 2;
    final double upperY = y + h;
    final double rightX = x + b / 2;
    final double lowerY = y;
    final double deltaX = m > 0 ? h * m : 0;

    final Coordinate[] points = new Coordinate[4];
    points[0] = new Coordinate( leftX, lowerY );
    points[1] = new Coordinate( leftX - deltaX, upperY );
    points[2] = new Coordinate( rightX + deltaX, upperY );
    points[3] = new Coordinate( rightX, lowerY );
    return points;
  }

  private Coordinate[] calculateEllipsisCoordinates( final ICulvertBuilding building )
  {
    final Double x = building.getBezugspunktX();
    final Double y = building.getBezugspunktY();
    final Double b = building.getBreite();

    if( org.kalypso.commons.java.lang.Objects.isNull( x, y, b ) )
      return null;

    if( Doubles.isNaN( x, y, b ) )
      return null;

    final double h = fetchHeight( building, b );

    final double leftX = x - b / 2;
    final double upperY = y + h;
    final double rightX = x + b / 2;
    final double lowerY = y;

    final Coordinate[] points = new Coordinate[2];
    points[0] = new Coordinate( leftX, upperY );
    points[1] = new Coordinate( rightX, lowerY );

    return points;
  }

  private double fetchHeight( final ICulvertBuilding building, final Double b )
  {
    if( building instanceof BuildingEi )
    {
      final Double hoehe = ((BuildingEi)building).getHoehe();
      if( hoehe != null )
        return hoehe.doubleValue();
    }

    if( building instanceof BuildingMaul )
    {
      final Double hoehe = ((BuildingMaul)building).getHoehe();
      if( hoehe != null )
        return hoehe.doubleValue();
    }

    // REMARK: We use the width as height, if there is no height in the building.
    return b.doubleValue();
  }

  @SuppressWarnings( "rawtypes" )
  private IFigure<IAreaStyle> paintEllipsis( final ICulvertBuilding building, final ICoordinateMapper cm )
  {
    final Coordinate[] points = calculateEllipsisCoordinates( building );
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

  @SuppressWarnings( { "unchecked", "rawtypes" } )
  public IDataRange<Double> getDomainRange( )
  {
    final Envelope box = getEnvelope();
    if( box == null )
      return null;

    return new DataRange(box.getMinX(), box.getMaxX() );
  }

  @SuppressWarnings( { "unchecked", "rawtypes" } )
  public IDataRange<Double> getTargetRange( )
  {
    final Envelope box = getEnvelope();
    if( box == null )
      return null;

    return new DataRange(box.getMinY(),box.getMaxY());
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

    if( m_tube instanceof BuildingTrapez )
      return calculateTrapezCoordinates( (BuildingTrapez)m_tube );

    if( m_tube instanceof BuildingKreis || m_tube instanceof BuildingEi || m_tube instanceof BuildingMaul )
      return calculateEllipsisCoordinates( m_tube );

    return null;
  }
}