/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and

 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de

 ---------------------------------------------------------------------------------------------------*/
package org.kalypso.ogc.gml.map.widgets;

import java.awt.Graphics;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.event.KeyEvent;

import org.apache.commons.lang.ArrayUtils;
import org.kalypso.ogc.gml.map.IMapPanel;
import org.kalypso.ogc.gml.map.utilities.MapUtilities;
import org.kalypso.ogc.gml.map.utilities.tooltip.ToolTipRenderer;
import org.kalypso.ogc.gml.map.widgets.builders.IGeometryBuilder;
import org.kalypso.ogc.gml.map.widgets.builders.LineGeometryBuilder;
import org.kalypso.ogc.gml.map.widgets.builders.PolygonGeometryBuilder;
import org.kalypso.ogc.gml.widgets.AbstractWidget;
import org.kalypsodeegree.KalypsoDeegreePlugin;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.geometry.GM_GenericCurve;
import org.kalypsodeegree.model.geometry.GM_GenericSurface;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Point;

/**
 * This tool measures distances or areas on the map.
 *
 * @author Gernot Belger
 */
public class MeasureMapWidget extends AbstractWidget
{
  private final String m_defaultCrs = KalypsoDeegreePlugin.getDefault().getCoordinateSystem();

  private final MeasureDelegate[] m_delegates = new MeasureDelegate[] { //
      new MeasureDelegate( new LineGeometryBuilder( 2, m_defaultCrs ), "2 Punkte", "Abstand" ), //
      new MeasureDelegate( new LineGeometryBuilder( 0, m_defaultCrs ), "Linie", "Gesamtlänge" ), //
      new MeasureDelegate( new PolygonGeometryBuilder( 0, m_defaultCrs ), "Polygon", "Fläche" ) //
  };

  private final static class MeasureDelegate
  {
    final IGeometryBuilder builder;

    final String label;

    final String sizeLabel;

    public MeasureDelegate( final IGeometryBuilder aBuilder, final String aLabel, final String aSizeLabel )
    {
      builder = aBuilder;
      label = aLabel;
      sizeLabel = aSizeLabel;
    }
  }

  private final ToolTipRenderer m_tooltip = new ToolTipRenderer();

  private MeasureDelegate m_delegate;

  private Point m_currentPoint;

  public MeasureMapWidget( )
  {
    this( "measure", "measure map distances" );
  }

  public MeasureMapWidget( final String name, final String toolTip )
  {
    super( name, toolTip );

    updateDelegate( 0 );
  }

  /**
   * Select to delegating widget and update tooltip
   */
  private void updateDelegate( final int index )
  {
    m_delegate = m_delegates[index];
    reset();
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractDelegateWidget#paint(java.awt.Graphics)
   */
  @Override
  public void paint( final Graphics g )
  {
    final IMapPanel mapPanel = getMapPanel();
    if( mapPanel == null )
      return;

    final GeoTransform projection = mapPanel.getProjection();
    m_delegate.builder.paint( g, projection, m_currentPoint );

    final Rectangle bounds = mapPanel.getScreenBounds();
    if( m_currentPoint != null )
      m_tooltip.paintToolTip( m_currentPoint, g, bounds );
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractDelegateWidget#keyPressed(java.awt.event.KeyEvent)
   */
  @Override
  public void keyPressed( final KeyEvent e )
  {
    if( e.getKeyCode() == KeyEvent.VK_SPACE )
    {
      e.consume();

      final int currentIndex = ArrayUtils.indexOf( m_delegates, m_delegate );
      final int newIndex = (currentIndex + 1) % m_delegates.length;
      updateDelegate( newIndex );

      getMapPanel().repaintMap();
      return;
    }

    if( e.getKeyCode() == KeyEvent.VK_ESCAPE )
    {
      e.consume();

      reset();
      getMapPanel().repaintMap();
      return;
    }

    super.keyPressed( e );
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.AbstractWidget#dragged(java.awt.Point)
   */
  @Override
  public void dragged( final Point p )
  {
    moved( p );
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#moved(java.awt.Point)
   */
  @Override
  public void moved( final Point p )
  {
    try
    {
      m_currentPoint = p;

      final IMapPanel mapPanel = getMapPanel();
      if( mapPanel == null )
        return;

      final GM_Point currentPoint = MapUtilities.transform( mapPanel, p );
      updateTooltip( currentPoint );

      mapPanel.repaintMap();
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }
  }

  private void updateTooltip( final GM_Point currentPoint ) throws Exception
  {
    m_delegate.builder.addPoint( currentPoint );
    final GM_Object geometry = m_delegate.builder.finish();
    m_delegate.builder.removeLastPoint();

    // as long as the user has not clicked yet, show modus tooltip
    // Would even be nicer, if we would test for count of added points, but this is missing in IGeometryBuilder
    // interface
    if( geometry != null )
    {
      final double size = calcSize( geometry );

      /* update tooltip */
      if( Double.isNaN( size ) )
        m_tooltip.setTooltip( String.format( "x = %.2f, y = %.2f", currentPoint.getX(), currentPoint.getY() ) );
      else
        m_tooltip.setTooltip( String.format( "%s:\t\t%.2f", m_delegate.sizeLabel, size ) );
    }
  }

  private double calcSize( final GM_Object geometry )
  {
    if( geometry instanceof GM_GenericCurve )
      return ((GM_GenericCurve) geometry).getLength();

    if( geometry instanceof GM_GenericSurface )
      return ((GM_GenericSurface) geometry).getArea();

    return Double.NaN;
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.AbstractWidget#leftPressed(java.awt.Point)
   */
  @Override
  public void leftPressed( final Point p )
  {
    try
    {
      final IMapPanel mapPanel = getMapPanel();
      if( mapPanel == null )
        return;

      final GM_Point currentPoint = MapUtilities.transform( mapPanel, p );
      final GM_Object geometry = m_delegate.builder.addPoint( currentPoint );

      if( geometry != null )
      {
        reset();
        m_delegate.builder.addPoint( currentPoint );
      }

      updateTooltip( currentPoint );

      mapPanel.repaintMap();
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#doubleClickedLeft(java.awt.Point)
   */
  @Override
  public void doubleClickedLeft( final Point p )
  {
    reset();
    getMapPanel().repaintMap();
  }

  private void reset( )
  {
    m_delegate.builder.reset();
    m_tooltip.setTooltip( m_delegate.label + " ('Leertaste' wechselt)" );
  }
}