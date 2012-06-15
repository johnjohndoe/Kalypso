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
package org.kalypso.kalypsomodel1d2d.ui.map.channeledit;

import java.awt.Color;
import java.awt.Cursor;
import java.awt.Graphics;
import java.awt.Point;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.kalypso.kalypsomodel1d2d.ui.map.util.GM_PointSnapper;
import org.kalypso.ogc.gml.map.IMapPanel;
import org.kalypso.ogc.gml.map.utilities.MapUtilities;
import org.kalypso.ogc.gml.map.widgets.providers.handles.Handle;
import org.kalypso.ogc.gml.map.widgets.providers.handles.IHandle;
import org.kalypsodeegree.graphics.displayelements.DisplayElement;
import org.kalypsodeegree.graphics.sld.Fill;
import org.kalypsodeegree.graphics.sld.Graphic;
import org.kalypsodeegree.graphics.sld.LineSymbolizer;
import org.kalypsodeegree.graphics.sld.Mark;
import org.kalypsodeegree.graphics.sld.PointSymbolizer;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_CurveSegment;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.graphics.displayelements.DisplayElementFactory;
import org.kalypsodeegree_impl.graphics.sld.LineSymbolizer_Impl;
import org.kalypsodeegree_impl.graphics.sld.PointSymbolizer_Impl;
import org.kalypsodeegree_impl.graphics.sld.Stroke_Impl;
import org.kalypsodeegree_impl.graphics.sld.StyleFactory;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

/**
 * @author Thomas Jung
 */
public class LineGeometryEditor
{
  private final Map<GM_Position, GM_Curve> m_positionMap = new HashMap<GM_Position, GM_Curve>();

  final java.awt.Cursor CROSSHAIR_CURSOR = java.awt.Cursor.getPredefinedCursor( Cursor.CROSSHAIR_CURSOR );

  final java.awt.Cursor DEFAULT_CURSOR = java.awt.Cursor.getPredefinedCursor( Cursor.DEFAULT_CURSOR );

  private final GM_Curve[] m_curves;

  private GM_PointSnapper m_pointSnapper = null;

  private GM_Curve m_curve = null;

  private ArrayList<IHandle> m_handles;

  private final int m_radius = 10;

  private Object m_startPoint;

  private Point m_currentPoint;

  private final IMapPanel m_mapPanel;

  private GM_Curve m_currentCurve;

  public LineGeometryEditor( final GM_Curve[] curves, final IMapPanel panel )
  {
    m_curves = curves;
    m_mapPanel = panel;

    m_startPoint = null;

    reinit();
  }

  private void reinit( )
  {
    final GM_Position[] positions = MainChannelHelper.getPositionsFromCurves( m_curves, m_positionMap );
    m_pointSnapper = new GM_PointSnapper( positions, m_mapPanel, m_curves[0].getCoordinateSystem() );
  }

  public void moved( final GM_Point p )
  {
    m_startPoint = null;

    final GM_Point movedPoint = m_pointSnapper.moved( p );
    if( movedPoint != null )
    {
      m_curve = m_positionMap.get( movedPoint.getPosition() );

      m_handles = collectHandles();
      m_mapPanel.setCursor( Cursor.getPredefinedCursor( Cursor.HAND_CURSOR ) );

    }
    else
      m_mapPanel.setCursor( Cursor.getPredefinedCursor( Cursor.DEFAULT_CURSOR ) );
  }

  private ArrayList<IHandle> collectHandles( )
  {
    final ArrayList<IHandle> list = new ArrayList<IHandle>();

    if( m_curve == null )
      return list;

    try
    {
      final int numberOfCurveSegments = m_curve.getNumberOfCurveSegments();

      for( int i = 0; i < numberOfCurveSegments; i++ )
      {
        /* One segment of a curve. It can contain several points. */
        final GM_CurveSegment curveSegment = m_curve.getCurveSegmentAt( i );

        final GM_Position[] positions = curveSegment.getPositions();
        for( final GM_Position position : positions )
        {
          /* Add the points of every segment to the list of handles. */
          list.add( new Handle( position, null, null, m_radius ) );
        }
      }
    }
    catch( final GM_Exception e )
    {
      e.printStackTrace();
    }

    return list;
  }

  public void paint( final Graphics g )
  {
    if( m_curve != null )
    {
      if( m_handles == null )
        return;

      final GM_Position[] positions = new GM_Position[m_handles.size()]; // number of handles plus start and end
      // point

      /* Paint all handles. */
      for( int i = 0; i < m_handles.size(); i++ )
      {
        final IHandle handle = m_handles.get( i );

        if( handle.isActive() )
        {
          if( m_currentPoint != null )
            positions[i] = MapUtilities.transform( m_mapPanel, m_currentPoint ).getPosition();
          else
          {
            final GM_Position position = handle.getPosition();
            positions[i] = GeometryFactory.createGM_Point( position, m_curve.getCoordinateSystem() ).getPosition();
          }
        }
        else
        {
          final GM_Position position = handle.getPosition();
          positions[i] = GeometryFactory.createGM_Point( position, m_curve.getCoordinateSystem() ).getPosition();
        }
      }
      //

      // paint line
      final LineSymbolizer symb = new LineSymbolizer_Impl();
      final org.kalypsodeegree.graphics.sld.Stroke stroke = new Stroke_Impl( new HashMap(), null, null );

      try
      {
        final GM_Curve curve = GeometryFactory.createGM_Curve( positions, m_curve.getCoordinateSystem() );
        org.kalypsodeegree.graphics.sld.Stroke defaultstroke = new Stroke_Impl( new HashMap(), null, null );
        defaultstroke = symb.getStroke();

        stroke.setWidth( 2 );
        final Color color = new Color( 30, 255, 255 );
        stroke.setStroke( color );
        symb.setStroke( stroke );

        final DisplayElement de = DisplayElementFactory.buildLineStringDisplayElement( null, curve, symb );
        de.paint( g, m_mapPanel.getProjection(), new NullProgressMonitor() );

        // Set the Stroke back to default
        symb.setStroke( defaultstroke );

        // paint points
        final List<GM_Point> points = MainChannelHelper.getPointsFromCurve( curve );
        final PointSymbolizer pointSymb = new PointSymbolizer_Impl();

        final Fill fill = StyleFactory.createFill( color );
        final Mark mark = StyleFactory.createMark( "square", fill, stroke ); //$NON-NLS-1$
        final Graphic graphic = StyleFactory.createGraphic( null, mark, 1, 5, 0 );

        pointSymb.setGraphic( graphic );
        final DisplayElement de2 = DisplayElementFactory.buildPointDisplayElement( null, points, pointSymb );
        if( de2 != null )
          de2.paint( g, m_mapPanel.getProjection(), new NullProgressMonitor() );

        m_currentCurve = curve;
      }
      catch( final CoreException e )
      {
        // TODO Auto-generated catch block
        e.printStackTrace();
      }
      catch( final GM_Exception e )
      {
        // TODO Auto-generated catch block
        e.printStackTrace();
      }

      m_pointSnapper.paint( g );
    }
  }

  public void dragged( final Point p, final IMapPanel mapPanel )
  {
    if( m_startPoint == null )
    {
      /* Store the start point. */
      m_startPoint = p;

      if( m_handles == null )
        return;

      /* Check, if the mouse cursor is near some handles. */
      for( final IHandle handle : m_handles )
      {
        /* If the cursor is near the handle, set it active, otherwise inactive. */
        if( handle.isSelectable( p, mapPanel.getProjection() ) )
          handle.setActive( true );
        else
          handle.setActive( false );
      }
    }

    /* Store the current mouse position. */
    m_currentPoint = p;

    // TODO: check if this repaint is really necessary
    if( mapPanel != null )
      mapPanel.repaintMap();
  }

  public GM_Curve finish( )
  {
    m_startPoint = null;
    setCurve();
    reinit();
    return m_currentCurve;
  }

  private void setCurve( )
  {
    for( int i = 0; i < m_curves.length; i++ )
    {
      final GM_Curve curve = m_curves[i];
      if( curve.equals( m_curve ) )
        m_curves[i] = m_currentCurve;
    }
  }
}
