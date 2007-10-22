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
package org.kalypso.kalypsomodel1d2d.ui.map.channeledit;

import java.awt.Color;
import java.awt.Graphics;
import java.awt.Point;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import org.eclipse.core.runtime.NullProgressMonitor;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.map.utilities.MapUtilities;
import org.kalypso.ogc.gml.map.widgets.AbstractWidget;
import org.kalypso.ogc.gml.map.widgets.providers.handles.Handle;
import org.kalypso.ogc.gml.map.widgets.providers.handles.IHandle;
import org.kalypsodeegree.graphics.displayelements.DisplayElement;
import org.kalypsodeegree.graphics.sld.LineSymbolizer;
import org.kalypsodeegree.graphics.sld.Stroke;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_CurveSegment;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.graphics.displayelements.DisplayElementFactory;
import org.kalypsodeegree_impl.graphics.sld.LineSymbolizer_Impl;
import org.kalypsodeegree_impl.graphics.sld.Stroke_Impl;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

import com.vividsolutions.jts.geom.LineString;

/**
 * @author jung
 */
public class DragBankLineWidget extends AbstractWidget
{
  /**
   * This list stores all handles of the selected feature.
   */
  private List<IHandle> m_handles;

  /**
   * The radius, in which a handle should be selectable.
   */
  private final int m_radius;

  /**
   * The start point, if dragged.
   */
  private Point m_startPoint;

  /**
   * The current point, if dragged, otherwise null.
   */
  private Point m_currentPoint;

  /**
   * The bankline.
   */
  private GM_Curve m_bankline;

  /**
   * The new curve.
   */
  private GM_Curve m_newCurve;

  /**
   * The side which is changed.
   */
  private final int m_side;

  /**
   * The current segment.
   */
  private final SegmentData m_currentSegment;

  private final CreateChannelData m_data;

  public DragBankLineWidget( final CreateChannelData channeldata, final SegmentData currentSegment, final int side ) throws GM_Exception
  {
    super( "Uferline", "Dieses Widget ermöglicht das Verändern der Uferlinie." );

    if( side == 1 )
      m_bankline = (GM_Curve) JTSAdapter.wrap( currentSegment.getBankLeftInters() );
    else
      m_bankline = (GM_Curve) JTSAdapter.wrap( currentSegment.getBankRightInters() );

    m_data = channeldata;
    m_currentSegment = currentSegment;
    m_side = side;
    m_newCurve = null;
    m_handles = null;
    m_radius = 4;

    /* The start & current points are null. */
    m_startPoint = null;
    m_currentPoint = null;

    /* Create a new list for the handles. */
    m_handles = new ArrayList<IHandle>();

    /* Collect all handles from the handle provider. */
    m_handles.addAll( collectHandles() );
    m_handles.remove( 0 );
    m_handles.remove( m_handles.size() - 1 );
  }

  private List<IHandle> collectHandles( )
  {
    ArrayList<IHandle> list = null;

    try
    {
      list = new ArrayList<IHandle>();

      final int numberOfCurveSegments = m_bankline.getNumberOfCurveSegments();

      for( int i = 0; i < numberOfCurveSegments; i++ )
      {
        /* One segment of a curve. It can contain several points. */
        final GM_CurveSegment curveSegment = m_bankline.getCurveSegmentAt( i );

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

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#dragged(java.awt.Point)
   */
  @Override
  public void dragged( final Point p )
  {
    if( m_handles == null )
    {
      // TODO: check if this repaint is really necessary
      final MapPanel panel = getMapPanel();
      if( panel != null )
        panel.repaint();
      return;

    }

    if( m_startPoint == null )
    {
      /* Store the start point. */
      m_startPoint = p;

      /* Check, if the mouse cursor is near some handles. */
      for( final IHandle handle : m_handles )
      {
        /* If the curser is near the handle, set it active, otherwise inactive. */
        if( handle.isSelectable( p, getMapPanel().getProjection() ) )
          handle.setActive( true );
        else
          handle.setActive( false );
      }
    }

    /* Store the current mouse position. */
    m_currentPoint = p;

    // TODO: check if this repaint is really necessary
    final MapPanel panel = getMapPanel();
    if( panel != null )
      panel.repaint();
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#finish()
   */
  @Override
  public void finish( )
  {
    /* Reset the widget. */
    reset();

    /* The parents finish method. */
    super.finish();
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#leftReleased(java.awt.Point)
   */
  @Override
  public void leftReleased( final Point p )
  {
    if( (m_handles == null) || (m_startPoint == null) || (m_currentPoint == null) )
      return;

    /* Set all handles inactive. */
    for( final IHandle handle : m_handles )
      handle.setActive( false );

    if( m_newCurve == null )
    {
      /* Reset. */
      m_currentPoint = null;
      m_startPoint = null;

      final MapPanel panel = getMapPanel();
      if( panel != null )
        panel.repaint();

      return;
    }

    try
    {
      /* Create new geometry. */
      if( m_side == 1 )
      {
        m_currentSegment.setBankLeftInters( (LineString) JTSAdapter.export( m_newCurve ) );
        m_currentSegment.setBankLeftOrg( (LineString) JTSAdapter.export( m_newCurve ) );
        m_data.updateSegments( true );

      }
      else
      {
        m_currentSegment.setBankRightInters( (LineString) JTSAdapter.export( m_newCurve ) );
        m_currentSegment.setBankRightOrg( (LineString) JTSAdapter.export( m_newCurve ) );
        m_data.updateSegments( true );
      }

      m_bankline = m_newCurve;
      m_newCurve = null;
      m_handles = collectHandles();
      m_handles.remove( 0 );
      m_handles.remove( m_handles.size() - 1 );
    }
    catch( final GM_Exception e )
    {
      e.printStackTrace();
    }

    /* Reset. */
    m_currentPoint = null;
    m_startPoint = null;

    final MapPanel panel = getMapPanel();
    if( panel != null )
      panel.repaint();
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#paint(java.awt.Graphics)
   */
  @Override
  public void paint( final Graphics g )
  {
    if( m_handles == null || m_bankline == null )
      return;

    // TODO: maybe it would be nice if the user is shown an arrow from the old point to the new dragged point.

    // final GM_Point[] linepoints = new GM_Point[m_handles.size() + 2]; //number of handles plus start and end point
    final GM_Position[] positions = new GM_Position[m_handles.size() + 2]; // number of handles plus start and end
    // point
    int i = 0;
    positions[i] = m_bankline.getStartPoint().getPosition();

    /* Paint all handles. */
    for( final IHandle handle : m_handles )
    {
      i = i + 1;
      if( handle.isActive() )
      {
        handle.paint( g, getMapPanel().getProjection(), m_startPoint, m_currentPoint );

        if( m_currentPoint != null )
          positions[i] = MapUtilities.transform( getMapPanel(), m_currentPoint ).getPosition();
        else
        {
          final GM_Position position = handle.getPosition();
          positions[i] = GeometryFactory.createGM_Point( position, m_bankline.getCoordinateSystem() ).getPosition();
        }
      }
      else
      {
        handle.paint( g, getMapPanel().getProjection(), null, null );

        final GM_Position position = handle.getPosition();
        positions[i] = GeometryFactory.createGM_Point( position, m_bankline.getCoordinateSystem() ).getPosition();
      }
    }
    i = i + 1;
    positions[i] = m_bankline.getEndPoint().getPosition();

    // paint the current dragged line
    final LineSymbolizer symb = new LineSymbolizer_Impl();
    final Stroke stroke = new Stroke_Impl( new HashMap(), null, null );

    final GM_Curve curve;
    try
    {
      curve = GeometryFactory.createGM_Curve( positions, m_bankline.getCoordinateSystem() );
      Stroke defaultstroke = new Stroke_Impl( new HashMap(), null, null );
      defaultstroke = symb.getStroke();

      stroke.setWidth( 2 );
      final Color color = new Color( 30, 255, 255 );
      stroke.setStroke( color );
      symb.setStroke( stroke );

      final DisplayElement de = DisplayElementFactory.buildLineStringDisplayElement( null, curve, symb );
      de.paint( g, getMapPanel().getProjection(), new NullProgressMonitor() );

      // Set the Stroke back to default
      symb.setStroke( defaultstroke );

      m_newCurve = curve;
    }
    catch( final Exception e1 )
    {
      // TODO Auto-generated catch block
      e1.printStackTrace();
    }

  }

  /**
   * Resets the widget.
   */
  public void reset( )
  {
    // m_bankline = null;
    m_newCurve = null;

    /* Reset the start & current points. */
    m_startPoint = null;
    m_currentPoint = null;
  }
}