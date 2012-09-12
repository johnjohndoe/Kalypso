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

import java.awt.Cursor;
import java.awt.Graphics;
import java.awt.Point;
import java.awt.event.MouseEvent;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.ui.map.channeledit.editdata.IBankData;
import org.kalypso.kalypsomodel1d2d.ui.map.channeledit.editdata.ISegmentData;
import org.kalypso.kalypsomodel1d2d.ui.map.util.GM_PointSnapper;
import org.kalypso.ogc.gml.IKalypsoLayerModell;
import org.kalypso.ogc.gml.map.IMapPanel;
import org.kalypso.ogc.gml.map.utilities.MapUtilities;
import org.kalypso.ogc.gml.map.widgets.advanced.utils.SLDPainter2;
import org.kalypso.ogc.gml.map.widgets.providers.handles.Handle;
import org.kalypso.ogc.gml.map.widgets.providers.handles.IHandle;
import org.kalypso.ogc.gml.widgets.AbstractWidget;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.LineString;

/**
 * @author Thomas Jung
 */
class DragBankLineWidget extends AbstractWidget
{
  private final SLDPainter2 m_handlePainter = new SLDPainter2( getClass().getResource( "resources/handleRect.sld" ) ); //$NON-NLS-1$

  private final SLDPainter2 m_currentLinePainter = new SLDPainter2( getClass().getResource( "resources/draggedBankLine.sld" ) ); //$NON-NLS-1$

  /**
   * This list stores all handles of the selected feature.
   */
  private final Map<IHandle, IBankData> m_handles = new HashMap<>();

  /**
   * The radius, in which a handle should be selectable.
   */
  private final int m_radius = 10;

  /**
   * The start point, if dragged.
   */
  private IHandle m_draggedHandle;

  /**
   * The current point, if dragged, otherwise null.
   */
  private Point m_currentPoint;

  /**
   * The new curve.
   */
  private GM_Curve m_currentCurve;

  private final ChannelEditData m_data;

  private GM_PointSnapper m_pointSnapper;

  public DragBankLineWidget( final ChannelEditData channeldata )
  {
    super( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.channeledit.DragBankLineWidget.0" ), Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.channeledit.DragBankLineWidget.1" ) ); //$NON-NLS-1$ //$NON-NLS-2$

    m_data = channeldata;

    reset();
  }

  @Override
  public void activate( final ICommandTarget commandPoster, final IMapPanel mapPanel )
  {
    super.activate( commandPoster, mapPanel );

    reset();
  }

  @Override
  public void finish( )
  {
    /* Reset the widget. */
    reset();

    /* The parents finish method. */
    super.finish();
  }

  @Override
  public void mouseMoved( final MouseEvent event )
  {
    final IMapPanel mapPanel = getMapPanel();
    if( mapPanel == null )
      return;

    if( m_pointSnapper == null || m_draggedHandle != null )
      return;

    final Point p = event.getPoint();

    final GM_Point currentPoint = MapUtilities.transform( mapPanel, p );
    final GM_Point snappedPoint = m_pointSnapper.moved( currentPoint );

    if( snappedPoint == null )
      getMapPanel().setCursor( Cursor.getPredefinedCursor( Cursor.DEFAULT_CURSOR ) );
    else
      getMapPanel().setCursor( Cursor.getPredefinedCursor( Cursor.HAND_CURSOR ) );

    repaintMap();
  }

  @Override
  public void mouseDragged( final MouseEvent event )
  {
    final Point point = event.getPoint();

    final IMapPanel mapPanel = getMapPanel();
    if( mapPanel == null )
      return;

    if( m_draggedHandle == null )
      return;

    /* Store the current mouse position. */
    m_currentPoint = point;

    final GM_Position oldPosition = m_draggedHandle.getPosition();

    /* Create new geometry. */
    final GM_Point newPoint = MapUtilities.transform( mapPanel, point );

    final String mapSRS = mapPanel.getMapModell().getCoordinatesSystem();

    try
    {
      final IBankData bank = m_handles.get( m_draggedHandle );
      m_currentCurve = updateCurrentCurve( bank, oldPosition, newPoint.getPosition(), mapSRS );
    }
    catch( final GM_Exception e )
    {
      e.printStackTrace();
    }

    repaintMap();
  }

  private GM_Curve updateCurrentCurve( final IBankData bank, final GM_Position oldPosition, final GM_Position newPosition, final String mapSRS ) throws GM_Exception
  {
    final Coordinate handleCrd = JTSAdapter.export( oldPosition );
    final Coordinate newCrd = JTSAdapter.export( newPosition );

    final LineString segmented = bank.getWorkingGeometry();

    final Coordinate[] oldCoordinates = segmented.getCoordinates();
    final Coordinate[] newCoordinates = new Coordinate[oldCoordinates.length];

    for( int i = 0; i < oldCoordinates.length; i++ )
    {
      final Coordinate oldCrd = oldCoordinates[i];

      if( oldCrd.distance( handleCrd ) < 0.001 )
      {
        newCoordinates[i] = newCrd;
        newCoordinates[i].z = oldCrd.z;
      }
      else
        newCoordinates[i] = oldCrd;
    }

    final GM_Position[] newPositions = JTSAdapter.wrap( newCoordinates );
    return GeometryFactory.createGM_Curve( newPositions, mapSRS );
  }

  private IHandle findHandle( final Point point, final GeoTransform projection )
  {
    /* Check, if the mouse cursor is near some handles. */
    for( final IHandle handle : m_handles.keySet() )
    {
      /* If the cursor is near the handle, set it active, otherwise inactive. */
      if( handle.isSelectable( point, projection ) )
        return handle;
    }

    return null;
  }

  @Override
  public void mousePressed( final MouseEvent event )
  {
    if( event.getButton() != MouseEvent.BUTTON1 )
      return;
    event.consume();

    final Point point = event.getPoint();

    final IMapPanel mapPanel = getMapPanel();
    if( mapPanel == null )
      return;

    final GeoTransform projection = mapPanel.getProjection();

    if( m_draggedHandle == null )
      m_draggedHandle = findHandle( point, projection );
  }

  @Override
  public void mouseReleased( final MouseEvent event )
  {
    if( event.getButton() != MouseEvent.BUTTON1 )
      return;
    event.consume();

    if( m_draggedHandle == null || m_currentCurve == null )
      return;

    try
    {
      final LineString currentLine = (LineString)JTSAdapter.export( m_currentCurve );

      final IBankData bank = m_handles.get( m_draggedHandle );
      final ISegmentData segment = bank.getSegment();
      segment.updateWorkingGeometry( bank, currentLine );
    }
    catch( final GM_Exception e )
    {
      e.printStackTrace();
    }

    reset();
  }

  @Override
  public void paint( final Graphics g )
  {
    final IMapPanel mapPanel = getMapPanel();
    if( mapPanel == null )
      return;

    final GeoTransform projection = mapPanel.getProjection();

    final IKalypsoLayerModell mapModell = mapPanel.getMapModell();
    if( mapModell == null )
      return;

    final String mapSRS = mapModell.getCoordinatesSystem();

    final IBankData draggedBank = m_handles.get( m_draggedHandle );

    /* Paint all banks */
    for( final IBankData bank : m_handles.values() )
    {
      if( bank != draggedBank )
      {
        try
        {
          final GM_Curve bankCurve = (GM_Curve)JTSAdapter.wrap( bank.getWorkingGeometry(), mapSRS );
          m_currentLinePainter.paint( g, projection, bankCurve );
        }
        catch( final GM_Exception e )
        {
          e.printStackTrace();
        }
      }
    }

    /* Paint all handles. */
    for( final IHandle handle : m_handles.keySet() )
    {
      if( handle != m_draggedHandle )
      {
        final GM_Position handlePos = handle.getPosition();
        final GM_Point handlePoint = GeometryFactory.createGM_Point( handlePos, mapSRS );

        m_handlePainter.paint( g, projection, handlePoint );
      }
    }

    // paint the current dragged line
    if( m_currentCurve != null )
      m_currentLinePainter.paint( g, projection, m_currentCurve );

    if( m_currentPoint != null )
    {
      final GM_Point currentPoint = MapUtilities.transform( mapPanel, m_currentPoint );
      m_handlePainter.paint( g, projection, currentPoint );
    }

    if( m_draggedHandle == null && m_pointSnapper != null )
      m_pointSnapper.paint( g );
  }

  /**
   * Resets the widget.
   */
  public void reset( )
  {
    /* Reset the start & current points. */
    m_currentCurve = null;
    m_draggedHandle = null;
    m_currentPoint = null;

    m_handles.clear();

    final Collection<GM_Position> handlePositions = new ArrayList<>();
    final ISegmentData[] segments = m_data.getSegments();
    for( final ISegmentData segment : segments )
    {
      final GM_Position[] leftPositions = updateHandles( segment.getBankLeft() );
      handlePositions.addAll( Arrays.asList( leftPositions ) );

      final GM_Position[] rightPositions = updateHandles( segment.getBankRight() );
      handlePositions.addAll( Arrays.asList( rightPositions ) );
    }

    final IMapPanel mapPanel = getMapPanel();
    if( mapPanel == null )
      m_pointSnapper = null;
    else
    {
      final GM_Position[] allPositions = handlePositions.toArray( new GM_Position[handlePositions.size()] );

      m_pointSnapper = new GM_PointSnapper( allPositions, mapPanel );
    }

    repaintMap();
  }

  private GM_Position[] updateHandles( final IBankData bank )
  {
    if( bank == null )
      return new GM_Position[0];

    final Geometry segmented = bank.getWorkingGeometry();
    if( segmented == null )
      return new GM_Position[0];

    final Coordinate[] coordinates = segmented.getCoordinates();
    final GM_Position[] positions = new GM_Position[coordinates.length - 2];

    // REMARK: omit first and last point
    for( int i = 1; i < coordinates.length - 1; i++ )
    {
      final GM_Position position = JTSAdapter.wrap( coordinates[i] );
      positions[i - 1] = position;

      /* Add the points of every segment to the list of handles. */
      m_handles.put( new Handle( position, null, null, m_radius ), bank );
    }

    return positions;
  }
}