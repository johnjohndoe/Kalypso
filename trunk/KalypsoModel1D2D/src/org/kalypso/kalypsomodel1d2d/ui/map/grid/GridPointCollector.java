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
package org.kalypso.kalypsomodel1d2d.ui.map.grid;

import java.awt.Color;
import java.awt.Graphics;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Logger;

import org.eclipse.core.runtime.IStatus;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.ui.map.quadmesh.QuadMesh;
import org.kalypso.kalypsomodel1d2d.ui.map.quadmesh.QuadMeshPainter;
import org.kalypso.kalypsomodel1d2d.ui.map.quadmesh.QuadMeshValidator;
import org.kalypso.kalypsomodel1d2d.ui.map.quadmesh.QuadMesher;
import org.kalypso.kalypsosimulationmodel.core.Assert;
import org.kalypso.ogc.gml.map.widgets.advanced.utils.SLDPainter2;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.CoordinateList;
import com.vividsolutions.jts.geom.LineString;
import com.vividsolutions.jts.linearref.LengthIndexedLine;

public class GridPointCollector
{
  private static final Logger logger = Logger.getLogger( GridPointCollector.class.getName() );

  public static final int SIDE_MAX_NUM = 4;

  public static final int SIDE_TOP = 0;

  public static final int SIDE_BOTTOM = 2;

  public static final int SIDE_LEFT = 1;

  public static final int SIDE_RIGHT = 3;

  private final SLDPainter2 m_meshEdgePainter = new SLDPainter2( getClass().getResource( "resources/meshEdge.sld" ) ); //$NON-NLS-1$

  private final SLDPainter2 m_meshEdgeInvalidPainter = new SLDPainter2( getClass().getResource( "resources/meshEdgeInvalid.sld" ) ); //$NON-NLS-1$

  private int m_actualSideKey;

  private boolean m_hasAllSides = false;

  private final LinePointCollector m_sides[] = new LinePointCollector[SIDE_MAX_NUM];

  private final LinePointCollectorConfig m_lpcConfigs[] = new LinePointCollectorConfig[SIDE_MAX_NUM];

  private QuadMesh m_tempGrid = null;

  private final List<IGridPointCollectorStateListener> m_stateListeners = new ArrayList<>();

  private final String m_srsName;

  private final IFEDiscretisationModel1d2d m_discModel;

  public GridPointCollector( final IFEDiscretisationModel1d2d discModel, final String srsName )
  {
    m_discModel = discModel;
    m_srsName = srsName;

    initLPCConfigs();
  }

  private final void initLPCConfigs( )
  {
    final Color colors[] = GridWidgetFace.getLineColors();
    final int pointRectSize = GridWidgetFace.getPointRectSize();
    for( int i = 0; i < m_sides.length; i++ )
    {
      m_lpcConfigs[i] = new LinePointCollectorConfig( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.grid.GridPointCollector.1" ) + (i + 1), colors[i], m_sides[i] ); //$NON-NLS-1$
      m_lpcConfigs[i].setPointRectSize( pointRectSize );
    }
  }

  public void reset( final String targetCrs )
  {
    for( LinePointCollector b : m_sides )
    {
      if( b != null )
        b.reset( targetCrs );
      else
        b = new LinePointCollector( 0, targetCrs );
    }

    m_actualSideKey = 0;
    m_hasAllSides = false;

    m_tempGrid = null;

    if( m_sides[m_actualSideKey] == null )
    {
      m_sides[m_actualSideKey] = new LinePointCollector( 0, targetCrs );
      m_lpcConfigs[m_actualSideKey].setConfigLinePointCollector( m_sides[m_actualSideKey] );
    }
    fireStateChanged();
  }

  public GM_Object addPoint( final GM_Point p )
  {
    if( m_hasAllSides || m_actualSideKey >= SIDE_MAX_NUM )
      return null;

    Assert.throwIAEOnNull( m_sides[m_actualSideKey], Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.grid.GridPointCollector.3" ) ); //$NON-NLS-1$

    final GM_Point previousAdded = m_sides[m_actualSideKey].getLastPoint();
    if( previousAdded != null )
    {
      if( previousAdded.getX() == p.getX() && previousAdded.getY() == p.getY() )
        return previousAdded;
    }

    final GM_Point lastAdded = (GM_Point)m_sides[m_actualSideKey].addPoint( p );

    final GM_Point autocompleted = autoComplete();
    if( autocompleted != null )
      return finishSide();

    fireStateChanged();
    return lastAdded;
  }

  /**
   * Auto complete this line collector and returns the completing point if done.<br/>
   * Auto completion is only done for the last two sides.
   * 
   * @return the auto completion point
   */
  public GM_Point autoComplete( )
  {
    // Last side: add very last point automatically
    if( m_actualSideKey == 3 && m_sides[m_actualSideKey].getRemainingPointCnt() == 1 )
    {
      final GM_Point point = m_sides[0].getFirstPoint();
      m_sides[m_actualSideKey].addPoint( point );
      return point;
    }

    // Auto advance side if max count is reached
    if( m_sides[m_actualSideKey].getRemainingPointCnt() == 0 )
      return (GM_Point)m_sides[m_actualSideKey].finish();

    return null;
  }

  public GM_Point getLastPoint( )
  {
    if( m_actualSideKey >= SIDE_MAX_NUM )
      return null;

    Assert.throwIAEOnNull( m_sides[m_actualSideKey], "builder not available for adding a point" ); //$NON-NLS-1$
    return m_sides[m_actualSideKey].getLastPoint();
  }

  public GM_Object finishSide( )
  {
    Assert.throwIAEOnNull( m_sides[m_actualSideKey], "builder not available" ); //$NON-NLS-1$

    if( m_actualSideKey >= SIDE_MAX_NUM )
      return null;

    final LinePointCollector oldBuilder = m_sides[m_actualSideKey];

    final GM_Object gmObject = m_sides[m_actualSideKey].finish();

    if( gmObject == null )
    {
      // not finish
      return null;
    }

    m_actualSideKey++;
    if( m_actualSideKey < SIDE_MAX_NUM )
    {
      LinePointCollector newSide = m_sides[m_actualSideKey];
      if( newSide == null )
      {
        newSide = oldBuilder.getNewBuilder();
        m_sides[m_actualSideKey] = newSide;
        m_lpcConfigs[m_actualSideKey].setConfigLinePointCollector( newSide );
      }

      final GM_Point lastP = oldBuilder.getLastPoint();
      newSide.setCntPoints( computeSize() );
      if( lastP != null )
        newSide.addPoint( lastP );
      else
        logger.warning( "Last point is null" ); //$NON-NLS-1$
    }
    else
    {
      m_hasAllSides = true;
      updateTempGrid();
    }

    fireStateChanged();
    return gmObject;
  }

  private final void updateTempGrid( )
  {
    final QuadMesher mesher = new QuadMesher();
    mesher.createMesh( m_srsName, m_sides[SIDE_TOP], m_sides[SIDE_BOTTOM], m_sides[SIDE_LEFT], m_sides[SIDE_RIGHT] );
    m_tempGrid = mesher.getMesh();
  }

  private final int computeSize( )
  {
    if( m_actualSideKey == 0 || m_actualSideKey == 1 )
      return 0;
    else if( m_actualSideKey == 2 )
      return m_sides[0].getCurrentPointCnt();
    else if( m_actualSideKey == 3 )
      return m_sides[1].getCurrentPointCnt();
    else
      return 0;
  }

  public void paint( final Graphics g, final GeoTransform projection, final GM_Point currentPoint )
  {
    LinePointCollector currentLine = null;
    if( m_actualSideKey < SIDE_MAX_NUM )
    {
      if( m_sides[m_actualSideKey] == null )
        return;

      currentLine = m_sides[m_actualSideKey];
      Assert.throwIAEOnNull( currentLine, Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.grid.GridPointCollector.9" ) ); //$NON-NLS-1$
    }

    /* draw temp grid */
    if( isValid().isOK() )
    {
      final QuadMeshPainter meshPainter = new QuadMeshPainter( m_tempGrid, m_meshEdgePainter, null );
      meshPainter.paint( g, projection );
    }
    else
    {
      final QuadMeshPainter meshPainter = new QuadMeshPainter( m_tempGrid, m_meshEdgeInvalidPainter, null );
      meshPainter.paint( g, projection );
    }

    /* draw selected line */
    // FIXME: separate the concept of the line currently created, and the selected line when editing the grid afterwards
    if( m_actualSideKey < SIDE_MAX_NUM )
    {
      final LinePointCollector selectedSide = m_sides[m_actualSideKey];
      selectedSide.paintLine( g, projection, 3, m_lpcConfigs[m_actualSideKey].getColor() );
    }

    for( int i = 0; i < m_sides.length; i++ )
    {
      final LinePointCollector lpc = m_sides[i];
      if( lpc == null )
        continue;

      g.setColor( m_lpcConfigs[i].getColor() );

      final int pointRectSize = m_lpcConfigs[i].getPointRectSize();
      if( lpc != currentLine )
        lpc.paint( g, projection, null, pointRectSize );
      else
        lpc.paint( g, projection, currentPoint, pointRectSize );
    }
  }

  public void clearCurrent( )
  {
    if( m_actualSideKey >= SIDE_MAX_NUM )
      return;

    final LinePointCollector builder = m_sides[m_actualSideKey];
    builder.clear();
    if( m_actualSideKey > 0 )
    {
      final LinePointCollector previousBuilder = m_sides[m_actualSideKey - 1];
      if( previousBuilder != null )
      {
        final GM_Point point = previousBuilder.getLastPoint();
        if( point != null )
          builder.addPoint( point );
      }
    }
    m_hasAllSides = false;
    m_tempGrid = null;

    fireStateChanged();
  }

  public void gotoPreviousSide( )
  {
    m_hasAllSides = false;
    m_tempGrid = null;

    LinePointCollector curBuilder;
    if( m_actualSideKey >= SIDE_MAX_NUM )
    {
      // empty
    }
    else
    {
      curBuilder = m_sides[m_actualSideKey];
      if( curBuilder != null )
        curBuilder.clear();
    }
    if( m_actualSideKey > 0 )
    {
      m_actualSideKey--;
      m_sides[m_actualSideKey].removeLastPoint( false );
    }

    fireStateChanged();
  }

  public void removeLastPoint( )
  {
    m_hasAllSides = false;
    m_tempGrid = null;

    if( m_actualSideKey >= SIDE_MAX_NUM )
    {
      // goto to the last line builder
      m_actualSideKey = SIDE_MAX_NUM - 1;
    }

    final LinePointCollector builder = m_sides[m_actualSideKey];

    builder.removeLastPoint( m_actualSideKey == 0 );
    fireStateChanged();
  }

  public void replaceLastPoint( final GM_Point point )
  {
    if( m_actualSideKey >= SIDE_MAX_NUM )
      return;

    // TODO check going to previous side
    final LinePointCollector builder = m_sides[m_actualSideKey];

    builder.replaceLastPoint( point );
  }

  public void selectNext( )
  {
    if( !getHasAllSides() )
      return;

    if( m_actualSideKey < SIDE_MAX_NUM )
      m_sides[m_actualSideKey].setSelected( false );

    m_actualSideKey = (m_actualSideKey + 1) % SIDE_MAX_NUM;
    m_sides[m_actualSideKey].setSelected( true );
    fireStateChanged();
  }

  public void selectPoint( final GM_Point squareCenter, final double squareWidth )
  {
    if( m_actualSideKey < SIDE_MAX_NUM )
      m_sides[m_actualSideKey].selectPoint( squareCenter, squareWidth );
  }

  public void changeSelectedPoint( final GM_Point newPosition )
  {
    if( m_actualSideKey < SIDE_MAX_NUM )
    {
      m_sides[m_actualSideKey].changeSelected( newPosition );
      updateTempGrid();
    }
  }

  public GM_Point getSelectedPoint( )
  {
    if( m_actualSideKey < SIDE_MAX_NUM )
      return m_sides[m_actualSideKey].getSelectedPoint();

    return null;
  }

  public boolean getHasAllSides( )
  {
    return m_hasAllSides;
  }

  public LinePointCollectorConfig[] getSideconfigsAsArray( )
  {
    final LinePointCollectorConfig[] cloneCollectorConfigs = m_lpcConfigs.clone();
    return cloneCollectorConfigs;
  }

  public LinePointCollectorConfig getCurrentLPCConfig( )
  {
    if( m_actualSideKey < SIDE_MAX_NUM )
      return m_lpcConfigs[m_actualSideKey];

    return null;
  }

  /**
   * To get the with for the square that are drawn to show point. if ther is an active {@link LinePointCollectorConfig} its actual point rect size is resturn otherwise the point square size of the
   * first {@link LinePointCollectorConfig}
   * 
   * @return Returns the with for the square that are drawn to show point
   */
  public int getPointRectSize( )
  {
    if( m_actualSideKey < SIDE_MAX_NUM )
      return m_lpcConfigs[m_actualSideKey].getPointRectSize();
    else
      return m_lpcConfigs[0].getPointRectSize();
  }

  public void addGridPointCollectorStateChangeListener( final IGridPointCollectorStateListener listener )
  {
    Assert.throwIAEOnNullParam( listener, "listener" ); //$NON-NLS-1$

    if( m_stateListeners.contains( listener ) )
      return;
    else
      m_stateListeners.add( listener );
  }

  private final void fireStateChanged( )
  {
    for( final IGridPointCollectorStateListener listener : m_stateListeners )
      listener.stateChanged( null );
  }

  public void setPointRectSize( final int pointRectSize )
  {
    if( pointRectSize <= 0 )
      throw new IllegalArgumentException( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.grid.GridPointCollector.12" ) + pointRectSize ); //$NON-NLS-1$
    else
    {
      for( final LinePointCollectorConfig config : m_lpcConfigs )
        config.setPointRectSize( pointRectSize );
    }
  }

  public void setColor( final int lineIndex, final Color lineColor )
  {
    m_lpcConfigs[lineIndex].setColor( lineColor );
    fireStateChanged();
  }

  public IStatus isValid( )
  {
    final QuadMeshValidator validator = new QuadMeshValidator( m_tempGrid );

    return validator.isValid( m_discModel );
  }

  public QuadMesh getTempGrid( )
  {
    return m_tempGrid;
  }

  /**
   * For the last two sides: auto adjust line and try to finish.
   */
  public void autoadjustOrAndFinishSide( )
  {
    if( m_actualSideKey > 1 )
      autoAdjust();

    finishSide();
  }

  private void autoAdjust( )
  {
    final LinePointCollector currentSide = m_sides[m_actualSideKey];

    // Do nothing, if point count is already reached
    if( currentSide.getRemainingPointCnt() == 0 )
      return;

    // Special case last side: add last point
    if( m_actualSideKey == 3 )
    {
      // hm...: actually we only want to remove the last point, if it snapped the starting point
      currentSide.removeLastPoint( false );

      final GM_Point point = m_sides[0].getFirstPoint();
      currentSide.addPoint( point );
    }

    try
    {
      /* get opposite line */
      final LinePointCollector oppositeSide = m_sides[m_actualSideKey % 2];
      final GM_Curve oppositeCurve = oppositeSide.getAsCurve();

      /* calculate structure of opposite line */
      final LineString oppositeLine = (LineString)JTSAdapter.export( oppositeCurve );
      final LineString reverseOppositeLine = (LineString)oppositeLine.reverse();

      final double[] segmentPercents = calculateSegmentPercents( reverseOppositeLine );

      /* recalculate point locations for current line */
      final GM_Curve currentCurve = currentSide.getAsCurve();
      final LineString currentLine = (LineString)JTSAdapter.export( currentCurve );
      final LineString newCurrentLine = resegmentLine( currentLine, segmentPercents );

      // remove all points from current Line
      final int currentPointCount = currentSide.getCurrentPointCnt();
      for( int i = 0; i < currentPointCount - 1; i++ )
        currentSide.removeLastPoint( false );

      // add all points
      final Coordinate[] newCoordinates = newCurrentLine.getCoordinates();
      for( int i = 1; i < newCoordinates.length; i++ )
      {
        final Coordinate coordinate = newCoordinates[i];

        final GM_Point newPoint = org.kalypsodeegree_impl.model.geometry.GeometryFactory.createGM_Point( coordinate.x, coordinate.y, m_srsName );
        currentSide.addPoint( newPoint );
      }
    }
    catch( final GM_Exception e )
    {
      e.printStackTrace();
    }
  }

  private double[] calculateSegmentPercents( final LineString line )
  {
    final LengthIndexedLine indexedLine = new LengthIndexedLine( line );

    final double lineLength = line.getLength();

    final Coordinate[] coordinates = line.getCoordinates();

    final double[] indices = new double[coordinates.length];
    for( int i = 0; i < indices.length; i++ )
      indices[i] = indexedLine.indexOf( coordinates[i] );

    final double[] percents = new double[indices.length];
    for( int i = 0; i < percents.length; i++ )
      percents[i] = indices[i] / lineLength;

    return percents;
  }

  private LineString resegmentLine( final LineString line, final double[] segmentPercents )
  {
    final CoordinateList newCoordinates = new CoordinateList();

    final double lineLength = line.getLength();
    final LengthIndexedLine indexedLine = new LengthIndexedLine( line );

    for( final double segmentPercent : segmentPercents )
    {
      final double index = segmentPercent * lineLength;
      final Coordinate newCoordinate = indexedLine.extractPoint( index );
      newCoordinates.add( newCoordinate, true );
    }

    return line.getFactory().createLineString( newCoordinates.toCoordinateArray() );
  }
}