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
import java.awt.Point;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Logger;

import org.eclipse.core.runtime.IStatus;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.ui.map.util.TempGrid;
import org.kalypso.kalypsosimulationmodel.core.Assert;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.map.IMapPanel;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Point;

public class GridPointCollector
{
  private static final Logger logger = Logger.getLogger( GridPointCollector.class.getName() );

  public static final int SIDE_MAX_NUM = 4;

  public static final int SIDE_TOP = 0;

  public static final int SIDE_BOTTOM = 2;

  public static final int SIDE_LEFT = 1;

  public static final int SIDE_RIGHT = 3;
  
  private static final double DISTANCE_DEF = 0.01;

  private int actualSideKey;

  private boolean m_hasAllSides = false;

  private final LinePointCollector m_sides[] = new LinePointCollector[SIDE_MAX_NUM];

  private final LinePointCollectorConfig m_lpcConfigs[] = new LinePointCollectorConfig[SIDE_MAX_NUM];

  private final TempGrid m_tempGrid = new TempGrid();

  private final List<IGridPointCollectorStateListener> m_stateListeners = new ArrayList<IGridPointCollectorStateListener>();

  private IKalypsoFeatureTheme m_nodeTheme;

  public GridPointCollector( )
  {
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
    actualSideKey = 0;
    m_hasAllSides = false;
    m_tempGrid.resetTempGrid( targetCrs );
    if( m_sides[actualSideKey] == null )
    {
      m_sides[actualSideKey] = new LinePointCollector( 0, targetCrs );
      m_lpcConfigs[actualSideKey].setConfigLinePointCollector( m_sides[actualSideKey] );
    }
    fireStateChanged();
  }

  public GM_Object addPoint( final GM_Point p )
  {
    if( m_hasAllSides || actualSideKey >= SIDE_MAX_NUM )
      return null;

    Assert.throwIAEOnNull( m_sides[actualSideKey], Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.grid.GridPointCollector.3" ) ); //$NON-NLS-1$

    final GM_Point lastAdded = (GM_Point) m_sides[actualSideKey].addPoint( p );
    final GM_Point autocompleted = autoComplete();
    if( autocompleted != null )
      return finishSide();
    else
    {
      fireStateChanged();
      return lastAdded;
    }

  }

  /**
   * Auto complete this line collector and returns the completing point if done. Auto completion is only done for the
   * last side because
   * 
   * @return the auto completion point
   */
  public GM_Point autoComplete( )
  {
    if( actualSideKey == 3 )
    {
      System.out.println( "Autocompleting:" ); //$NON-NLS-1$
      if( m_sides[actualSideKey].getRemainingPointCnt() == 1 )
      {
        final GM_Point point = m_sides[0].getFirstPoint();
        if( point instanceof MutableGMPoint )
        {
          m_sides[actualSideKey].addPoint( point );
          return point;
        }
        else
          throw new RuntimeException( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.grid.GridPointCollector.5" ) ); //$NON-NLS-1$
      }
      else
        return null;
    }
    else
    {
      if( m_sides[actualSideKey].getRemainingPointCnt() == 0 )
        return (GM_Point) m_sides[actualSideKey].finish();// getLastPoint();
      else
        return null;
    }
  }

  public GM_Point getLastPoint( ) throws Exception
  {
    if( actualSideKey >= SIDE_MAX_NUM )
      return null;

    Assert.throwIAEOnNull( m_sides[actualSideKey], Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.grid.GridPointCollector.6" ) ); //$NON-NLS-1$
    return m_sides[actualSideKey].getLastPoint();
  }

  public GM_Object finishSide( )
  {
    Assert.throwIAEOnNull( m_sides[actualSideKey], Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.grid.GridPointCollector.7" ) ); //$NON-NLS-1$
    if( actualSideKey >= SIDE_MAX_NUM )
    {
      return null;
    }
    final LinePointCollector oldBuilder = m_sides[actualSideKey];
    final GM_Object gmObject = m_sides[actualSideKey].finish();
    if( gmObject == null )
    {
      // not finish
      return null;
    }
    actualSideKey++;
    if( actualSideKey < SIDE_MAX_NUM )
    {
      // actualSideKey++;
      LinePointCollector newSide = m_sides[actualSideKey];
      if( newSide == null )
      {
        newSide = oldBuilder.getNewBuilder();
        m_sides[actualSideKey] = newSide;
        m_lpcConfigs[actualSideKey].setConfigLinePointCollector( newSide );
      }

      final GM_Point lastP = oldBuilder.getLastPoint();
      newSide.setCntPoints( computeSize() );
      if( lastP != null )
        newSide.addPoint( lastP );
      else
        logger.warning( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.grid.GridPointCollector.8" ) ); //$NON-NLS-1$
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
    m_tempGrid.setTempGrid( m_sides[SIDE_TOP], m_sides[SIDE_BOTTOM], m_sides[SIDE_LEFT], m_sides[SIDE_RIGHT] );
  }

  private final int computeSize( )
  {
    if( actualSideKey == 0 || actualSideKey == 1 )
      return 0;
    else if( actualSideKey == 2 )
      return m_sides[0].getCurrentPointCnt();
    else if( actualSideKey == 3 )
      return m_sides[1].getCurrentPointCnt();
    else
      return 0;
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.builders.IGeometryBuilder#paint(java.awt.Graphics,
   *      org.kalypsodeegree.graphics.transformation.GeoTransform, java.awt.Point)
   */
  public void paint( final Graphics g, final GeoTransform projection, final Point currentPoint )
  {
    LinePointCollector builder = null;
    if( actualSideKey < SIDE_MAX_NUM )
    {
      if( m_sides[actualSideKey] == null )
        return;

      builder = m_sides[actualSideKey];
      Assert.throwIAEOnNull( builder, Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.grid.GridPointCollector.9" ) ); //$NON-NLS-1$
    }

    final Color curColor = g.getColor();

    int i = 0;
    for( final LinePointCollector b : m_sides )
    {
      if( b == null )
        continue;

      g.setColor( m_lpcConfigs[i].getColor() );
      final int pointRectSize = m_lpcConfigs[i].getPointRectSize();
      if( b != builder )
        b.paint( g, projection, null, pointRectSize );
      else
        b.paint( g, projection, currentPoint, pointRectSize );

      i++;
    }

    /* draw temp grid */
    m_tempGrid.paint( g, projection );

    /* draw selected line */
    if( actualSideKey < SIDE_MAX_NUM )
    {
      builder = m_sides[actualSideKey];
      builder.paintLine( g, projection, 1, m_lpcConfigs[actualSideKey].getColor() );
      g.setColor( curColor );
    }

  }

  public void clearCurrent( )
  {
    if( actualSideKey >= SIDE_MAX_NUM )
      return;

    final LinePointCollector builder = m_sides[actualSideKey];
    builder.clear();
    if( actualSideKey > 0 )
    {
      final LinePointCollector previousBuilder = m_sides[actualSideKey - 1];
      if( previousBuilder != null )
      {
        final GM_Point point = previousBuilder.getLastPoint();
        if( point != null )
          builder.addPoint( point );
      }
    }
    m_hasAllSides = false;
    fireStateChanged();
  }

  public void gotoPreviousSide( )
  {
    LinePointCollector curBuilder;
    if( actualSideKey >= SIDE_MAX_NUM )
    {
      // empty
    }
    else
    {
      curBuilder = m_sides[actualSideKey];
      if( curBuilder != null )
        curBuilder.clear();
    }
    if( actualSideKey > 0 )
    {
      actualSideKey--;
      m_sides[actualSideKey].removeLastPoint( false );
      if( actualSideKey < 2 )
        m_sides[actualSideKey].removeMaxNum();
    }

    fireStateChanged();
  }

  public void removeLastPoint( )
  {
    if( actualSideKey >= SIDE_MAX_NUM )
    {
      // goto to the last line builder
      actualSideKey = SIDE_MAX_NUM - 1;
    }

    final LinePointCollector builder = m_sides[actualSideKey];

    builder.removeLastPoint( actualSideKey == 0 );
    fireStateChanged();
  }

  public void replaceLastPoint( final GM_Point point )
  {
    if( actualSideKey >= SIDE_MAX_NUM )
      return;

    // TODO check going to previous side
    final LinePointCollector builder = m_sides[actualSideKey];

    builder.replaceLastPoint( point );
  }

  public void selectNext( )
  {
    if( actualSideKey < SIDE_MAX_NUM )
      m_sides[actualSideKey].setSelected( false );

    actualSideKey = (actualSideKey + 1) % SIDE_MAX_NUM;
    m_sides[actualSideKey].setSelected( true );
    fireStateChanged();
  }

  public void selectPoint( final GM_Point squareCenter, final double squareWidth )
  {
    if( actualSideKey < SIDE_MAX_NUM )
      m_sides[actualSideKey].selectPoint( squareCenter, squareWidth );
  }

  public void changeSelectedPoint( final GM_Point newPosition )
  {
    if( actualSideKey < SIDE_MAX_NUM )
    {
      m_sides[actualSideKey].changeSelected( newPosition );
      updateTempGrid();
    }
  }

  public GM_Point getSelectedPoint( )
  {
    if( actualSideKey < SIDE_MAX_NUM )
      return m_sides[actualSideKey].getSelectedPoint();

    return null;
  }

  public boolean getHasAllSides( )
  {
    return m_hasAllSides;
  }

  public IStatus getAddToModelCommand( final IMapPanel mapPanel, final IFEDiscretisationModel1d2d model, final CommandableWorkspace commandableWorkspace )
  {
    return m_tempGrid.getAddToModelCommand( mapPanel, model, commandableWorkspace, DISTANCE_DEF );
  }

  public LinePointCollectorConfig[] getSideconfigsAsArray( )
  {
    final LinePointCollectorConfig[] cloneCollectorConfigs = m_lpcConfigs.clone();
    return cloneCollectorConfigs;
  }

  public LinePointCollectorConfig getCurrentLPCConfig( )
  {
    if( actualSideKey < SIDE_MAX_NUM )
      return m_lpcConfigs[actualSideKey];

    return null;
  }

  /**
   * To get the with for the square that are drawn to show point. if ther is an active {@link LinePointCollectorConfig}
   * its actual point rect size is resturn otherwise the point square size of the first {@link LinePointCollectorConfig}
   * 
   * @return Returns the with for the square that are drawn to show point
   */
  public int getPointRectSize( )
  {
    if( actualSideKey < SIDE_MAX_NUM )
      return m_lpcConfigs[actualSideKey].getPointRectSize();
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

  public void setNodeTheme( final IKalypsoFeatureTheme nodeTheme )
  {
    m_nodeTheme = nodeTheme;
    m_tempGrid.setNodeTheme( m_nodeTheme );
  }

  public IStatus isValid( )
  {
    return m_tempGrid.isValid();
  }
}