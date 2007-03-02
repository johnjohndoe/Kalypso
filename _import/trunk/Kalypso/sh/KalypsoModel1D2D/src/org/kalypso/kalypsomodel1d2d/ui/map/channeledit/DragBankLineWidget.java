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

import java.awt.Graphics;
import java.awt.Point;
import java.util.ArrayList;
import java.util.List;

import org.kalypso.ogc.gml.map.utilities.MapUtilities;
import org.kalypso.ogc.gml.map.widgets.AbstractWidget;
import org.kalypso.ogc.gml.map.widgets.providers.handles.Handle;
import org.kalypso.ogc.gml.map.widgets.providers.handles.IHandle;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_CurveSegment;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_LineString;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

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

  public DragBankLineWidget( GM_Curve bankline )
  {
    super( "Uferline", "Dieses Widget ermöglicht das Verändern der Uferlinie." );
    m_bankline = bankline;

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

      int numberOfCurveSegments = m_bankline.getNumberOfCurveSegments();

      for( int i = 0; i < numberOfCurveSegments; i++ )
      {
        /* One segment of a curve. It can contain several points. */
        GM_CurveSegment curveSegment = m_bankline.getCurveSegmentAt( i );

        GM_Position[] positions = curveSegment.getPositions();
        for( GM_Position position : positions )
        {
          /* Add the points of every segment to the list of handles. */
          list.add( new Handle( position, null, null, m_radius ) );
        }
      }
    }
    catch( GM_Exception e )
    {
      e.printStackTrace();
    }

    return list;
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#dragged(java.awt.Point)
   */
  @Override
  public void dragged( Point p )
  {
    if( m_handles == null )
      return;

    if( m_startPoint == null )
    {
      /* Store the start point. */
      m_startPoint = p;

      /* Check, if the mouse cursor is near some handles. */
      for( IHandle handle : m_handles )
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
  public void leftReleased( Point p )
  {
    if( (m_handles == null) || (m_startPoint == null) || (m_currentPoint == null) )
      return;

    /* Memory to collect all active handles. */
    ArrayList<Handle> list = new ArrayList<Handle>();

    /* Set all handles inactive. */
    for( IHandle handle : m_handles )
      handle.setActive( false );

    // TODO collect all handles to change

    if( list.size() == 0 )
    {
      /* Reset. */
      m_currentPoint = null;
      m_startPoint = null;

      return;
    }

    /* Create new geometry. */
    // TODO
    /* Reset. */
    m_currentPoint = null;
    m_startPoint = null;
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#paint(java.awt.Graphics)
   */
  @Override
  public void paint( Graphics g )
  {
    if( m_handles == null )
      return;
    
    //GM_LineString linestrin = m_bankline.getAsLineString();
    GM_Point[] linepoints = new GM_Point[m_handles.size() + 2];  //number of handles plus start and end point
    
    int i = 0;
    
    /* Paint all handles. */
    for( IHandle handle : m_handles )
    {
      i = i+1;
      if( handle.isActive() )
      {
        handle.paint( getMapPanel().getGraphics(), getMapPanel().getProjection(), m_startPoint, m_currentPoint );
        linepoints[i] = MapUtilities.transform( getMapPanel(), m_currentPoint );
      }
      else
      {
        handle.paint( getMapPanel().getGraphics(), getMapPanel().getProjection(), null, null );
        
      }
    }

    // paint the current dragged line
    // take all handles, check for the acitve handle, add it to the start ad endpoint of the orig curve and built a line
    
  }

  /**
   * Resets the widget.
   */
  public void reset( )
  {
    /* Reset the start & current points. */
    m_startPoint = null;
    m_currentPoint = null;
  }
}