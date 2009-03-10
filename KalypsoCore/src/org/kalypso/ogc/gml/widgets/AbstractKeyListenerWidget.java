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
package org.kalypso.ogc.gml.widgets;

import java.awt.Color;
import java.awt.Cursor;
import java.awt.Graphics;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.event.KeyEvent;

import org.kalypso.ogc.gml.map.IMapPanel;
import org.kalypso.ogc.gml.map.utilities.MapUtilities;
import org.kalypso.ogc.gml.map.utilities.tooltip.ToolTipRenderer;
import org.kalypsodeegree.model.geometry.GM_Point;

/**
 * Includes a MapView KeyListener
 * 
 * <pre>
 * Actions:
 *  ESCAPE - Deactivate current widget
 * </pre>
 * 
 * @author Dirk Kuch
 */
public abstract class AbstractKeyListenerWidget extends AbstractWidget
{
  private final ToolTipRenderer m_toolTipRenderer = new ToolTipRenderer();

  private Point m_currentPoint = null;

  public AbstractKeyListenerWidget( final String name )
  {
    super( name, name );

    m_toolTipRenderer.setBackgroundColor( new Color( 1f, 1f, 0.6f, 0.70f ) );

  }

  protected void paintToolTip( final Graphics g )
  {
    final IMapPanel mapPanel = getMapPanel();
    final Rectangle bounds = mapPanel.getScreenBounds();

    m_toolTipRenderer.setTooltip( getToolTip() );
    m_toolTipRenderer.paintToolTip( new Point( 5, bounds.height - 5 ), g, bounds );
  }

  /**
   * Resets the widget, must be reinitialized before it can be used again.
   * 
   * @see #init(Feature, CommandableWorkspace)
   */
  protected void reset( )
  {
    final IMapPanel mapPanel = getMapPanel();

    mapPanel.setCursor( java.awt.Cursor.getPredefinedCursor( Cursor.DEFAULT_CURSOR ) );
    mapPanel.getWidgetManager().setActualWidget( null );
  }

  /**
   * Escape Key pressed? -> reset / deactivate widget
   * 
   * @see org.kalypso.ogc.gml.widgets.AbstractWidget#keyReleased(java.awt.event.KeyEvent)
   */
  @Override
  public void keyReleased( final KeyEvent e )
  {
    final int keyCode = e.getKeyCode();
    if( KeyEvent.VK_ESCAPE == keyCode )
    {
      reset();
    }

    super.keyPressed( e );
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#moved(java.awt.Point)
   */
  @Override
  public void moved( final Point p )
  {
    m_currentPoint = p;

    /* Repaint. */
    final IMapPanel panel = getMapPanel();
    if( panel != null )
    {
      panel.repaintMap();
    }
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.AbstractWidget#dragged(java.awt.Point)
   */
  @Override
  public void dragged( final java.awt.Point p )
  {
    m_currentPoint = p;

    /* Repaint. */
    final IMapPanel panel = getMapPanel();
    if( panel != null )
    {
      panel.repaintMap();
    }
  }

  protected Point getCurrentPoint( )
  {
    return m_currentPoint;
  }

  protected GM_Point getCurrentGmPoint( )
  {
    final Point current = getCurrentPoint();
    if( current != null )
      return MapUtilities.transform( getMapPanel(), current );

    return null;
  }
}
