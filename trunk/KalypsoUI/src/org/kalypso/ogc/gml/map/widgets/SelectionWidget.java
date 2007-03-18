/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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
package org.kalypso.ogc.gml.map.widgets;

import java.awt.Graphics;
import java.awt.Point;

import org.eclipse.jface.viewers.ISelection;
import org.eclipse.swt.graphics.Rectangle;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.map.widgets.mapfunctions.IRectangleMapFunction;
import org.kalypso.ogc.gml.map.widgets.mapfunctions.RectangleSelector;
import org.kalypso.ogc.gml.map.widgets.providers.tooltips.ITooltipProvider;

/**
 * This class is a selection widget over all themes.
 * 
 * @author Holger Albert
 */
public class SelectionWidget extends AbstractWidget
{
  /**
   * This selector is responsible for drawing the rectangle for selecting a feature.
   */
  private RectangleSelector m_selector = null;

  /**
   * The map function selects a feature.
   */
  private final IRectangleMapFunction m_clickFunction;

  /**
   * A tooltip provider.
   */
  private ITooltipProvider m_tooltipProvider = null;

  /**
   * The tooltip, which should be displayed.
   */
  private String m_tooltip = "";

  private Point m_current_point;

  /**
   * The constructor.
   * 
   * @param name
   *          The name of the widget.
   * @param toolTip
   *          The tooltip of the widget.
   * @param clickFunction
   *          The map function, which should select a feature. May not be null!
   */
  public SelectionWidget( final String name, final String toolTip, final IRectangleMapFunction clickFunction )
  {
    this( name, toolTip, clickFunction, null );
  }

  /**
   * The constructor.
   * 
   * @param name
   *          The name of the widget.
   * @param toolTip
   *          The tooltip of the widget.
   * @param clickFunction
   *          The map function, which should select a feature. May not be null!
   * @param tooltipProvider
   *          The provider, which will be used on move. May be null.
   */
  public SelectionWidget( final String name, final String toolTip, final IRectangleMapFunction clickFunction, ITooltipProvider tooltipProvider )
  {
    super( name, toolTip );

    m_clickFunction = clickFunction;
    m_tooltipProvider = tooltipProvider;
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#dragged(java.awt.Point)
   */
  @Override
  public void dragged( final Point p )
  {
    if( m_selector != null )
    {
      m_selector.setEndPoint( new org.eclipse.swt.graphics.Point( p.x, p.y ) );
      getMapPanel().setMessage( "Auswahl durchf�hren ..." );
    }
    //TODO: check if this repaint is really necessary
    MapPanel panel = getMapPanel();
    if (panel != null)
      panel.repaint();

  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#leftPressed(java.awt.Point)
   */
  @Override
  public void leftPressed( final Point p )
  {
    /* Start painting the selection rectangle. */
    m_selector = new RectangleSelector( new org.eclipse.swt.graphics.Point( p.x, p.y ) );
    getMapPanel().setMessage( "Auswahl begonnen ..." );
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#leftReleased(java.awt.Point)
   */
  @Override
  public void leftReleased( final Point p )
  {
    try
    {
      if( m_selector != null )
      {
        /* Set the end point. */
        m_selector.setEndPoint( new org.eclipse.swt.graphics.Point( p.x, p.y ) );

        /* Select the feature in this rectangle. */
        m_clickFunction.execute( getMapPanel(), m_selector.getRectangle() );

        getMapPanel().setMessage( "Klicken und ziehen Sie, um ein Feature auszuw�hlen." );
      }
    }
    finally
    {
      m_selector = null;
    }
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#moved(java.awt.Point)
   */
  @Override
  public void moved( Point p )
  {
    super.moved( p );

    if( m_tooltipProvider != null )
      m_tooltip = m_tooltipProvider.getTooltip( getMapPanel(), new Rectangle( p.x, p.y, 0, 0 ) );

    m_current_point = p;

    if ( m_tooltipProvider != null )
    {
      MapPanel panel = getMapPanel();
      if ( panel != null)
        panel.repaint();
    }
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#paint(java.awt.Graphics)
   */
  @Override
  public void paint( final Graphics g )
  {
    if( m_selector != null )
      m_selector.paint( g );

    if( (m_current_point != null) && (m_tooltipProvider != null) && (!m_tooltip.equals( "" )) )
      m_tooltipProvider.paintTooltip( g, m_current_point, m_tooltip );
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#finish()
   */
  @Override
  public void finish( )
  {
    getMapPanel().setMessage( "" );
    m_selector = null;

    super.finish();
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#setSelection(org.eclipse.jface.viewers.ISelection)
   */
  @Override
  public void setSelection( ISelection selection )
  {
    super.setSelection( selection );

    getMapPanel().setMessage( "Klicken und ziehen Sie, um ein Feature auszuw�hlen." );
  }
}