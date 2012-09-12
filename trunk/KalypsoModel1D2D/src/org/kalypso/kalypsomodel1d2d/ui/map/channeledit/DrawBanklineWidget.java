/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005, 2006 by:

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
package org.kalypso.kalypsomodel1d2d.ui.map.channeledit;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Cursor;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.event.KeyEvent;
import java.awt.event.MouseEvent;

import org.apache.commons.lang3.StringUtils;
import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.ui.map.channeledit.ChannelEditData.SIDE;
import org.kalypso.ogc.gml.map.IMapPanel;
import org.kalypso.ogc.gml.map.utilities.MapUtilities;
import org.kalypso.ogc.gml.map.utilities.tooltip.ToolTipRenderer;
import org.kalypso.ogc.gml.map.widgets.builders.LineGeometryBuilder;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.ogc.gml.widgets.AbstractWidget;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Point;

/**
 * widget to create bank line geometries
 *
 * @author Thomas Jung
 */
class DrawBanklineWidget extends AbstractWidget
{
  private LineGeometryBuilder m_lineBuilder = null;

  private GM_Point m_currentPos = null;

  /**
   * The bankline.
   */
  private GM_Curve m_bankline;

  private final ToolTipRenderer m_toolTipRenderer = new ToolTipRenderer();

  private final ToolTipRenderer m_warningRenderer = new ToolTipRenderer();

  private final ChannelEditData m_data;

  private final SIDE m_side;

  private boolean m_edit;

  private LineGeometryEditor m_lineEditor = null;

  public DrawBanklineWidget( final ChannelEditData channeldata, final SIDE side )
  {
    super( StringUtils.EMPTY, StringUtils.EMPTY );

    m_data = channeldata;
    m_side = side;

    m_toolTipRenderer.setBackgroundColor( new Color( 1f, 1f, 0.6f, 0.70f ) );
    m_warningRenderer.setBackgroundColor( new Color( 1f, 0.4f, 0.4f, 0.80f ) );
  }

  @Override
  public void activate( final ICommandTarget commandPoster, final IMapPanel mapPanel )
  {
    super.activate( commandPoster, mapPanel );

    m_edit = false;

    reinit();
  }

  private void reinit( )
  {
    final IMapPanel mapPanel = getMapPanel();
    final IMapModell mapModell = mapPanel.getMapModell();

    m_lineBuilder = new LineGeometryBuilder( 0, mapModell.getCoordinatesSystem() );

    m_bankline = m_data.getBanklineForSide( m_side );

    if( m_bankline != null )
      m_lineEditor = new LineGeometryEditor( new GM_Curve[] { m_bankline }, m_bankline, getMapPanel() );
  }

  @Override
  public void mouseMoved( final MouseEvent event )
  {
    final Point p = event.getPoint();
    if( p == null )
      return;

    final IMapPanel mapPanel = getMapPanel();
    if( mapPanel == null )
      return;

    m_currentPos = MapUtilities.transform( getMapPanel(), p );

    if( m_edit && m_bankline != null )
      m_lineEditor.moved( m_currentPos );
    else
      getMapPanel().setCursor( Cursor.getPredefinedCursor( Cursor.DEFAULT_CURSOR ) );

    repaintMap();
  }

  @Override
  public void mouseDragged( final MouseEvent event )
  {
    if( m_edit && m_bankline != null )
      m_lineEditor.dragged( event.getPoint(), getMapPanel() );
  }

  @Override
  public void mousePressed( final MouseEvent event )
  {
    if( event.getButton() != MouseEvent.BUTTON1 )
      return;

    event.consume();

    final IMapPanel mapPanel = getMapPanel();
    if( mapPanel == null )
      return;

    final Point p = event.getPoint();

    m_currentPos = MapUtilities.transform( mapPanel, p );

    /* If we have a node, take this position, else take the current one */
    // final GM_Point currentPos = MapUtilities.transform( mapPanel, p );

    if( !m_edit )
    {
      try
      {
        final GM_Curve curve = (GM_Curve) m_lineBuilder.addPoint( m_currentPos );
        if( curve != null )
          finishLine( curve );
      }
      catch( final Exception e )
      {
        e.printStackTrace();
        reinit();
      }
    }
  }

  @Override
  public void mouseReleased( final MouseEvent event )
  {
    if( event.getButton() != MouseEvent.BUTTON1 )
      return;
    event.consume();

    if( m_edit && m_lineEditor != null )
    {
      final GM_Curve curve = m_lineEditor.finish();
      if( curve != null )
        finishLine( curve );
    }
  }

  @Override
  public void mouseClicked( final MouseEvent event )
  {
    if( event.getButton() != MouseEvent.BUTTON1 )
      return;
    if( event.getClickCount() < 2 )
      return;
    event.consume();

    if( m_edit && event.isShiftDown() )
    {
      // TODO: insert points into current line
    }

    if( m_lineBuilder != null )
    {
      try
      {
        final GM_Curve curve = (GM_Curve) m_lineBuilder.finish();
        finishLine( curve );
      }
      catch( final Exception e )
      {
        e.printStackTrace();
        reinit();
      }
    }
  }

  @Override
  public void paint( final Graphics g )
  {
    final IMapPanel mapPanel = getMapPanel();
    if( mapPanel == null )
      return;

    super.paint( g );

    // if edit mode is active, paint edited line
    if( m_edit && m_bankline != null && m_lineEditor != null )
      m_lineEditor.paint( g );
    else
    {
      // paint drawn line
      if( m_currentPos != null )
      {
        final Graphics2D g2 = (Graphics2D) g;

        g2.setStroke( new BasicStroke( 1 ) );
        g2.setColor( new Color( 255, 100, 100 ) );

        final Point currentPoint = MapUtilities.retransform( getMapPanel(), m_currentPos );
        if( m_lineBuilder != null )
          m_lineBuilder.paint( g2, mapPanel.getProjection(), currentPoint );
      }
    }

    final Rectangle bounds = mapPanel.getScreenBounds();

    final String tooltipMsg = getTooltipMessage();

    m_toolTipRenderer.setTooltip( tooltipMsg );
    m_toolTipRenderer.paintToolTip( new Point( 5, bounds.height - 5 ), g, bounds );

    // TODO: validate current line and set warning message
    m_warningRenderer.paintToolTip( new Point( 5, bounds.height - 80 ), g, bounds );
  }

  private String getTooltipMessage( )
  {
    if( m_edit )
      return Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.channeledit.DrawBanklineWidget.0" ); //$NON-NLS-1$
    else
      return Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.channeledit.DrawBanklineWidget.1" ); //$NON-NLS-1$
  }

  private void finishLine( final GM_Curve curve )
  {
    m_data.setBankline( curve, m_side );

    // also set bank line, so we directly can start to edit
    m_bankline = curve;

    reinit();
  }

  @Override
  public void finish( )
  {
    m_toolTipRenderer.setTooltip( "" ); //$NON-NLS-1$
  }

  @Override
  public void keyPressed( final KeyEvent e )
  {
    final int keyCode = e.getKeyCode();

    switch( keyCode )
    {
      case KeyEvent.VK_ESCAPE:
        reinit();
        break;

      case KeyEvent.VK_DELETE:
      case KeyEvent.VK_BACK_SPACE:
        if( m_lineBuilder != null && !m_edit )
          m_lineBuilder.removeLastPoint();
        break;

      // switch mode
      case KeyEvent.VK_SPACE:
        if( m_edit == true )
          m_edit = false;
        else
        {
          m_edit = true;
          if( m_bankline != null & m_lineEditor == null )
            m_lineEditor = new LineGeometryEditor( new GM_Curve[] { m_bankline }, m_bankline, getMapPanel() );
        }
        break;
    }

    repaintMap();
  }
}