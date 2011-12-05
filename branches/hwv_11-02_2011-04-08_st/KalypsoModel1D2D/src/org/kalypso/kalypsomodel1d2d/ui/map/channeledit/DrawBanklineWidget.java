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
import java.awt.Stroke;
import java.awt.event.KeyEvent;

import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.ui.map.channeledit.CreateChannelData.SIDE;
import org.kalypso.kalypsomodel1d2d.ui.map.util.PointSnapper;
import org.kalypso.kalypsomodel1d2d.ui.map.util.UtilMap;
import org.kalypso.ogc.gml.map.IMapPanel;
import org.kalypso.ogc.gml.map.utilities.MapUtilities;
import org.kalypso.ogc.gml.map.utilities.tooltip.ToolTipRenderer;
import org.kalypso.ogc.gml.map.widgets.builders.LineGeometryBuilder;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.ogc.gml.widgets.AbstractWidget;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Point;

/**
 * widget to create line geometries<br>
 * 
 * @author Thomas Jung
 */
public class DrawBanklineWidget extends AbstractWidget
{
  private LineGeometryBuilder m_lineBuilder = null;

  private GM_Point m_currentPos = null;

  /**
   * The bankline.
   */
  private GM_Curve m_bankline;

  public static final int SNAPPING_RADIUS = 20;

  private final ToolTipRenderer m_toolTipRenderer = new ToolTipRenderer();

  private final ToolTipRenderer m_warningRenderer = new ToolTipRenderer();

  private final CreateChannelData m_data;

  private final SIDE m_side;

  private boolean m_edit;

  private LineGeometryEditor m_lineEditor = null;
  
  private boolean m_snappingActive = true;

  private PointSnapper m_pointSnapper;

  private IFEDiscretisationModel1d2d m_discModel;

  private boolean m_warning;

  public DrawBanklineWidget( final CreateChannelData channeldata, final SIDE side, final String name, final String toolTip )
  {
    super( name, toolTip );
    m_data = channeldata;
    m_side = side;
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.AbstractWidget#activate(org.kalypso.commons.command.ICommandTarget,
   *      org.kalypso.ogc.gml.map.IMapPanel)
   */
  @Override
  public void activate( final ICommandTarget commandPoster, final IMapPanel mapPanel )
  {
    m_toolTipRenderer.setBackgroundColor( new Color( 1f, 1f, 0.6f, 0.70f ) );
    m_warningRenderer.setBackgroundColor( new Color( 1f, 0.4f, 0.4f, 0.80f ) );

    super.activate( commandPoster, mapPanel );
    
    m_discModel = UtilMap.findFEModelTheme( mapPanel );
    m_pointSnapper = new PointSnapper( m_discModel, mapPanel );
    
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
      m_lineEditor = new LineGeometryEditor( new GM_Curve[] { m_bankline }, getMapPanel() );
  }

  @Override
  public void moved( final Point p )
  {
    if( p == null )
      return;
    final Object newNode = checkNewNode( p );

    if( newNode instanceof IFE1D2DNode )
      m_currentPos = ((IFE1D2DNode) newNode).getPoint();// MapUtilities.retransform( getMapPanel(), ((IFE1D2DNode) newNode).getPoint() );
    else
      m_currentPos = MapUtilities.transform( getMapPanel(), p );

    if( m_edit && m_bankline != null )
      m_lineEditor.moved( m_currentPos );
    else
      getMapPanel().setCursor( Cursor.getPredefinedCursor( Cursor.DEFAULT_CURSOR ) );

    final IMapPanel panel = getMapPanel();
    if( panel != null )
      panel.repaintMap();
  }
  
  private Object checkNewNode( final Point p )
  {
    final IMapPanel mapPanel = getMapPanel();
    if( mapPanel == null )
      return null;

    final GM_Point currentPoint = MapUtilities.transform( mapPanel, p );
    IFE1D2DNode m_snapNode = null;

    if( m_snappingActive )
      m_snapNode = m_pointSnapper == null ? null : m_pointSnapper.moved( currentPoint );

    final Object newNode = m_snapNode == null ? currentPoint : m_snapNode;

    return newNode;
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#dragged(java.awt.Point)
   */
  @Override
  public void dragged( final Point p )
  {
    if( m_edit && m_bankline != null )
      m_lineEditor.dragged( p, getMapPanel() );
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#leftClicked(java.awt.Point)
   */
  @Override
  public void leftPressed( final Point p )
  {
    final IMapPanel mapPanel = getMapPanel();
  
    final Object newNode = checkNewNode( p );

    if( newNode instanceof IFE1D2DNode )
      m_currentPos = ((IFE1D2DNode) newNode).getPoint();//MapUtilities.retransform( mapPanel, ((IFE1D2DNode) newNode).getPoint() );
    else
      m_currentPos = MapUtilities.transform( mapPanel, p );

    /* If we have a node, take this position, else take the current one */
//    final GM_Point currentPos = MapUtilities.transform( mapPanel, p );

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

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#paint(java.awt.Graphics)
   */
  @Override
  public void paint( final Graphics g )
  {
    final IMapPanel mapPanel = getMapPanel();
    if( mapPanel == null )
      return;

    super.paint( g );

    // if edit mode is active, paint edited line
    if( m_edit && m_bankline != null )
      m_lineEditor.paint( g );
    else
    {
      // paint drawn line
      if( m_currentPos != null )
      {
        final Color color = new Color( 255, 100, 100 );
        final Graphics2D g2 = (Graphics2D) g;
        final Stroke oldStroke = g2.getStroke();

        /* paint the snap */
        if( m_pointSnapper != null )
          m_pointSnapper.paint( g );
        
        final float width = 1;
        final BasicStroke basicStroke = new BasicStroke( width );
        g2.setStroke( basicStroke );
        g2.setColor( color );
        final Point currentPoint = MapUtilities.retransform( getMapPanel(), m_currentPos );
        if( m_lineBuilder != null )
          m_lineBuilder.paint( g2, getMapPanel().getProjection(), currentPoint );

        g2.setStroke( oldStroke );
      }

    }
    final Rectangle bounds = mapPanel.getScreenBounds();

    String tooltipMsg = ""; //$NON-NLS-1$
    if( m_edit )
      tooltipMsg = Messages.getString("org.kalypso.kalypsomodel1d2d.ui.map.channeledit.DrawBanklineWidget.0"); //$NON-NLS-1$
    else
      tooltipMsg = Messages.getString("org.kalypso.kalypsomodel1d2d.ui.map.channeledit.DrawBanklineWidget.1"); //$NON-NLS-1$

    m_toolTipRenderer.setTooltip( tooltipMsg );
    m_toolTipRenderer.paintToolTip( new Point( 5, bounds.height - 5 ), g, bounds );

    if( m_warning )
      m_warningRenderer.paintToolTip( new Point( 5, bounds.height - 80 ), g, bounds );

  }

  /**
   * @see org.kalypso.ogc.gml.widgets.AbstractWidget#leftReleased(java.awt.Point)
   */
  @Override
  public void leftReleased( final Point p )
  {
    if( m_edit && m_lineEditor != null )
    {
      final GM_Curve curve = m_lineEditor.finish();
      if( curve != null )
        finishLine( curve );
    }
    super.leftReleased( p );
  }

  @Override
  public void doubleClickedLeft( final Point p )
  {
    if( m_lineBuilder != null )
    {
      try
      {
        final GM_Curve curve = (GM_Curve) m_lineBuilder.finish();
        finishLine( curve );
        reinit();
      }
      catch( final Exception e )
      {
        e.printStackTrace();
        reinit();
      }
    }
  }

  public void finishLine( final GM_Curve curve )
  {
    m_data.setBankline( curve, m_side );

    reinit();
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.AbstractWidget#finish()
   */
  @Override
  public void finish( )
  {
    m_toolTipRenderer.setTooltip( "" ); //$NON-NLS-1$
    super.finish();
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.AbstractWidget#keyPressed(java.awt.event.KeyEvent)
   */
  @Override
  public void keyPressed( final KeyEvent e )
  {
    final int keyCode = e.getKeyCode();

    switch( keyCode )
    {
      // reset
      case KeyEvent.VK_ESCAPE:
        reinit();
        break;

      case KeyEvent.VK_SHIFT:
        // TODO; insert points to current line
        break;

      case KeyEvent.VK_DELETE:
        // TODO; remove snapped point from current line
        break;

      // switch mode
      case KeyEvent.VK_SPACE:
        if( m_edit == true )
          m_edit = false;
        else
        {
          m_edit = true;
          if( m_bankline != null & m_lineEditor == null )
            m_lineEditor = new LineGeometryEditor( new GM_Curve[] { m_bankline }, getMapPanel() );
        }
        break;
    }
    getMapPanel().repaintMap();
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#keyReleased(java.awt.event.KeyEvent)
   */
  @Override
  public void keyReleased( final KeyEvent e )
  {
    super.keyReleased( e );
  }

}