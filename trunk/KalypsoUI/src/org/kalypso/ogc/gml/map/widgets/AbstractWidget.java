/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

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
package org.kalypso.ogc.gml.map.widgets;

import java.awt.Cursor;
import java.awt.Graphics;
import java.awt.Point;
import java.awt.event.KeyEvent;

import org.eclipse.jface.viewers.ISelection;
import org.kalypso.commons.command.ICommand;
import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.map.IMapPanel;
import org.kalypso.ogc.gml.widgets.IWidget;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

/**
 * @author bce
 */
public abstract class AbstractWidget implements IWidget
{
  private IMapPanel m_mapPanel = null;

  private ICommandTarget m_commandPoster;

  private final String m_name;

  private final String m_toolTip;

  public AbstractWidget( final String name, final String toolTip )
  {
    m_name = name;
    m_toolTip = toolTip;
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#activate(org.kalypso.commons.command.ICommandTarget,
   *      org.kalypso.ogc.gml.map.MapPanel)
   */
  public void activate( final ICommandTarget commandPoster, final IMapPanel mapPanel )
  {
    m_commandPoster = commandPoster;
    m_mapPanel = mapPanel;
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.builders.IGeometryBuilderExtensionProvider#setCursor(java.awt.Cursor)
   */
  public void setCursor( final Cursor cursor )
  {
    getMapPanel().setCursor( cursor );
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#setSelection(org.kalypso.ogc.gml.selection.IFeatureSelection)
   */
  public void setSelection( final ISelection selection )
  {
    // does nothing on default
  }

  /**
   * @param selection
   *          The selection of the part, here the selection of the map which is the selection of the active theme TODO:
   *          maybe it is better to give the whole selection
   * @see org.kalypso.ogc.gml.widgets.IWidget#isActive()
   */
  public boolean canBeActivated( final ISelection selection, final IMapPanel mapPanel )
  {
    return true;
  }

  protected final void postViewCommand( final ICommand command, final Runnable runAfterCommand )
  {
    m_commandPoster.postCommand( command, runAfterCommand );
  }

  protected final GM_Position getPosition( final Point pixelPoint )
  {
    final GeoTransform transform = m_mapPanel.getProjection();
    final GM_Position pixelPos = GeometryFactory.createGM_Position( pixelPoint.getX(), pixelPoint.getY() );
    return transform.getSourcePoint( pixelPos );
  }

  // Helper
  protected final GM_Envelope getDragbox( final int mx, final int my, final int dx )
  {
    if( m_mapPanel == null )
      return null;

    final double ratio = getRatio();

    final GeoTransform transform = m_mapPanel.getProjection();
    final double gisMX = transform.getSourceX( mx );
    final double gisMY = transform.getSourceY( my );

    final double gisX1 = transform.getSourceX( mx - dx );
    final double gisDX = gisMX - gisX1;

    final double gisDY = gisDX * ratio;

    final double gisX2 = gisMX + gisDX;
    final double gisY1 = gisMY - gisDY;
    final double gisY2 = gisMY + gisDY;

    return GeometryFactory.createGM_Envelope( gisX1, gisY1, gisX2, gisY2, m_mapPanel.getMapModell().getCoordinatesSystem() );
  }

  protected final double getRatio( )
  {
    final GM_Envelope boundingBox = m_mapPanel.getBoundingBox();

    final double ratio = boundingBox.getHeight() / boundingBox.getWidth();
    return ratio;
  }

  /*
   * returns GM_Envelope for the pixel xmin, ymin, xmax, ymax.
   */
  protected final GM_Envelope getBox( final double x, final double y, final double x2, final double y2 )
  {

    final GeoTransform gt = m_mapPanel.getProjection();
    return GeometryFactory.createGM_Envelope( gt.getSourceX( x ), gt.getSourceY( y ), gt.getSourceX( x2 ), gt.getSourceY( y2 ), m_mapPanel.getMapModell().getCoordinatesSystem() );
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#clickPopup(java.awt.Point)
   */
  public void clickPopup( final Point p )
  {
    // not implemented by default
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#dragged(java.awt.Point)
   */
  public void dragged( final Point p )
  {
    // not implemented by default
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#finish()
   */
  public void finish( )
  {
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#leftClicked(java.awt.Point)
   */
  public void leftClicked( final Point p )
  {
    // not implemented by default
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#leftPressed(java.awt.Point)
   */
  public void leftPressed( final Point p )
  {
    // not implemented by default
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#leftReleased(java.awt.Point)
   */
  public void leftReleased( final Point p )
  {
    // not implemented by default
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#middleClicked(java.awt.Point)
   */
  public void middleClicked( final Point p )
  {
    // not implemented by default
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#middlePressed(java.awt.Point)
   */
  public void middlePressed( final Point p )
  {
    // not implemented by default
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#middleReleased(java.awt.Point)
   */
  public void middleReleased( final Point p )
  {
    // not implemented by default
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#moved(java.awt.Point)
   */
  public void moved( final Point p )
  {
    // not implemented by default
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#rightClicked(java.awt.Point)
   */
  public void rightClicked( final Point p )
  {
    // not implemented by default
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#rightPressed(java.awt.Point)
   */
  public void rightPressed( final Point p )
  {
    // not implemented by default
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#rightReleased(java.awt.Point)
   */
  public void rightReleased( final Point p )
  {
    // not implemented by default
  }

  /**
   * TODO: give World2Screen Transformation into paint method
   * 
   * @see org.kalypso.ogc.gml.widgets.IWidget#paint(java.awt.Graphics)
   */
  public void paint( final Graphics g )
  {
    // not implemented by default
  }

  /**
   * Causes the map to be repainted. The {@link IWidget#paint(Graphics)} method will be called soon.<br>
   * Does not invalidate the map.<br>
   * Use this method, if the state of the widget changes.
   */
  protected void repaintMap( )
  {
    final IMapPanel panel = getMapPanel();
    if( panel != null )
    {
      panel.repaintMap();
    }
  }

  protected final IMapPanel getMapPanel( )
  {
    return m_mapPanel;
  }

  public ICommandTarget getCommandTarget( )
  {
    return m_commandPoster;
  }

  public IKalypsoTheme getActiveTheme( )
  {
    try
    {
      return m_mapPanel.getMapModell().getActiveTheme();
    }
    catch( final Exception e )
    {
      return null;
    }
  }

  public String getName( )
  {
    return m_name;
  }

  public String getToolTip( )
  {
    return m_toolTip;
  }

  protected void mouseFunctionChanged( )
  {
    //
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#doubleClickedLeft(java.awt.Point)
   */
  public void doubleClickedLeft( final Point p )
  {
    // not implemented by default

  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#doubleClickedRight(java.awt.Point)
   */
  public void doubleClickedRight( final Point p )
  {
    // not implemented by default

  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#keyPressed(java.awt.event.KeyEvent)
   */
  public void keyPressed( final KeyEvent e )
  {
    // not implemented by default
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#keyReleased(java.awt.event.KeyEvent)
   */
  public void keyReleased( final KeyEvent e )
  {
    // not implemented by default
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#keyTyped(java.awt.event.KeyEvent)
   */
  public void keyTyped( final KeyEvent e )
  {
    // not implemented by default
  }
}