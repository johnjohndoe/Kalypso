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
package org.kalypso.ogc.gml.widgets;

import java.awt.Graphics;
import java.awt.Point;

import org.deegree.graphics.transformation.GeoTransform;
import org.deegree.model.feature.event.ModellEvent;
import org.deegree.model.feature.event.ModellEventListener;
import org.deegree.model.geometry.GM_Envelope;
import org.deegree.model.geometry.GM_Position;
import org.deegree_impl.model.geometry.GeometryFactory;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.util.command.ICommand;
import org.kalypso.util.command.ICommandTarget;

/**
 * @author bce
 */
public abstract class AbstractWidget implements IWidget, ModellEventListener
{
  private MapPanel m_mapPanel = null;

  private ICommandTarget m_commandPoster;

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#activate(org.kalypso.util.command.ICommandTarget,
   *      org.kalypso.ogc.gml.map.MapPanel)
   */
  public void activate( final ICommandTarget commandPoster, final MapPanel mapPanel )
  {
    // unregister Modelllistener
    if( m_mapPanel != null )
    {
      IMapModell mapModell = m_mapPanel.getMapModell();
      if( mapModell != null )
        mapModell.removeModellListener( this );
    }
    // TODO: register modelllistener?
    m_commandPoster = commandPoster;
    m_mapPanel = mapPanel;

    if( m_mapPanel != null )
      m_mapPanel.getMapModell().addModellListener( this );
    // registerModelllistener
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#perform()
   */
  public void perform()
  {
    final ICommand command = performIntern();
    if( command != null )
      m_commandPoster.postCommand( command, null );
  }

  protected abstract ICommand performIntern();

  protected final GM_Position getPosition( Point pixelPoint )
  {
    final GeoTransform transform = m_mapPanel.getProjection();
    GM_Position pixelPos = GeometryFactory.createGM_Position( pixelPoint.getX(), pixelPoint.getY() );
    return transform.getSourcePoint( pixelPos );
  }

  // Helper
  protected final GM_Envelope getDragbox( int mx, int my, int dx )
  {
    if( m_mapPanel == null )
      return null;

    final double ratio = getRatio();

    final GeoTransform transform = m_mapPanel.getProjection();
    double gisMX = transform.getSourceX( mx );
    double gisMY = transform.getSourceY( my );

    double gisX1 = transform.getSourceX( mx - dx );
    double gisDX = gisMX - gisX1;

    double gisDY = gisDX * ratio;

    double gisX2 = gisMX + gisDX;
    double gisY1 = gisMY - gisDY;
    double gisY2 = gisMY + gisDY;

    return GeometryFactory.createGM_Envelope( gisX1, gisY1, gisX2, gisY2 );
  }

  protected final double getRatio()
  {
    final GM_Envelope boundingBox = m_mapPanel.getBoundingBox();

    final double ratio = boundingBox.getHeight() / boundingBox.getWidth();
    return ratio;
  }

  /*
   * returns GM_Envelope for the pixel xmin, ymin, xmax, ymax.
   *  
   */
  protected final GM_Envelope getBox( double x, double y, double x2, double y2 )
  {

    final GeoTransform gt = m_mapPanel.getProjection();
    return GeometryFactory.createGM_Envelope( gt.getSourceX( x ), gt.getSourceY( y ), gt
        .getSourceX( x2 ), gt.getSourceY( y2 ) );
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#clickPopup(java.awt.Point)
   */
  public void clickPopup( Point p )
  {
  // not implemented by default
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#dragged(java.awt.Point)
   */
  public void dragged( Point p )
  {
  // not implemented by default

  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#finish()
   */
  public void finish()
  {
  // not implemented by default
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#leftClicked(java.awt.Point)
   */
  public void leftClicked( Point p )
  {
  // not implemented by default

  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#leftPressed(java.awt.Point)
   */
  public void leftPressed( Point p )
  {

  // not implemented by default
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#leftReleased(java.awt.Point)
   */
  public void leftReleased( Point p )
  {
  // not implemented by default
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#middleClicked(java.awt.Point)
   */
  public void middleClicked( Point p )
  {
  // not implemented by default
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#middlePressed(java.awt.Point)
   */
  public void middlePressed( Point p )
  {
  // not implemented by default
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#middleReleased(java.awt.Point)
   */
  public void middleReleased( Point p )
  {
  // not implemented by default
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#moved(java.awt.Point)
   */
  public void moved( Point p )
  {
  // not implemented by default
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#rightClicked(java.awt.Point)
   */
  public void rightClicked( Point p )
  {
  // not implemented by default
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#rightPressed(java.awt.Point)
   */
  public void rightPressed( Point p )
  {
  // not implemented by default
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#rightReleased(java.awt.Point)
   */
  public void rightReleased( Point p )
  {
  // not implemented by default
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#paint(java.awt.Graphics)
   */
  public void paint( Graphics g )
  {
  // not implemented by default
  }

  protected final MapPanel getMapPanel()
  {
    return m_mapPanel;
  }

  protected final void postCommand( final ICommand command, final Runnable runAfterCommand )
  {
    m_commandPoster.postCommand( command, runAfterCommand );
  }

  public ICommandTarget getCommandTarget()
  {
    return m_commandPoster;
  }

  public void onModellChange( final ModellEvent modellEvent )
  {
  //
  }

  public IKalypsoTheme getActiveTheme()
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

  //  public FeatureType getActiveFeatureType()
  //  {
  //    try
  //    {
  //      return ( getActiveTheme() ).getFeatureType();
  //    }
  //    catch( Exception e )
  //    {
  //      // no active layer
  //      // layer not loaded complete
  //      // not a featurelayer e.g. a wms-layer
  //      return null;
  //    }
  //  }

  //  public KalypsoFeatureLayer[] getAllKalypsoFeatureLayers()
  //  {
  //    List result = new ArrayList();
  //    IKalypsoTheme[] themes = m_mapPanel.getMapModell().getAllThemes();
  //    for( int i = 0; i < themes.length; i++ )
  //    {
  //      IKalypsoLayer layer = themes[i].getLayer();
  //      if( layer != null && layer instanceof KalypsoFeatureLayer )
  //        result.add( layer );
  //    }
  //    return (KalypsoFeatureLayer[])result.toArray( new
  // KalypsoFeatureLayer[result.size()] );
  //  }
}