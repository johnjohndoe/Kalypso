/**
 * TODO: license definieren
 */

package org.kalypso.ogc.gml.widgets;

import java.awt.Graphics;
import java.awt.Point;
import java.util.ArrayList;
import java.util.List;

import org.deegree.model.feature.FeatureType;
import org.deegree.model.feature.FeatureTypeProperty;
import org.deegree.model.feature.event.ModellEvent;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.util.command.ICommand;
import org.kalypso.util.command.ICommandTarget;

/**
 * 
 * @author von Dömming
 */
public class CreateGeometryFeatureWidget extends AbstractWidget
{
  private AbstractWidget myWidget = null;

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#activate(org.kalypso.util.command.ICommandTarget,
   *      org.kalypso.ogc.gml.mapmodel.MapPanel)
   */
  public void activate( ICommandTarget commandPoster, MapPanel mapPanel )
  {
    super.activate( commandPoster, mapPanel );
    setup();
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#clickPopup(java.awt.Point)
   */
  public void clickPopup( Point p )
  {
    if( myWidget != null )
      myWidget.clickPopup( p );
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#dragged(java.awt.Point)
   */
  public void dragged( Point p )
  {
    if( myWidget != null )
      myWidget.dragged( p );
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#leftClicked(java.awt.Point)
   */
  public void leftClicked( Point p )
  {
    if( myWidget != null )
      myWidget.leftClicked( p );
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#leftPressed(java.awt.Point)
   */
  public void leftPressed( Point p )
  {
    if( myWidget != null )
      myWidget.leftPressed( p );
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#leftReleased(java.awt.Point)
   */
  public void leftReleased( Point p )
  {
    if( myWidget != null )
      myWidget.leftReleased( p );
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#middleClicked(java.awt.Point)
   */
  public void middleClicked( Point p )
  {
    if( myWidget != null )
      myWidget.middleClicked( p );
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#middlePressed(java.awt.Point)
   */
  public void middlePressed( Point p )
  {
    if( myWidget != null )
      myWidget.middlePressed( p );
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#middleReleased(java.awt.Point)
   */
  public void middleReleased( Point p )
  {
    if( myWidget != null )
      myWidget.middleReleased( p );
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#moved(java.awt.Point)
   */
  public void moved( Point p )
  {
    if( myWidget != null )
      myWidget.moved( p );
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#paint(java.awt.Graphics)
   */
  public void paint( Graphics g )
  {
    if( myWidget != null )
      myWidget.paint( g );
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#rightClicked(java.awt.Point)
   */
  public void rightClicked( Point p )
  {
    if( myWidget != null )
      myWidget.rightClicked( p );
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#rightPressed(java.awt.Point)
   */
  public void rightPressed( Point p )
  {
    if( myWidget != null )
      myWidget.rightPressed( p );
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#rightReleased(java.awt.Point)
   */
  public void rightReleased( Point p )
  {
    if( myWidget != null )
      myWidget.rightReleased( p );
  }

  public CreateGeometryFeatureWidget()
  {
    setup();
  }

  private void setup()
  {
    final IKalypsoTheme activeTheme = getActiveTheme();
    myWidget = null;
    if( activeTheme != null && activeTheme instanceof IKalypsoFeatureTheme )
    {
      final FeatureType ft = ( (IKalypsoFeatureTheme)activeTheme ).getFeatureType();
      final FeatureTypeProperty[] ftps = ft.getProperties();
      final List geoFtps = new ArrayList();
      // collect available geometry properties
      for( int i = 0; i < ftps.length; i++ )
      {
        if( ftps[i].getType().startsWith( "org.deegree.model.geometry" ) )
          geoFtps.add( ftps[i] );
      }

      //    TODO ask for geometry to create
      //      if( geoFtps.size() > 1 )
      //        ;

      if( geoFtps.size() > 0 )
        setGeometryWidget( (IKalypsoFeatureTheme)activeTheme, (FeatureTypeProperty)geoFtps.get( 0 ) );
    }
  }

  private void setGeometryWidget( final IKalypsoFeatureTheme theme, final FeatureTypeProperty ftp )
  {
    if( ftp.getType().equals( "org.deegree.model.geometry.GM_Point" ) )
      myWidget = new CreatePointFeatureWidget( this, theme, ftp );
    //    else if( ftp.getType().equals( "org.deegree.model.geometry.GM_MultiPoint"
    // ) )
    //      myWidget = new CreateMultipointFeatureWidget();
    //    else if( ftp.getType().equals( "org.deegree.model.geometry.GM_Polygon" )
    // )
    //      myWidget = new CreatePolygonFeatureWidget();
    //    else if( ftp.getType().equals(
    // "org.deegree.model.geometry.GM_MultiSurface" ) )
    //      myWidget = new CreateMultipolygonFeatureWidget();
    //    else if( ftp.getType().equals( "org.deegree.model.geometry.GM_LineString"
    // ) )
    //      myWidget = new CreateLinestringFeatureWidget();
    //    else if( ftp.getType().equals( "org.deegree.model.geometry.GM_MultiCurve"
    // ) )
    //      myWidget = new CreateMultilinestringFeatureWidget();
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.AbstractWidget#performIntern()
   */
  protected ICommand performIntern()
  {
    if( myWidget != null )
      return myWidget.performIntern();
    return null;
  }

  /**
   * @see org.deegree.model.feature.event.ModellEventListener#onModellChange(org.deegree.model.feature.event.ModellEvent)
   */
  public void onModellChange( ModellEvent modellEvent )
  {
    super.onModellChange( modellEvent );
    setup();
  }
}