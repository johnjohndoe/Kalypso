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

import java.awt.Graphics;
import java.awt.Point;
import java.util.ArrayList;
import java.util.List;

import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.IValuePropertyType;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypsodeegree.model.feature.event.ModellEvent;
import org.kalypsodeegree_impl.tools.GeometryUtilities;

/**
 * TODO: not used. Obsolete, remove?
 * 
 * @author dömming
 */
public class CreateGeometryFeatureWidget extends AbstractWidget
{
  private AbstractWidget myWidget = null;

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#activate(org.kalypso.commons.command.ICommandTarget,
   *      org.kalypso.ogc.gml.map.MapPanel)
   */
  @Override
  public void activate( ICommandTarget commandPoster, MapPanel mapPanel )
  {
    super.activate( commandPoster, mapPanel );
    setup();
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#clickPopup(java.awt.Point)
   */
  @Override
  public void clickPopup( Point p )
  {
    if( myWidget != null )
      myWidget.clickPopup( p );
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#dragged(java.awt.Point)
   */
  @Override
  public void dragged( Point p )
  {
    if( myWidget != null )
      myWidget.dragged( p );
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#leftClicked(java.awt.Point)
   */
  @Override
  public void leftClicked( Point p )
  {
    if( myWidget != null )
      myWidget.leftClicked( p );
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#leftPressed(java.awt.Point)
   */
  @Override
  public void leftPressed( Point p )
  {
    if( myWidget != null )
      myWidget.leftPressed( p );
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#leftReleased(java.awt.Point)
   */
  @Override
  public void leftReleased( Point p )
  {
    if( myWidget != null )
      myWidget.leftReleased( p );
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#middleClicked(java.awt.Point)
   */
  @Override
  public void middleClicked( Point p )
  {
    if( myWidget != null )
      myWidget.middleClicked( p );
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#middlePressed(java.awt.Point)
   */
  @Override
  public void middlePressed( Point p )
  {
    if( myWidget != null )
      myWidget.middlePressed( p );
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#middleReleased(java.awt.Point)
   */
  @Override
  public void middleReleased( Point p )
  {
    if( myWidget != null )
      myWidget.middleReleased( p );
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#moved(java.awt.Point)
   */
  @Override
  public void moved( Point p )
  {
    if( myWidget != null )
      myWidget.moved( p );
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#paint(java.awt.Graphics)
   */
  @Override
  public void paint( Graphics g )
  {
    if( myWidget != null )
      myWidget.paint( g );
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#rightClicked(java.awt.Point)
   */
  @Override
  public void rightClicked( Point p )
  {
    if( myWidget != null )
      myWidget.rightClicked( p );
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#rightPressed(java.awt.Point)
   */
  @Override
  public void rightPressed( Point p )
  {
    if( myWidget != null )
      myWidget.rightPressed( p );
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#rightReleased(java.awt.Point)
   */
  @Override
  public void rightReleased( Point p )
  {
    if( myWidget != null )
      myWidget.rightReleased( p );
  }

  public CreateGeometryFeatureWidget( String name, String tooltip )
  {
    super( name, tooltip );
    setup();
  }

  private void setup( )
  {
    final IKalypsoTheme activeTheme = getActiveTheme();
    myWidget = null;
    if( activeTheme != null && activeTheme instanceof IKalypsoFeatureTheme )
    {
      final IFeatureType ft = ((IKalypsoFeatureTheme) activeTheme).getFeatureType();
      final IPropertyType[] ftps = ft.getProperties();
      final List<IPropertyType> geoFtps = new ArrayList<IPropertyType>();
      // collect available geometry properties
      for( int i = 0; i < ftps.length; i++ )
      {
        if( GeometryUtilities.isGeometry( ftps[i] ) )
          geoFtps.add( ftps[i] );
      }

      // TODO ask for geometry to create
      // if( geoFtps.size() > 1 )
      // ;

      if( geoFtps.size() > 0 )
        setGeometryWidget( (IKalypsoFeatureTheme) activeTheme, (IValuePropertyType) geoFtps.get( 0 ) );
    }
  }

  private void setGeometryWidget( final IKalypsoFeatureTheme theme, final IValuePropertyType ftp )
  {
    if( GeometryUtilities.isPointGeometry( ftp ) )
      myWidget = new CreatePointFeatureWidget( "Gemoetrie-editor", "editieren von " + ftp.getQName().getLocalPart(), this, theme, ftp );
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#perform()
   */
  public void perform( )
  {
    // if( myWidget != null )
    // return myWidget.performIntern();
    // return null;
  }

  /**
   * @see org.kalypsodeegree.model.feature.event.ModellEventListener#onModellChange(org.kalypsodeegree.model.feature.event.ModellEvent)
   */
  @Override
  public void onModellChange( ModellEvent modellEvent )
  {
    super.onModellChange( modellEvent );
    setup();
  }
}