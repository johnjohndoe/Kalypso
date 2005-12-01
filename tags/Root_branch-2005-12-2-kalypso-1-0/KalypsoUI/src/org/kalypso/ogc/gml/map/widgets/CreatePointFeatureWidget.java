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

import java.awt.Point;

import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypsodeegree.model.feature.FeatureTypeProperty;

/**
 * @author vDoemming
 */
public class CreatePointFeatureWidget extends AbstractWidget
{
  //  private Point myPoint = null;

  //  private final FeatureTypeProperty myFtp;

  private final CreateGeometryFeatureWidget myParentWidget;

  //  private final IKalypsoFeatureTheme m_theme;

  public CreatePointFeatureWidget( String name, String tooltip, CreateGeometryFeatureWidget parentWidget,
      final IKalypsoFeatureTheme theme, FeatureTypeProperty ftp )
  {
    super( name, tooltip );
    myParentWidget = parentWidget;
    theme.getClass();
    ftp.getClass();
    //    m_theme = theme;
    //    myFtp = ftp;
  }

  public void leftPressed( Point p )
  {
    //    myPoint = p;
    myParentWidget.perform();
  }

  /**
   * 
   * @see org.kalypso.ogc.gml.widgets.IWidget#perform()
   */
  public void perform()
  {
  //    if( myPoint != null )
  //    {
  //      final Feature feature = FeatureFactory.createFeature( "x",
  // m_theme.getFeatureType() );
  //      final GM_Position position = myParentWidget.getPosition( myPoint );
  //      final CS_CoordinateSystem coordinatesSystem =
  // myLayer.getCoordinatesSystem();
  //      final GM_Object geometry = GeometryFactory.createGM_Point( position,
  // coordinatesSystem );
  //      final FeatureProperty fp = FeatureFactory.createFeatureProperty(
  // myFtp.getName(), geometry );
  //      feature.setProperty( fp );
  //      return new CreateFeatureCommand( myLayer, new Feature[]
  //      { feature } );
  //    }
  }
}