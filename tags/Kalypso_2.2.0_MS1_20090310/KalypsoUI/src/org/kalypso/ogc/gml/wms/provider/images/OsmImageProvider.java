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
package org.kalypso.ogc.gml.wms.provider.images;

import java.awt.Color;
import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.image.BufferedImage;

import org.kalypso.ogc.gml.wms.loader.ICapabilitiesLoader;
import org.kalypsodeegree.model.geometry.GM_Envelope;

/**
 * Fetches images from a OpenStreepMap-Tile-Server
 * 
 * @author Gernot Belger
 */
public class OsmImageProvider implements IKalypsoImageProvider
{
  public OsmImageProvider( )
  {
    // TODO Auto-generated constructor stub
  }

  /**
   * @see org.kalypso.ogc.gml.wms.provider.images.IKalypsoImageProvider#init(java.lang.String, java.lang.String[],
   *      java.lang.String[], java.lang.String, java.lang.String)
   */
  @Override
  public void init( final String themeName, final String[] layers, final String[] styles, final String service, final String localSRS )
  {
    // TODO Auto-generated method stub

  }

  /**
   * @see org.kalypso.ogc.gml.wms.provider.images.IKalypsoImageProvider#getLabel()
   */
  @Override
  public String getLabel( )
  {
    return "OpenStreeMap";
  }

  /**
   * @see org.kalypso.ogc.gml.wms.provider.images.IKalypsoImageProvider#getFullExtent()
   */
  @Override
  public GM_Envelope getFullExtent( )
  {
    return null;
  }

  /**
   * @see org.kalypso.ogc.gml.wms.provider.images.IKalypsoImageProvider#getImage(int, int,
   *      org.kalypsodeegree.model.geometry.GM_Envelope)
   */
  @Override
  public Image getImage( final int width, final int height, final GM_Envelope bbox )
  {
    // TODO:
    // - guess best zoom level
    // - calculate bbox in latlong
    // - translate to osm coordinates
    // - build request url
    // - request image
    // - draw image

    final BufferedImage bi = new BufferedImage( width, height, BufferedImage.TYPE_INT_ARGB );

    final Graphics2D g = bi.createGraphics();

    g.setColor( Color.cyan );

    g.fillRect( 0, 0, width, height );

    g.dispose();

    return bi;
  }

  /**
   * @see org.kalypso.ogc.gml.wms.provider.images.IKalypsoImageProvider#getLoader()
   */
  public ICapabilitiesLoader getLoader( )
  {
    return null;
  }

}
