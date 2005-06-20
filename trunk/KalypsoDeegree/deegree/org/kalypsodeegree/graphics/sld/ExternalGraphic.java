/*--------------- Kalypso-Deegree-Header ------------------------------------------------------------

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
 
 
 history:
 
 Files in this package are originally taken from deegree and modified here
 to fit in kalypso. As goals of kalypso differ from that one in deegree
 interface-compatibility to deegree is wanted but not retained always. 
 
 If you intend to use this software in other ways than in kalypso 
 (e.g. OGC-web services), you should consider the latest version of deegree,
 see http://www.deegree.org .

 all modifications are licensed as deegree, 
 original copyright:
 
 Copyright (C) 2001 by:
 EXSE, Department of Geography, University of Bonn
 http://www.giub.uni-bonn.de/exse/
 lat/lon GmbH
 http://www.lat-lon.de
 
 ---------------------------------------------------------------------------------------------------*/

package org.kalypsodeegree.graphics.sld;

import java.awt.image.BufferedImage;
import java.net.URL;

/**
 * The ExternalGraphic element allows a reference to be made to an external graphic file with a Web URL. The
 * OnlineResource sub-element gives the URL and the Format sub-element identifies the expected document MIME type of a
 * successful fetch. Knowing the MIME type in advance allows the styler to select the best- supported format from the
 * list of URLs with equivalent content.
 * <p>
 * ----------------------------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @version $Revision$ $Date$
 */
public interface ExternalGraphic
{

  /**
   * the Format sub-element identifies the expected document MIME type of a successful fetch.
   * 
   * @return Format of the external graphic
   */
  String getFormat();

  /**
   * the Format sub-element identifies the expected document MIME type of a successful fetch. This method sets the
   * format of an ExternalGraphic.
   * 
   * @param format
   *          Format of the external graphic
   */
  void setFormat( String format );

  /**
   * The OnlineResource gives the URL of the external graphic
   * 
   * @return URL of the external graphic
   */
  URL getOnlineResource();

  /**
   * The OnlineResource gives the URL of the external graphic This method sets the OnlineRessource of an
   * ExternalGraphic.
   * 
   * @param onlineResource
   *          URL of the external graphic
   */
  void setOnlineResource( URL onlineResource );

  /**
   * returns the external graphic as an image. this method is not part of the sld specifications but it is added for
   * speed up applications
   * 
   * @return the external graphic as BufferedImage
   */
  BufferedImage getAsImage();

  /**
   * sets the external graphic as an image.
   * 
   * @param image
   *          the external graphic as BufferedImage
   */
  void setAsImage( BufferedImage image );
}