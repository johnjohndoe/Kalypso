/*----------------    FILE HEADER  ------------------------------------------

 This file is part of deegree.
 Copyright (C) 2001 by:
 EXSE, Department of Geography, University of Bonn
 http://www.giub.uni-bonn.de/exse/
 lat/lon Fitzke/Fretter/Poth GbR
 http://www.lat-lon.de

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

 Andreas Poth
 lat/lon Fitzke/Fretter/Poth GbR
 Meckenheimer Allee 176
 53115 Bonn
 Germany
 E-Mail: poth@lat-lon.de

 Jens Fitzke
 Department of Geography
 University of Bonn
 Meckenheimer Allee 166
 53115 Bonn
 Germany
 E-Mail: jens.fitzke@uni-bonn.de

 
 ---------------------------------------------------------------------------*/

package org.deegree.services.wms.capabilities;

import java.net.MalformedURLException;
import java.net.URL;

/**
 * The optional <Attribution>element provides a way to identify the source of
 * the map data used in a Layer or collection of Layers. Attribution encloses
 * several optional elements: <OnlineResource>states the data provider's URL;
 * <Title>is a human-readable string naming the data provider; <LogoURL>is the
 * URL of a logo image. Client applications may choose to display one or more of
 * these items. A <Format>element in LogoURL indicates the MIME type of the logo
 * image, and the attributes width and height state the size of the image in
 * pixels.
 * 
 * <p>
 * ----------------------------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @version 2002-03-01
 */
public interface Attribution
{

  /**
   * a human-readable string naming the data providerreturns the title of the
   * attribution.
   */
  String getTitle();

  /**
   * returns the data provider's URL
   */
  URL getOnlineResource() throws MalformedURLException;

  /**
   * returns the URL of a logo image
   */
  LogoURL getLogoURL() throws MalformedURLException;
}