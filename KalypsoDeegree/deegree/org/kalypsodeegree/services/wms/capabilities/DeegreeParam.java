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

import java.net.URL;

import org.deegree.enterprise.Proxy;

/**
 * 
 * 
 * @version $Revision$
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 */
public interface DeegreeParam
{
  /**
   * returns the root directory of the deegree WMS installation, The directory
   * contains the WEB-INF directory and so the deployment descriptor for
   * publishing the WMS to the servlet engine.
   * <p>
   * The WEB-INF directory contains a directory named xml that contains all
   * required XML-Documents that are not addressed with its absolut path in the
   * capabilities/configuration document.
   * 
   * @return directory name
   */
  String getRootDirectory();

  /**
   * returns the default URL of the deegree WMS which is used as default if no
   * other URL is specified in the capabilities/configuration document if
   * required.
   * 
   * @return URL
   */
  URL getDefaultOnlineResource();

  /**
   * returns the cache size used for the WMS in MB. default is 100MB.
   * Datasources that are linked to WMS are not targeted by this value.
   * 
   * @return cache size
   */
  int getCacheSize();

  /**
   * returns the maximum life time of the internal processes (Threads) of the
   * deegree WMS. default is 3600 seconds. Datasources that are linked to WMS
   * are not targeted by this value.
   * 
   * @return
   */
  int getMaxLifeTime();

  /**
   * returns the maximum time the perfoming of a request is allowed to take.
   * default is 15 seconds.
   */
  int getRequestTimeLimit();

  /**
   * returns the maximum map width that can be requested. If the
   * GetMap-Parameter 'WIDTH' extends max map width an exception shall be
   * returned to the client.
   * <p>
   * Default is 1000 Pixel
   */
  int getMaxMapWidth();

  /**
   * returns the maximum map height that can be requested. If the
   * GetMap-Parameter 'HEIGHT' extends max map width an exception shall be
   * returned to the client.
   * <p>
   * Default is 1000 Pixel
   */
  int getMaxMapHeight();

  /**
   * returns the quality of the map for none loss-less image formats. the value
   * ranges from 0 (lowest quality) to 1 (best quality)
   * <p>
   * Default is 0.95
   */
  float getMapQuality();

  /**
   * returns a copy right note to draw at the left side of the maps bottom
   */
  String getCopyRight();

  /**
   * returns the description where to access the gazetteer service associated
   * with the WMS
   */
  GazetteerParam getGazetteer();

  /**
   * returns the URL where the schema definition of the responses to a
   * GetFeatureInfo request is located.
   */
  URL getSchemaLocation();

  /**
   * returns the URL where the DTD defining the OGC WMS capabilities is located
   */
  URL getDTDLocation();

  /**
   * returns the proxy used with the WMS.
   */
  Proxy getProxy();

}