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
package org.deegree_impl.clients.wcasclient.configuration;

import java.net.URL;

/**
 * The class encapsulates the configuration informations required for offering
 * download functions to the catalog service.
 * 
 * @version $Revision$
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 */
public class Download
{
  private String mailFrom = null;

  private String mailHost = null;

  private String mailSubject = null;

  private String storageDirectory = null;

  private URL onlineResource = null;

  private int cleanerInterval = 0;

  private int lifeTime = 0;

  /**
   * Creates a new Download object.
   * 
   * @param cleanerInterval
   * @param lifeTime
   * @param storageDirectory
   * @param onlineResource
   * @param mailFrom
   * @param mailSubject
   * @param mailHost
   */
  public Download( int cleanerInterval, int lifeTime, String storageDirectory, URL onlineResource,
      String mailFrom, String mailSubject, String mailHost )
  {
    this.cleanerInterval = cleanerInterval;
    this.lifeTime = lifeTime;
    this.storageDirectory = storageDirectory;
    this.onlineResource = onlineResource;
    this.mailFrom = mailFrom;
    this.mailSubject = mailSubject;
    this.mailHost = mailHost;
  }

  /**
   * 
   * 
   * @return
   */
  public int getCleanerInterval()
  {
    return cleanerInterval;
  }

  /**
   * 
   * 
   * @param cleanerInterval
   */
  public void setCleanerInterval( int cleanerInterval )
  {
    this.cleanerInterval = cleanerInterval;
  }

  /**
   * 
   * 
   * @return
   */
  public int getLifeTime()
  {
    return lifeTime;
  }

  /**
   * 
   * 
   * @param lifeTime
   */
  public void setLifeTime( int lifeTime )
  {
    this.lifeTime = lifeTime;
  }

  /**
   * 
   * 
   * @return
   */
  public String getStorageDirectory()
  {
    return storageDirectory;
  }

  /**
   * 
   * 
   * @param storageDirectory
   */
  public void setStorageDirectory( String storageDirectory )
  {
    this.storageDirectory = storageDirectory;
  }

  /**
   * 
   * 
   * @return
   */
  public URL getOnlineResource()
  {
    return onlineResource;
  }

  /**
   * 
   * 
   * @param onlineResource
   */
  public void setOnlineResource( URL onlineResource )
  {
    this.onlineResource = onlineResource;
  }

  /**
   * 
   * 
   * @return
   */
  public String getMailFrom()
  {
    return mailFrom;
  }

  /**
   * 
   * 
   * @param mailFrom
   */
  public void setMailFrom( String mailFrom )
  {
    this.mailFrom = mailFrom;
  }

  /**
   * 
   * 
   * @return
   */
  public String getMailSubject()
  {
    return mailSubject;
  }

  /**
   * 
   * 
   * @param mailSubject
   */
  public void setMailSubject( String mailSubject )
  {
    this.mailSubject = mailSubject;
  }

  /**
   * 
   * 
   * @return
   */
  public String getMailHost()
  {
    return mailHost;
  }

  /**
   * 
   * 
   * @param mailHost
   */
  public void setMailHost( String mailHost )
  {
    this.mailHost = mailHost;
  }
}