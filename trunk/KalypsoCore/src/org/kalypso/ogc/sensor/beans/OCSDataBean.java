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
package org.kalypso.ogc.sensor.beans;

import java.io.Serializable;

/**
 * Contains meta-information on the localisation of the data for an observation. OCSDataBean
 * are designed to be used within the Kalypso Observation Web Service API. They are used
 * on both client side and server side.
 * 
 * @author schlienger
 */
public class OCSDataBean implements Serializable
{
  /** Identifier of the observation bean it delivers data for */
  private String m_obsId;

  /** Location of the data, URL which should be accessible for both client and server */
  private String m_location;

  /** Internal id of this ODCDataBean */
  private int m_id;

  public OCSDataBean()
  {
    this( 0, "", "" );
  }

  /**
   * Constructs a new OCSDataBean.
   * 
   * @param id Internal id of this ODCDataBean
   * @param obsId Identifier of the observation bean it delivers data for
   * @param location Location of the data, URL which should be accessible for both client and server
   */
  public OCSDataBean( final int id, final String obsId, final String location )
  {
    m_id = id;
    m_obsId = obsId;
    m_location = location;
  }

  public int getId()
  {
    return m_id;
  }

  public void setId( int id )
  {
    m_id = id;
  }

  public String getObsId()
  {
    return m_obsId;
  }

  public void setObsId( String id )
  {
    m_obsId = id;
  }

  public String getLocation()
  {
    return m_location;
  }

  public void setLocation( String location )
  {
    m_location = location;
  }
}