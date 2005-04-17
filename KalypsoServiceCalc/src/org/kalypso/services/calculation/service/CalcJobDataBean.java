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
package org.kalypso.services.calculation.service;

import java.io.Serializable;

/**
 * Bean zum Datenaustausch (Eingabe und Ausgabedaten)
 * 
 * @author belger
 */
public class CalcJobDataBean implements Serializable
{
  private String m_path;

  private String m_name;

  private String m_id;

//  private DataHandler m_data;

  public CalcJobDataBean()
  {
  // nur für wspcompile
  }

  /**
   * @param id
   *          ID dieses Datenobjekts
   * @param name
   *          Name, für eventuelle Benutzermeldungen
   * @param path
   *          Ein relativer Pfad. Hinweis, wie der Server, bzw. der Client die
   *          Daten ablegen soll, damit die relativen Pfade erhalten bleiben.
   *          <p>
   *          Bei Senden zum Server: die Datei wird Relativ zum /basedir/INPUT_DIR_NAME/ abgelegt.
   *          </p>
   *          <p>
   *          Bei Senden zum Client: die Datei wird mit diesem Pfad unterhalb
   *          der Rechenvariante abgelegt.
   *          </p>
//   * @param data Die eigentlichen Daten
   *  
   */
  public CalcJobDataBean( final String id, final String name, final String path /*, final DataHandler data */ )
  {
    m_id = id;
    m_name = name;
    m_path = path;
//    m_data = data;
  }

  public final String getId()
  {
    return m_id;
  }

  public final void setId( String id )
  {
    m_id = id;
  }

  public final String getName()
  {
    return m_name;
  }

  public final void setName( String name )
  {
    m_name = name;
  }

  public final String getPath()
  {
    return m_path;
  }

  public final void setPath( String url )
  {
    m_path = url;
  }

  //  public final DataHandler getData()
  //  {
  //    return m_data;
  //  }
  //  public final void setData( final DataHandler data )
  //  {
  //    m_data = data;
  //  }

  /**
   * @see java.lang.Object#toString()
   */
  public String toString()
  {
    return super.toString() + "\n ID: " + m_id + "\n NAME: " + m_name + "\n PATH: " + m_path;
  }
}