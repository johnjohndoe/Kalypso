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
package org.kalypso.ui.rrm.internal.logviewer;

/**
 * Single object which represents single row in CF5 log file.
 * 
 * @author Madan
 */
class LogFileRow
{
  private String level = null;

  private String message = null;

  private String element = null;

  private String parameter = null;

  public LogFileRow( final String l, final String e, final String m, final String p )
  {
    this.level = l;
    this.element = e;
    this.message = m;
    this.parameter = p;
  }

  public String getLevel( )
  {
    return level;
  }

  public String getMessage( )
  {
    return message;
  }

  public String getElement( )
  {
    return element;
  }

  public String getParameter( )
  {
    return parameter;
  }
}
