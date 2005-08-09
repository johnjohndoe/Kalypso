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
package org.kalypso.ui.calcwizard.bericht;

import java.net.URL;

import org.kalypso.commons.arguments.Arguments;

/**
 * @deprecated
 * @author belger
 */
public abstract class AbstractBerichtExporter implements IBerichtExporter
{
  private Arguments m_arguments = new Arguments();

  private static final String ARG_NAME = "name";

  private URL m_context = null;

  /**
   * @see org.kalypso.ui.calcwizard.bericht.IBerichtExporter#init(URL, Arguments)
   */
  public void init( final URL context, final Arguments arguments )
  {
    m_context = context;
    m_arguments.putAll( arguments );
  }

  /**
   * @return Returns the context.
   */
  protected URL getContext()
  {
    return m_context;
  }

  /**
   * @return Returns the arguments.
   */
  protected Arguments getArguments()
  {
    return m_arguments;
  }

  /**
   * @see org.kalypso.ui.calcwizard.bericht.IBerichtExporter#toString()
   */
  public String toString()
  {
    return m_arguments.getProperty( ARG_NAME, "<unbekannt>" );
  }
}
