/*--------------- Kalypso-Header ------------------------------------------

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

 --------------------------------------------------------------------------*/

package org.kalypso.auth.scenario;

import java.util.Properties;

/**
 * A default implementation of IScenario. Simply holds the scenario text and description. This class is immutable.
 * 
 * @author schlienger
 */
public class Scenario implements IScenario
{
  public final static IScenario DEFAULT_SCENARIO = new Scenario( ID_DEFAULT_SCENARIO, new Properties() );
  
  private final String m_id;

  private final Properties m_props;

  public Scenario(final String id, final Properties props )
  {
    m_id = id;
    m_props = props;
  }

  /**
   * @see org.kalypso.auth.scenario.IScenario#getName()
   */
  public String getName()
  {
    return getProperty( IScenario.PROP_NAME, "" );
  }

  /**
   * @see org.kalypso.auth.scenario.IScenario#getDescription()
   */
  public String getDescription()
  {
    return getProperty( IScenario.PROP_DESCRIPTION, "" );
  }
  
  /**
   * @see java.lang.Object#toString()
   */
  public String toString()
  {
    return getName();
  }

  /**
   * @see org.kalypso.auth.scenario.IScenario#getId()
   */
  public String getId()
  {
    return m_id;
  }

  /**
   * @see org.kalypso.auth.scenario.IScenario#getProperty(java.lang.String, java.lang.String)
   */
  public String getProperty( final String name, final String defaultValue )
  {
    return m_props.getProperty( name, defaultValue );
  }
}
