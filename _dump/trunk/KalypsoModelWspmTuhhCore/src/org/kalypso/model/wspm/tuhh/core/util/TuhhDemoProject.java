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
package org.kalypso.model.wspm.tuhh.core.util;

import java.net.URL;

/**
 * Objects of this class represent extensions of the org.kalypso.model.wspm.tuhh.core.demoProject extension-point.
 * 
 * @author Gernot Belger
 */
public class TuhhDemoProject
{
  private final String m_label;

  private final String m_projectName;

  private final String m_icon;

  private final URL m_data;

  private final String m_description;

  public TuhhDemoProject( final String label, final String projectName, final String description, final String icon, final URL data )
  {
    m_label = label;
    m_projectName = projectName;
    m_description = description;
    m_icon = icon;
    m_data = data;
  }

  public String getLabel( )
  {
    return m_label;
  }

  public String getProjectName( )
  {
    return m_projectName;
  }

  public String getIcon( )
  {
    return m_icon;
  }

  public URL getData( )
  {
    return m_data;
  }

  public String getDescription( )
  {
    return m_description;
  }

}
