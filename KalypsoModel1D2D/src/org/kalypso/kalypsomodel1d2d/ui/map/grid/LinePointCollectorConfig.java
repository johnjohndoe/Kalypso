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
package org.kalypso.kalypsomodel1d2d.ui.map.grid;

import java.awt.Color;

/**
 * Hold config data for a line point collector
 * 
 * @author Patrice Congo
 */
public class LinePointCollectorConfig
{
  private String m_name;

  private Color m_color;

  private int m_pointRectSize;

  private LinePointCollector m_configLinePointCollector;

  public LinePointCollectorConfig( final String name, final Color color, final LinePointCollector linePointCollector )
  {
    m_name = name;
    m_configLinePointCollector = linePointCollector;
    m_color = color;
  }

  public Color getColor( )
  {
    return m_color;
  }

  public void setColor( final Color color )
  {
    m_color = color;
  }

  public LinePointCollector getConfigLinePointCollector( )
  {
    return m_configLinePointCollector;
  }

  public void setConfigLinePointCollector( final LinePointCollector configLinePointCollector )
  {
    m_configLinePointCollector = configLinePointCollector;
  }

  public String getName( )
  {
    return m_name;
  }

  public void setName( final String name )
  {
    m_name = name;
  }

  public int getPointRectSize( )
  {
    return m_pointRectSize;
  }

  public void setPointRectSize( final int pointRectSize )
  {
    m_pointRectSize = pointRectSize;
  }
}