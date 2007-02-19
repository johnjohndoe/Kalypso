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
package xp;

import java.awt.Color;

import org.eclipse.jface.preference.IPreferenceStore;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;



/**
 * Hold config data for a line point collector
 * 
 * @author Patrice Congo
 */
public class LinePointCollectorConfig
{
  
  
  private String name;
  private Color color;
  private int pointRectSize;
  private LinePointCollector configLinePointCollector;
  
  public LinePointCollectorConfig(
            String name,
            Color color,
            LinePointCollector linePointCollector)
  {
    this.name=name;
    this.configLinePointCollector = configLinePointCollector;
    this.color=color;
  }
  
  

  public Color getColor( )
  {
    return color;
  }

  public void setColor( Color color )
  {
    this.color = color;
  }

  public LinePointCollector getConfigLinePointCollector( )
  {
    return configLinePointCollector;
  }

  public void setConfigLinePointCollector( LinePointCollector configLinePointCollector )
  {
    this.configLinePointCollector = configLinePointCollector;
  }

  public String getName( )
  {
    return name;
  }

  public void setName( String name )
  {
    this.name = name;
  }
  
   public int getPointRectSize( )
  {
    return pointRectSize;
  }
   
  public void setPointRectSize( int pointRectSize )
  {
    this.pointRectSize = pointRectSize;
  }
  
}
