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
package org.kalypso.ogc.sensor.diagview;

import java.util.Collection;

import org.kalypso.ogc.sensor.template.ITemplateEventProvider;

/**
 * Template for diagram
 * 
 * @author schlienger
 */
public interface IDiagramTemplate extends ITemplateEventProvider
{
  public String getTitle();
  public String getLegendName();
  public boolean isShowLegend();
  
  /**
   * @return list of <code>IDiagramAxis</code>
   */
  public Collection getDiagramAxes();
  
  /**
   * Returns the axis that has the given id.
   * 
   * @param diagAxisId
   * @return diagram axis if found or null if not found
   */
  public IDiagramAxis getDiagramAxis( final String diagAxisId );
  
  /**
   * @return list of <code>IDiagramTemplateTheme</code>
   */
  public Collection getThemes();
  
  /**
   * @return list of <code>IDiagramCurve</code>
   */
  public Collection getCurves();
  
  /**
   * Clean all possible resources before object is thrown away
   */
  public void dispose();
}
