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

import java.awt.Color;

import org.kalypso.eclipse.ui.IViewable;


/**
 * Represents a curve in a diagram
 * 
 * @author schlienger
 */
public interface IDiagramCurve extends IViewable
{
  /**
   * @return name of the curve as displayed in the diagram
   */
  public String getName();
  public void setName( String string );
  
  /**
   * @return list of mappings between diagram axes and observation axes
   */
  public IAxisMapping[] getMappings();
  
  /**
   * @return theme to which this curve belongs to
   */
  public IDiagramTemplateTheme getTheme();

  /**
   * @return color of the curve
   */
  public Color getColor();

  public void setShown( boolean shown);
}
