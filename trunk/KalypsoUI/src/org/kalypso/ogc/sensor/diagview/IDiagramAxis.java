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

/**
 * An axis in a diagram
 * 
 * @author schlienger
 */
public interface IDiagramAxis
{
  public final static String DIRECTION_HORIZONTAL = "horizontal";
  public final static String DIRECTION_VERTICAL = "vertical";
  
  public final static String POSITION_LEFT = "left";
  public final static String POSITION_RIGHT = "right";
  public final static String POSITION_BOTTOM = "bottom";
  public final static String POSITION_TOP = "top";
  
  
  public String getLabel();
  
  public String getUnit();
  
  public String getDirection();
  
  public String getPosition();
  
  public boolean isInverted();

  public String getDataType();
  
  public String getIdentifier();
  
  /**
   * The lower margin is expressed in percent of the whole axis range.
   * 
   * @return the lower margin in percent (for instance 0.07 for 7%) or null if not set
   */
  public Double getLowerMargin();

  /**
   * The upper margin is expressed in percent of the whole axis range.
   * 
   * @return the upper margin in percent (for instance 0.07 for 7%) or null if not set
   */
  public Double getUpperMaring();
  
  /**
   * @return complete Label of this axis (concatenates the label and the unit)
   */
  public String toFullString();
}
