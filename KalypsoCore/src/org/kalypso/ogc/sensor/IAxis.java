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
package org.kalypso.ogc.sensor;



/**
 * An axis is used to describe a 'column' of values within
 * a Tupple Model.
 * <p>
 * Two axes are said to be equal when:
 * <ol>
 * <li>the dataclass of their elements is the same</li>
 * <li>their types are identical</li>
 * <li>they have the same unit</li>
 * <li>the value of the key-property is identical</li>
 * </ol>
 * 
 * @author schlienger
 */
public interface IAxis
{
  /**
   * @return the class of the data in this axis
   */
  public Class getDataClass();
  
  /** 
   * @return the application dependent type of this axis
   */
  public String getType();
  
  /** 
   * @return the unit of this axis
   */
  public String getUnit();
  
  /**
   * @return the name of this axis
   */
  public String getName();
  
  /**
   * Returns true when this axis is part of the key of the TuppleModel. Key can be used 
   * to avoid duplicate entries in the model.
   * 
   * @return boolean
   */
  public boolean isKey();
  
  /**
   * Returns true when this axis is persistable when saving the observation
   * to some medium like a file. Some observations generate new axes depending
   * on their characteristics. For instance a WQ-ObservationFilter
   * either simulates a Q or a W axis, depending on the wrapped observation, 
   * and this simulated axis should not be saved along the others (since
   * it can be computed at any time).
   * 
   * @return boolean
   */
  public boolean isPersistable();
}
