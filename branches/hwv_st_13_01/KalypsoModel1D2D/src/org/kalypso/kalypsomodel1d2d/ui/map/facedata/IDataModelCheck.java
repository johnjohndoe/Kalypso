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
package org.kalypso.kalypsomodel1d2d.ui.map.facedata;

/**
 * Interface for classes that provides validation 
 * checks for a data model
 * 
 * @author Patrice Congo
 *
 */
public interface IDataModelCheck
{
  
  
  
  public enum VALIDITY_STATE{
        VALID,
        ACCEPTABLE,
        INVALID
  }
  
  /**
   * Returns the message associated with the current state
   * of the model data being checked .
   * <p>
   * Such a message may provide hints to the user what to do
   * in order t make the data valid
   * </p>
   * 
   * @return the message, or <code>null</code> if none
   */
  public String getMessage();

  /**
   * Returns {@link VALIDITY_STATE} of the data model
   * being checked
   * 
   * @return the validity check of the message being checked
   * @see VALIDITY_STATE
   */
  public VALIDITY_STATE getValidityState();
  
  /**
   * update this check 
   * @param key key for the data being changed
   * @param newData new data to be set
   * @param dataModel new data providing a context for the check
   * @throws IllegalArgumentException if data is not conform
   *            to what is expected by the particular 
   *            implementation 
   */
  public void update(
                String key, 
                Object newData, 
                KeyBasedDataModel dataModel) 
                throws IllegalArgumentException;
}
