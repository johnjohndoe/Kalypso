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
 * Data Model for the value elements of observations.
 * 
 * @author schlienger
 */
public interface ITuppleModel
{
  /**
   * @return axis list for which this model delivers elements
   */
  public IAxis[] getAxisList();

  /**
   * Returns the position of the axis in this tupple model
   * 
   * @param axis
   * @return the position of the axis in this tupple model
   * @throws SensorException
   *           when axis not part of this model
   */
  public int getPositionFor( final IAxis axis ) throws SensorException;

  /**
   * @return amount of items in this observation's model
   * @throws SensorException
   */
  public int getCount() throws SensorException;

  /**
   * @param axis
   * @return the range of the given axis for this tupple model
   * @throws SensorException
   */
  public IAxisRange getRangeFor( IAxis axis ) throws SensorException;

  /**
   * @param index
   * @param axis
   * @return element at index for axis
   * 
   * @throws SensorException
   */
  public Object getElement( final int index, final IAxis axis ) throws SensorException;

  /**
   * Sets the element at index for axis.
   * 
   * @param index
   * @param element
   * @param axis
   * 
   * @throws SensorException
   */
  public void setElement( final int index, final Object element, final IAxis axis ) throws SensorException;

  /**
   * Returns the index of the given element in the valueslist for the given axis. Calling this method makes only sense
   * for key axes since other axes can have duplicates. In either case it returns the index of the first element found.
   * 
   * @param element
   * @param axis
   * @return index >= 0 if element is found. Returns -1 if element could not be found.
   * 
   * @throws SensorException
   *           when there are no axis
   */
  public int indexOf( final Object element, final IAxis axis ) throws SensorException;
}
