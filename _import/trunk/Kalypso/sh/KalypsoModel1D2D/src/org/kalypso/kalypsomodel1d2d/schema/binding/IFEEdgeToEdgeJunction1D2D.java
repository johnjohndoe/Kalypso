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
package org.kalypso.kalypsomodel1d2d.schema.binding;


/**
 * Interface for classes that represents a 
 * wb1d2d:EdgeToEdgeJunction1D2D element
 * 
 * @author Patrice Congo
 */
public interface IFEEdgeToEdgeJunction1D2D<CT extends IFE1D2DComplexElement, ET extends IFE1D2DEdge> extends IFE1D2DElement<CT, ET>
{
  /**
   * Get the 1D edge of this juction
   * @return the 1D edge of this junction
   */
  public IFE1D2DEdge get1DEdge();
  
  /**
   * To set the 1D edge of this juction.
   * This method also registers junction as container and
   * unregister itself from a previous set edge
   * 
   * @param newEdge the new 1D edge of this junction
   */
  public void set1DEdge(IFE1D2DEdge newEdge);
  
  /**
   * Get the 2D edge of this juction.
   * 
   * @return the 2D edge of this junction
   */
  public IFE1D2DEdge get2DEdge();
  
  /**
   * Get the 2D edge of this junction.
   * 
   * This method also registers junction as container and
   * unregister itself from a previous set edge
   * 
   * @return the 2D edge of this junction
   */
  public void set2DEdge(IFE1D2DEdge new2DEdge);
  
}
