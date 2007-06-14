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
package org.kalypso.kalypsomodel1d2d.schema.binding.discr;

import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Object;

/**
 * Interface for classes representing
 * 
 * @author Patrice Congo
 *
 */
public interface IJunctionContext1DToCLine 
                        extends IFE1D2DComplexElement
{
  /**
   * To get the 1D elmement of this junction context
   * @return the 1D element of this juntion context or null
   *            if this junction context does not have a 1D element
   */
  public IElement1D getElement1D();
  
  /**
   * To get the continuity line of this junction context
   * @return the continuity line of this juntion context or null
   *            if this junction context does not have a continuity line
   */
  public ILineElement getContinuityLine();
  
  /**
   * Gets the virtual geometry of this junction context 
   * by recalculating it
   * 
   * @return the recalculated geometry of this junction context
   */
  public GM_Object recalculateElementGeometry( )throws GM_Exception;
  
}
