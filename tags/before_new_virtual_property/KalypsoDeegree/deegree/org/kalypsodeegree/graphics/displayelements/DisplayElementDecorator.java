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
package org.kalypsodeegree.graphics.displayelements;

/**
 * Interface for classes that decorate a {@link DisplayElement}
 * to change (in general augments) it drawing behavior.
 * 
 * It is specialy use in {@link org.kalypsodeegree_impl.graphics.displayelements.DisplayElementFactory#buildDisplayElement(org.kalypsodeegree.model.feature.Feature, org.kalypsodeegree.graphics.sld.Symbolizer, org.kalypsodeegree.model.feature.GMLWorkspace)}
 * in association with the adapter framework to automaticaly decorate
 * the configured display element with a feature specific display element 
 * 
 * @author Patrice Congo
 * @see DisplayElement
 * @see org.kalypsodeegree_impl.graphics.displayelements.DisplayElementFactory
 *
 */
public interface DisplayElementDecorator extends DisplayElement
{
  /**
   * Sets the display to decorate
   * @param decorated the display to decorate
   */
  public void setDecorated(DisplayElement decorated);
  
  /**
   * To get the {@link DisplayElement} which is being decorated
   * @return the decorated display element
   */
  public DisplayElement getDecorated();
}
