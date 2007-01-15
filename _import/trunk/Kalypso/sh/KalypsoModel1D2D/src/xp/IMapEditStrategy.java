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



import org.eclipse.core.resources.IResource;
import org.eclipse.swt.widgets.Composite;
import org.kalypso.ogc.gml.widgets.IWidget;

/**
 * This interface is for classes which provide a service for
 * editing the map content.
 * Such a class typicaly maps a the mouse and key events
 * it receives according to an internal specification that leeds to
 * the creation or modification of a object in the map 
 * (e.g. a feature).
 * The event will be vorwarded to this {@link IMapEditSpecification}
 * using the {@link ActivateModelMapEditProtocolView} and 
 * the {@link ModelMapEditProtocolView}.
 * 
 * complex Specification (like the Main channel builder) are required 
 * to provide a graphical representation of themselse for the sake 
 * of guiding the user. This is done by invoking the 
 * {@link #createControl(Composite)} method 
 *  
 * 
 * @author Patrice Congo
 */
public interface IMapEditStrategy extends IWidget
{
  /**
   * Create the gui representation of the protocol
   *  @param parent the parent composite which provide
   *    a client area for the {@link IProtocol} to layout
   *    its visual representation
   */
  public void createControl(Composite parent);
  
  /**
   * Makes a 
   * Return true if this {@link IProtocol} possesses 
   * a visual representation
   * @return 
   */
  public boolean hasVisualRepresentation();
  
  /**
   * Returns the icon {@link IResource}
   */
  public IResource getIconResource();
  
  /**
   * Config this map strategy with its icon's {@link IResource}
   * 
   * @param iconResource the resource corresponding to this icon
   */
  public void setIconResource(IResource iconResource);
  
  /**
   * Config this map edit strategy with a name
   * @param name the name for this tooltip
   * @throws throws an {@link IllegalArgumentException} if the
   *  passed name is null or empty
   */
  public void setName(String name) throws IllegalArgumentException;
  
  /**
   * Config this map strategy with a tooltip
   * @param tooltip the tooltip text for this map strategy
   */
  public void setToolTip(String tooltip);
}
