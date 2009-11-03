/*
 *  This file is part of Kalypso
 *
 *  Copyright (c) 2008 by
 *
 *  Björnsen Beratende Ingenieure GmbH, Koblenz, Germany (Bjoernsen Consulting Engineers), http://www.bjoernsen.de
 *  Technische Universität Hamburg-Harburg, Institut für Wasserbau, Hamburg, Germany
 *  (Technical University Hamburg-Harburg, Institute of River and Coastal Engineering), http://www.tu-harburg.de/wb/
 *
 *  Kalypso is free software: you can redistribute it and/or modify it under the terms  
 *  of the GNU Lesser General Public License (LGPL) as published by the Free Software 
 *  Foundation, either version 3 of the License, or (at your option) any later version.
 *
 *  Kalypso is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied 
 *  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with Kalypso.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.kalypso.model.product.application;

import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.ui.IElementFactory;
import org.eclipse.ui.IMemento;
import org.eclipse.ui.IPersistableElement;
import org.eclipse.ui.model.IWorkbenchAdapter;

/**
 * Wrapper class for workbench root from <code>ResourcesPlugin.getWorkspace().getRoot()</code>. Ensures that the
 * workspace root conforms to IPersistableElement according to <a
 * href="https://bugs.eclipse.org/bugs/show_bug.cgi?id=145233">bug 145233</a>. Also see this <a
 * href="http://dev.eclipse.org/newslists/news.eclipse.platform.rcp/msg16330.html">RCP post</a>.
 * 
 * @author Andreas Goetz
 */
@SuppressWarnings("unchecked")
public class NavigatorRoot implements IAdaptable, IPersistableElement, IElementFactory
{
  public NavigatorRoot( )
  {
  }

  public Object getAdapter( Class adapter )
  {
    if( adapter == IPersistableElement.class )
      return this;

    if( adapter == IWorkbenchAdapter.class )
      return ResourcesPlugin.getWorkspace().getRoot().getAdapter( adapter );

    return null;
  }

  public String getFactoryId( )
  {
    return this.getClass().getCanonicalName();
  }

  public void saveState( IMemento memento )
  {
    return;
  }

  public IAdaptable createElement( IMemento memento )
  {
    return ResourcesPlugin.getWorkspace().getRoot();
  }
}