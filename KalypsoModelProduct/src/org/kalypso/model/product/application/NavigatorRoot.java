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
public class NavigatorRoot implements IAdaptable, IPersistableElement, IElementFactory
{
  public NavigatorRoot( )
  {
  }

  @Override
  public Object getAdapter( final Class adapter )
  {
    if( adapter == IPersistableElement.class )
      return this;

    if( adapter == IWorkbenchAdapter.class )
      return ResourcesPlugin.getWorkspace().getRoot().getAdapter( adapter );

    return null;
  }

  @Override
  public String getFactoryId( )
  {
    return this.getClass().getCanonicalName();
  }

  @Override
  public void saveState( final IMemento memento )
  {
    return;
  }

  @Override
  public IAdaptable createElement( final IMemento memento )
  {
    return ResourcesPlugin.getWorkspace().getRoot();
  }
}