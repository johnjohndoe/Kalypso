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
package org.kalypso.eclipse.ui.internal.dialogs;

import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Item;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.ui.internal.dialogs.ContainerCheckedTreeViewer;
import org.kalypso.eclipse.ui.IViewable;

/**
 * ContainerCheckedTreeViewer2 extends <code>ContainerCheckedTreeViewer</code>
 * and allows to update the check state of the items. Objects lying
 * behind the items displayed in this viewer should (but must not)
 * implement the <code>IViewable</code> interface.
 * 
 * @author schlienger
 */
public class ContainerCheckedTreeViewer2 extends ContainerCheckedTreeViewer
{
  public ContainerCheckedTreeViewer2( final Composite parent )
  {
    super( parent );
  }

  public ContainerCheckedTreeViewer2( final Composite parent, final int style )
  {
    super( parent, style );
  }

  public ContainerCheckedTreeViewer2( final Tree tree )
  {
    super( tree );
  }

  /**
   * @see org.eclipse.jface.viewers.TreeViewer#doUpdateItem(org.eclipse.swt.widgets.Item, java.lang.Object)
   */
  protected void doUpdateItem( final Item item, final Object element )
  {
    super.doUpdateItem( item, element );
    
    if( element instanceof IViewable )
    {
      final IViewable v = (IViewable) element;

      setChecked( item, v.isShown() );
    }
  }
}
