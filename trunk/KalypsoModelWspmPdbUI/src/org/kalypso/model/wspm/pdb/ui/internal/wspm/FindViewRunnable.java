/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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
package org.kalypso.model.wspm.pdb.ui.internal.wspm;

import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.IViewPart;
import org.eclipse.ui.IViewReference;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;

/**
 * Helps to access a view in the ui thread.
 * 
 * @author Gernot Belger
 */
public class FindViewRunnable<T extends IViewPart> implements Runnable
{
  private final String m_viewID;

  private final IWorkbenchWindow m_window;

  private final boolean m_restore;

  private T m_view;

  public FindViewRunnable( final String viewID, final IWorkbenchWindow window, final boolean restoreView )
  {
    m_viewID = viewID;
    m_window = window;
    m_restore = restoreView;
  }

  public T execute( )
  {
    final Display display = m_window.getShell().getDisplay();
    display.syncExec( this );
    return m_view;
  }

  @SuppressWarnings("unchecked")
  @Override
  public void run( )
  {
    final IWorkbenchPage page = m_window.getActivePage();
    final IViewReference viewReference = page.findViewReference( m_viewID );
    m_view = (T) viewReference.getView( m_restore );
  }
}