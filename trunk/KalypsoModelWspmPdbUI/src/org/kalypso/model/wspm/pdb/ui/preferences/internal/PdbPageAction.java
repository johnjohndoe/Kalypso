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
package org.kalypso.model.wspm.pdb.ui.preferences.internal;

import org.eclipse.jface.action.Action;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Shell;
import org.kalypso.model.wspm.pdb.connect.IPdbConnectInfo;

/**
 * @author Gernot Belger
 */
abstract class PdbPageAction extends Action
{
  private final WspmPdbPreferencePage m_page;

  public PdbPageAction( final String text, final WspmPdbPreferencePage page )
  {
    super( text );

    m_page = page;
  }

  private IPdbConnectInfo getInfo( )
  {
    return m_page.getSelectedItem();
  }

  protected WspmPdbPreferencePage getPage( )
  {
    return m_page;
  }

  public void update( )
  {
    setEnabled( checkEnabled( getInfo() ) );
  }

  @Override
  public void runWithEvent( final Event event )
  {
    final Shell shell = event.display.getActiveShell();
    doRun( shell, getInfo() );
  }

  protected abstract void doRun( Shell shell, IPdbConnectInfo info );

  protected abstract boolean checkEnabled( final IPdbConnectInfo info );
}