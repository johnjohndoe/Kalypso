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
package org.kalypso.model.wspm.pdb.ui.internal;

import org.eclipse.core.commands.ExecutionException;
import org.eclipse.ui.IWorkbenchWindow;
import org.kalypso.model.wspm.pdb.connect.IPdbConnection;
import org.kalypso.model.wspm.pdb.ui.internal.preferences.PdbView;
import org.kalypso.model.wspm.pdb.ui.internal.wspm.FindViewRunnable;


/**
 * General helper stuff for PDB-UI
 * 
 * @author Gernot Belger
 */
public final class PdbUiUtils
{
  private PdbUiUtils( )
  {
    throw new UnsupportedOperationException();
  }

  public static IPdbConnection getConnectionChecked( final IWorkbenchWindow window ) throws ExecutionException
  {
    final PdbView view = new FindViewRunnable<PdbView>( PdbView.ID, window, true ).execute();
    if( view == null )
      throw new ExecutionException( "Failed to find Cross Section Database View" );

    final IPdbConnection connection = view.getConnection();
    if( connection == null )
      throw new ExecutionException( "Not connected to database" );

    return connection;
  }
}