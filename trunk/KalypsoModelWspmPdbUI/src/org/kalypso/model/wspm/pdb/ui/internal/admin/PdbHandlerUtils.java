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
package org.kalypso.model.wspm.pdb.ui.internal.admin;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.handlers.HandlerUtil;
import org.hibernate.Session;
import org.kalypso.model.wspm.pdb.connect.IPdbConnection;
import org.kalypso.model.wspm.pdb.connect.PdbConnectException;
import org.kalypso.model.wspm.pdb.db.mapping.WaterBody;
import org.kalypso.model.wspm.pdb.ui.internal.content.IConnectionViewer;

/**
 * @author Gernot Belger
 */
public class PdbHandlerUtils
{
  public static IConnectionViewer getConnectionViewerChecked( final ExecutionEvent event ) throws ExecutionException
  {
    final IConnectionViewer viewer = getConnectionViewer( event );
    if( viewer == null )
      throw new ExecutionException( "Unable to find pdb view" ); //$NON-NLS-1$
    return viewer;
  }

  private static IConnectionViewer getConnectionViewer( final ExecutionEvent event )
  {
    final IWorkbenchPart part = HandlerUtil.getActivePart( event );
    if( part instanceof IConnectionViewer )
      return (IConnectionViewer) part;

    return null;
  }

  public static Session aquireSession( final IConnectionViewer viewer ) throws ExecutionException
  {
    try
    {
      final IPdbConnection connection = viewer.getConnection();
      return connection.openSession();
    }
    catch( final PdbConnectException e )
    {
      e.printStackTrace();
      throw new ExecutionException( "Failed to open database session", e );
    }
  }

  public static WaterBody getSelectedWaterBodyChecked( final ExecutionEvent event ) throws ExecutionException
  {
    final WaterBody waterBody = getSelectedWaterBody( event );
    if( waterBody == null )
      throw new ExecutionException( "Failed to find selected water body" ); //$NON-NLS-1$
    return waterBody;
  }

  private static WaterBody getSelectedWaterBody( final ExecutionEvent event )
  {
    final ISelection selection = HandlerUtil.getCurrentSelection( event );
    if( !(selection instanceof IStructuredSelection) )
      return null;

    final Object element = ((IStructuredSelection) selection).getFirstElement();
    if( element instanceof WaterBody )
      return (WaterBody) element;

    return null;
  }
}
