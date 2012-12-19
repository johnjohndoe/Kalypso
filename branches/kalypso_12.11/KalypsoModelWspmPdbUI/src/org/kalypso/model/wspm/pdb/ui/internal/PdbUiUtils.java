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
package org.kalypso.model.wspm.pdb.ui.internal;

import org.eclipse.core.commands.ExecutionException;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.IWorkbenchPartSite;
import org.eclipse.ui.IWorkbenchWindow;
import org.kalypso.model.wspm.pdb.connect.IPdbConnection;
import org.kalypso.model.wspm.pdb.ui.internal.content.IConnectionViewer;
import org.kalypso.model.wspm.pdb.ui.internal.content.PdbView;
import org.kalypso.model.wspm.pdb.ui.internal.i18n.Messages;
import org.kalypso.model.wspm.pdb.ui.internal.wspm.FindViewRunnable;

/**
 * General helper stuff for PDB-UI
 * 
 * @author Gernot Belger
 */
public final class PdbUiUtils
{
  private static final String ERROR_FAILED_VIEW = Messages.getString( "PdbUiUtils.0" ); //$NON-NLS-1$

  private PdbUiUtils( )
  {
    throw new UnsupportedOperationException();
  }

  public static IPdbConnection getConnectionChecked( final IWorkbenchPart part ) throws ExecutionException
  {
    final IConnectionViewer viewer = getConnectionViewerChecked( part );
    return getConnectionChecked( viewer );
  }

  public static IConnectionViewer getConnectionViewerChecked( final IWorkbenchPart part ) throws ExecutionException
  {
    if( part == null )
      throw new ExecutionException( ERROR_FAILED_VIEW );

    final IWorkbenchPartSite site = part.getSite();
    if( site == null )
      throw new ExecutionException( ERROR_FAILED_VIEW );

    final IWorkbenchWindow window = site.getWorkbenchWindow();
    return getConnectionViewerChecked( window );
  }

  public static IPdbConnection getConnection( final IWorkbenchWindow window )
  {
    final IConnectionViewer viewer = getConnectionViewer( window );
    if( viewer == null )
      return null;

    return viewer.getConnection();
  }

  public static IPdbConnection getConnectionChecked( final IWorkbenchWindow window ) throws ExecutionException
  {
    final IConnectionViewer viewer = getConnectionViewerChecked( window );
    return getConnectionChecked( viewer );
  }

  public static IConnectionViewer getConnectionViewerChecked( final IWorkbenchWindow window ) throws ExecutionException
  {
    final PdbView view = new FindViewRunnable<PdbView>( PdbView.ID, window, true ).execute();
    if( view == null )
      throw new ExecutionException( ERROR_FAILED_VIEW ); //$NON-NLS-1$

    return view;
  }

  public static IConnectionViewer getConnectionViewer( final IWorkbenchWindow window )
  {
    return new FindViewRunnable<PdbView>( PdbView.ID, window, true ).execute();
  }

  public static IConnectionViewer getConnectionViewerChecked( final IWorkbench workbench ) throws ExecutionException
  {
    final IWorkbenchWindow window = workbench.getActiveWorkbenchWindow();
    if( window == null )
      throw new ExecutionException( ERROR_FAILED_VIEW );

    return getConnectionViewerChecked( window );
  }

  private static IPdbConnection getConnectionChecked( final IConnectionViewer viewer ) throws ExecutionException
  {
    final IPdbConnection connection = viewer.getConnection();
    if( connection == null )
      throw new ExecutionException( Messages.getString( "PdbUiUtils.1" ) ); //$NON-NLS-1$

    return connection;
  }
}