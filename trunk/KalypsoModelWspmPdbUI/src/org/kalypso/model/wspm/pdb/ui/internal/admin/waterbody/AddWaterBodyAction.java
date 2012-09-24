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
package org.kalypso.model.wspm.pdb.ui.internal.admin.waterbody;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.window.Window;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Shell;
import org.hibernate.Session;
import org.kalypso.contribs.eclipse.jface.action.UpdateableAction;
import org.kalypso.core.status.StatusDialog;
import org.kalypso.model.wspm.pdb.connect.Executor;
import org.kalypso.model.wspm.pdb.connect.PdbConnectException;
import org.kalypso.model.wspm.pdb.connect.command.SaveObjectOperation;
import org.kalypso.model.wspm.pdb.db.mapping.WaterBody;
import org.kalypso.model.wspm.pdb.ui.internal.WspmPdbUiPlugin;
import org.kalypso.model.wspm.pdb.ui.internal.admin.waterbody.WaterBodyControl.Mode;
import org.kalypso.model.wspm.pdb.ui.internal.i18n.Messages;

/**
 * @author Gernot Belger
 */
public class AddWaterBodyAction extends UpdateableAction
{
  private final WaterBodyViewer m_viewer;

  private final Session m_session;

  public AddWaterBodyAction( final Session session, final WaterBodyViewer viewer, final String title )
  {
    m_session = session;
    m_viewer = viewer;

    setText( title );
  }

  @Override
  public void runWithEvent( final Event event )
  {
    final Shell shell = event.widget.getDisplay().getActiveShell();

    final WaterBody newWaterBody = new WaterBody();

    final EditWaterBodyWizard wizard = new EditWaterBodyWizard( m_viewer.getExistingWaterbodies(), newWaterBody, Mode.NEW );
    wizard.setWindowTitle( Messages.getString( "AddWaterBodyAction.0" ) ); //$NON-NLS-1$

    final WizardDialog dialog = new WizardDialog( shell, wizard );
    if( dialog.open() == Window.OK )
    {
      try
      {
        final SaveObjectOperation operation = new SaveObjectOperation( newWaterBody );
        new Executor( m_session, operation ).execute();
        m_viewer.refreshWaterBody( newWaterBody.getName() );
      }
      catch( final PdbConnectException e )
      {
        e.printStackTrace();
        final IStatus status = new Status( IStatus.ERROR, WspmPdbUiPlugin.PLUGIN_ID, e.getLocalizedMessage(), e );
        new StatusDialog( shell, status, wizard.getWindowTitle() ).open();
      }
    }
  }

  @Override
  protected boolean checkEnabled( )
  {
    return true;
  }
}