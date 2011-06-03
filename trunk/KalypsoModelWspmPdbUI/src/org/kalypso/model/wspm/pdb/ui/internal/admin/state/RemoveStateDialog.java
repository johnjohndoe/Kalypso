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
package org.kalypso.model.wspm.pdb.ui.internal.admin.state;

import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Shell;
import org.kalypso.model.wspm.pdb.db.mapping.State;
import org.kalypso.model.wspm.pdb.ui.internal.content.ByStateContentProvider;
import org.kalypso.model.wspm.pdb.ui.internal.content.PdbComparator;
import org.kalypso.model.wspm.pdb.ui.internal.content.PdbLabelProvider;

/**
 * @author Gernot Belger
 */
public class RemoveStateDialog extends MessageDialog
{
  private final State m_state;

  public RemoveStateDialog( final Shell shell, final String dialogTitle, final State state )
  {
    super( shell, dialogTitle, null, formatMessage( state ), CONFIRM, new String[] { IDialogConstants.OK_LABEL, IDialogConstants.CANCEL_LABEL }, 0 );

    m_state = state;
  }

  private static String formatMessage( final State state )
  {
    return String.format( "Remove state '%s'?  All referenced cross sections (see below) will be removed as well.\nThis operation cannot be undone.", state.getName() );
  }

  @Override
  protected boolean isResizable( )
  {
    return true;
  }

  @Override
  protected Control createCustomArea( final Composite parent )
  {
    final TreeViewer viewer = new TreeViewer( parent, SWT.BORDER );
    final GridData data = new GridData( SWT.FILL, SWT.FILL, true, true );
    data.heightHint = 200;
    data.widthHint = 200;
    viewer.getControl().setLayoutData( data );

    viewer.setLabelProvider( new PdbLabelProvider() );
    viewer.setContentProvider( new ByStateContentProvider( true ) );
    viewer.setComparator( new PdbComparator() );

    viewer.setInput( m_state.getCrossSections() );

    return viewer.getControl();
  }
}