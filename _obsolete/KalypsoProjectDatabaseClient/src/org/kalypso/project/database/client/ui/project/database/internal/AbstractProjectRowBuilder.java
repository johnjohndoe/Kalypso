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
package org.kalypso.project.database.client.ui.project.database.internal;

import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.forms.widgets.ImageHyperlink;
import org.kalypso.afgui.extension.IKalypsoProjectOpenAction;
import org.kalypso.afgui.extension.IProjectDatabaseUiLocker;

/**
 * @author kuch
 */
public abstract class AbstractProjectRowBuilder implements IProjectRowBuilder
{

  private final IKalypsoProjectOpenAction m_action;

  private final IProjectDatabaseUiLocker m_locker;

  public AbstractProjectRowBuilder( final IKalypsoProjectOpenAction action, final IProjectDatabaseUiLocker locker )
  {
    m_action = action;
    m_locker = locker;
  }

  protected IKalypsoProjectOpenAction getOpenAction( )
  {
    return m_action;
  }

  protected IProjectDatabaseUiLocker getLocker( )
  {
    return m_locker;
  }

  protected void getSpacer( final Composite parent, final FormToolkit toolkit )
  {
    final ImageHyperlink lnk = toolkit.createImageHyperlink( parent, SWT.NONE );
    lnk.setText( "" ); //$NON-NLS-1$
    final GridData data = new GridData( GridData.FILL, GridData.FILL, false, false );
    data.minimumWidth = data.widthHint = 18;
    lnk.setLayoutData( data );
    lnk.setEnabled( false );
    lnk.setUnderlined( false );
  }

}
