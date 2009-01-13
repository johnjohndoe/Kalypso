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
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.forms.widgets.ImageHyperlink;
import org.kalypso.afgui.extension.IKalypsoProjectOpenAction;
import org.kalypso.project.database.client.core.model.ProjectHandler;
import org.kalypso.project.database.client.ui.project.database.IProjectDatabaseUiLocker;

/**
 * Projects hosted at the project database model server
 * 
 * @author Dirk Kuch
 */
public class RemoteProjectRowBuilder extends AbstractProjectRowBuilder implements IProjectRowBuilder
{
  private final boolean m_isExpert;

  public RemoteProjectRowBuilder( final ProjectHandler handler, final IKalypsoProjectOpenAction openAction, final IProjectDatabaseUiLocker locker, final boolean isExpert )
  {
    super( handler, openAction, locker );
    m_isExpert = isExpert;
  }

  /**
   * @see org.kalypso.project.database.client.ui.project.internal.IProjectRowBuilder#render(org.eclipse.swt.widgets.Composite,
   *      org.eclipse.ui.forms.widgets.FormToolkit)
   */
  @Override
  public void render( final Composite parent, final FormToolkit toolkit )
  {
    final Composite body = toolkit.createComposite( parent );
    body.setLayout( new GridLayout( 6, false ) );
    body.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, false ) );

    final ImageHyperlink lnk = toolkit.createImageHyperlink( body, SWT.NONE );
    lnk.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, false ) );

    if( getHandler().getBean().isProjectLockedForEditing() )
    {
      lnk.setImage( IMG_REMOTE_PROJECT_LOCKED );
      lnk.setToolTipText( String.format( "Projekt: \"%s\" befindet sich aktuell in Bearbeitung.", getHandler().getName() ) );
    }
    else
    {
      lnk.setImage( IMG_REMOTE_PROJECT );
      lnk.setToolTipText( String.format( "Projekt: %s", getHandler().getName() ) );
    }

    lnk.setUnderlined( false );
    lnk.setEnabled( false );

    lnk.setText( getHandler().getName() );

    /* info */
    getRemoteInfoLink( body, toolkit, m_isExpert );

    /* import */
    getImportLink( body, toolkit );

    getSpacer( body, toolkit );
    getSpacer( body, toolkit );
    getSpacer( body, toolkit );
  }

}
