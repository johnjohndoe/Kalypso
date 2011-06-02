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
package org.kalypso.model.wspm.pdb.ui.internal.preferences;

import org.eclipse.jface.layout.GridLayoutFactory;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.kalypso.contribs.eclipse.jface.wizard.IUpdateable;
import org.kalypso.model.wspm.pdb.connect.IPdbConnection;
import org.kalypso.model.wspm.pdb.ui.internal.admin.ConnectionAdminControl;
import org.kalypso.model.wspm.pdb.ui.internal.content.ConnectionContentControl;
import org.kalypso.model.wspm.pdb.ui.internal.wspm.PdbWspmProject;

/**
 * @author Gernot Belger
 */
public class ConnectionViewer extends Composite
{
  private final IUpdateable m_updateable = new IUpdateable()
  {
    @Override
    public void update( )
    {
      handleUpdate();
    }
  };

  private final IPdbConnection m_connection;

  private ConnectionContentControl m_contentViewer;

  private final PdbWspmProject m_project;

  public ConnectionViewer( final FormToolkit toolkit, final Composite parent, final IPdbConnection connection, final PdbWspmProject project )
  {
    super( parent, SWT.NONE );

    m_connection = connection;
    m_project = project;

    toolkit.adapt( this );
    GridLayoutFactory.swtDefaults().applyTo( this );

    createAdminGroup( toolkit, this ).setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, false ) );
    createPdbView( toolkit, this ).setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );
  }

  private Control createAdminGroup( final FormToolkit toolkit, final Composite parent )
  {
    final Group group = new Group( parent, SWT.NONE );
    toolkit.adapt( group );
    // TODO: nur zeigen, wenn der user admin rechte hat
    group.setText( "Administration" );
    group.setLayout( new FillLayout() );

    new ConnectionAdminControl( toolkit, group, m_connection, m_updateable );

    return group;
  }

  private Control createPdbView( final FormToolkit toolkit, final Composite parent )
  {
    final Group group = new Group( parent, SWT.NONE );
    toolkit.adapt( group );
    group.setText( "Inhalt" );
    group.setLayout( new FillLayout() );

    m_contentViewer = new ConnectionContentControl( toolkit, group, m_connection, m_project );

    return group;
  }

  protected void handleUpdate( )
  {
    if( m_contentViewer != null )
      m_contentViewer.refresh();
  }
}