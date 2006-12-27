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
package org.kalypso.model.wspm.tuhh.ui.panel;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.kalypso.model.wspm.core.profil.IProfilChange;
import org.kalypso.model.wspm.core.profil.IProfilEventManager;
import org.kalypso.model.wspm.core.profil.changes.ProfilChangeHint;
import org.kalypso.model.wspm.ui.view.AbstractProfilView;
import org.kalypso.model.wspm.ui.view.ProfilViewData;


/**
 * @author gernot
 * 
 */
public class GelaendePanel extends AbstractProfilView
{
  public GelaendePanel( final IProfilEventManager pem, final ProfilViewData viewdata )
  {
    super( pem, viewdata, null );
  }

  /**
   * @see org.kalypso.model.wspm.ui.profil.view.AbstractProfilView#doCreateControl(org.eclipse.swt.widgets.Composite, int)
   */
  @Override
  protected Control doCreateControl( final Composite parent, final int style )
  {
    final Composite panel = new Composite( parent, SWT.NONE );
    panel.setLayout( new GridLayout( ) );
    
    final Group editgroup = new Group( panel, SWT.NONE );
    editgroup.setText( "Editieren" );
    final GridData gridData = new GridData( SWT.FILL, SWT.CENTER, true, false );
    editgroup.setLayoutData( gridData );
    editgroup.setLayout( new GridLayout() );

    final Button horzbutton = new Button( editgroup, SWT.CHECK );
    horzbutton.setSelection( getViewData().isEdithorz() );
    horzbutton.setText( "Horizontal" );
    horzbutton.addSelectionListener( new SelectionAdapter()
    {
      @Override
      public void widgetSelected( org.eclipse.swt.events.SelectionEvent e )
      {
        getViewData().setEdithorz( horzbutton.getSelection() );
      }
    } );

    final Button vertbutton = new Button( editgroup, SWT.CHECK );
    vertbutton.setSelection( getViewData().isEditvert() );
    vertbutton.setText( "Vertikal" );
    vertbutton.addSelectionListener( new SelectionAdapter()
    {
      @Override
      public void widgetSelected( org.eclipse.swt.events.SelectionEvent e )
      {
        getViewData().setEditvert( vertbutton.getSelection() );
      }
    } );
    
    return panel;
  }

  public void onProfilChanged( final ProfilChangeHint hint, final IProfilChange[] changes )
  {
  }
}
