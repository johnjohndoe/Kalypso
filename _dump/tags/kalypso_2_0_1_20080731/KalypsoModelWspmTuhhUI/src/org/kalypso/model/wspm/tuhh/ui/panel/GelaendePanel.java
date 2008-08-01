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
import org.eclipse.swt.events.FocusAdapter;
import org.eclipse.swt.events.FocusEvent;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Text;
import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilChange;
import org.kalypso.model.wspm.core.profil.changes.ProfilChangeHint;
import org.kalypso.model.wspm.core.profil.changes.ProfilPropertyEdit;
import org.kalypso.model.wspm.ui.profil.operation.ProfilOperation;
import org.kalypso.model.wspm.ui.profil.operation.ProfilOperationJob;
import org.kalypso.model.wspm.ui.view.AbstractProfilView;
import org.kalypso.model.wspm.ui.view.ProfilViewData;

/**
 * @author gernot
 */
public class GelaendePanel extends AbstractProfilView
{
  protected Text m_comment;

  public GelaendePanel( final IProfil profile, final ProfilViewData viewdata )
  {
    super( profile, viewdata );
  }

  /**
   * @see org.kalypso.model.wspm.ui.profil.view.AbstractProfilView#doCreateControl(org.eclipse.swt.widgets.Composite,
   *      int)
   */
  @Override
  protected Control doCreateControl( final Composite parent, final int style )
  {
    final Composite panel = new Composite( parent, SWT.NONE );
    panel.setLayout( new GridLayout() );

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
      public void widgetSelected( final org.eclipse.swt.events.SelectionEvent e )
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
      public void widgetSelected( final org.eclipse.swt.events.SelectionEvent e )
      {
        getViewData().setEditvert( vertbutton.getSelection() );
      }
    } );
    final Group cg = new Group( panel, SWT.None );
    cg.setText( "Kommentar:" );
    final GridData cgData = new GridData( SWT.FILL, SWT.FILL, true, true );

    cg.setLayoutData( cgData );
    cg.setLayout( new GridLayout() );

    m_comment = new Text( cg, SWT.MULTI + SWT.H_SCROLL + SWT.V_SCROLL + SWT.BORDER );

    m_comment.addFocusListener( new FocusAdapter()
    {
      /**
       * @see org.eclipse.swt.events.FocusAdapter#focusLost(org.eclipse.swt.events.FocusEvent)
       */
      @Override
      public void focusLost( final FocusEvent e )
      {
        final String comment = m_comment.getText();
        if( comment != null && !comment.equals( getProfil().getComment() ) )
        {
          /*
           * we need both methods to stay synchronized with featureView
           */
          getProfil().setComment( m_comment.getText() );
          final ProfilOperation operation = new ProfilOperation( "", getProfil(), new ProfilPropertyEdit( getProfil(), IWspmConstants.PROFIL_PROPERTY_COMMENT, m_comment.getText() ), true );
          new ProfilOperationJob( operation ).schedule();
        }
      }
    } );
    m_comment.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );
    if( getProfil().getComment() != null )
      m_comment.setText( getProfil().getComment() );
    return panel;
  }

  public void onProfilChanged( final ProfilChangeHint hint, final IProfilChange[] changes )
  {
    if( hint.isProfilPropertyChanged() )
    {
      final Control control = getControl();
      if( control != null && !control.isDisposed() )
        control.getDisplay().asyncExec( new Runnable()
        {
          public void run( )
          {
            m_comment.setText( getProfil().getComment() );
          }
        } );
    }
  }
}
