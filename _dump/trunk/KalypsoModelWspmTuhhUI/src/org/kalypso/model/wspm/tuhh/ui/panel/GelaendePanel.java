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
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilChange;
import org.kalypso.model.wspm.core.profil.changes.ProfilChangeHint;
import org.kalypso.model.wspm.core.profil.changes.ProfilPropertyEdit;
import org.kalypso.model.wspm.ui.profil.operation.ProfilOperation;
import org.kalypso.model.wspm.ui.profil.operation.ProfilOperationJob;
import org.kalypso.model.wspm.ui.view.AbstractProfilView;
import org.kalypso.model.wspm.ui.view.chart.IProfilChartLayer;

import de.openali.odysseus.chart.framework.model.layer.IChartLayer;

/**
 * @author gernot
 * @author kimwerner
 */
public class GelaendePanel extends AbstractProfilView
{
  protected Text m_comment;

  final private IChartLayer m_layer;


  public GelaendePanel( final IProfil profile, final IChartLayer layer )
  {
    super( profile );
    m_layer = layer;
  }

  /**
   * @see org.kalypso.model.wspm.ui.profil.view.AbstractProfilView#doCreateControl(org.eclipse.swt.widgets.Composite,
   *      int)
   */
  @Override
  protected Control doCreateControl( final Composite parent, FormToolkit toolkit, final int style )
  {
    final Composite panel = toolkit.createComposite( parent );
    panel.setLayout( new GridLayout() );

    final Group editgroup = new Group( panel, SWT.NONE );
    editgroup.setText( "Editieren" );
    final GridData gridData = new GridData( SWT.FILL, SWT.CENTER, true, false );
    editgroup.setLayoutData( gridData );
    editgroup.setLayout( new GridLayout() );

    toolkit.adapt( editgroup );

    final Button horzbutton = toolkit.createButton( editgroup, "Horizontal", SWT.CHECK );
    horzbutton.setSelection( allowHorizontal() );
    final Button vertbutton = toolkit.createButton( editgroup, "Vertikal", SWT.CHECK );
    vertbutton.setSelection( allowVertical() );

    horzbutton.addSelectionListener( new SelectionAdapter()
    {
      @Override
      public void widgetSelected( final org.eclipse.swt.events.SelectionEvent e )
      {
        setLayerData( horzbutton.getSelection(), vertbutton.getSelection() );
      }
    } );

    vertbutton.addSelectionListener( new SelectionAdapter()
    {
      @Override
      public void widgetSelected( final org.eclipse.swt.events.SelectionEvent e )
      {
        setLayerData( horzbutton.getSelection(), vertbutton.getSelection() );
      }
    } );
    final Group cg = new Group( panel, SWT.None );
    cg.setText( "Kommentar:" );
    final GridData cgData = new GridData( SWT.FILL, SWT.FILL, true, true );

    cg.setLayoutData( cgData );
    cg.setLayout( new GridLayout() );

    toolkit.adapt( cg );
    m_comment = toolkit.createText( cg, getProfil().getComment(), SWT.MULTI );
    m_comment.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );

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

    return panel;
  }

  protected final void setLayerData( final boolean horz, final boolean vert )
  {
    Integer data = 0;
    if( horz )
      data = data + 1;
    if( vert )
      data = data + 2;
    int old = 0;

    try
    {
      final Object o = m_layer.getData( IProfilChartLayer.VIEW_DATA_KEY );
      old = o == null ? 0 : Integer.valueOf( o.toString() );
    }
    catch( NumberFormatException e )
    {
      old = 0;
    }

    if( old != data )
      m_layer.setData( IProfilChartLayer.VIEW_DATA_KEY, data.toString() );
  }

  private final boolean allowVertical( )
  {
   
    final Object o = m_layer.getData( IProfilChartLayer.VIEW_DATA_KEY );
    if( o == null )
      return true;
    try
    {
      final int i = Integer.valueOf( o.toString() );
      return (i & 2) == 2;
    }
    catch( NumberFormatException e )
    {
      return true;
    }
  }

  private final boolean allowHorizontal( )
  {
    
    final Object o = m_layer.getData( IProfilChartLayer.VIEW_DATA_KEY );
    if( o == null )
      return false;
    try
    {
      final int i = Integer.valueOf( o.toString() );
      return (i & 1) == 1;
    }
    catch( NumberFormatException e )
    {
      return false;
    }
  }

  @Override
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
