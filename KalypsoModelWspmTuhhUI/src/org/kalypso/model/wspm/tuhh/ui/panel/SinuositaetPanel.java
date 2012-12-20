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

import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.ComboViewer;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.FocusEvent;
import org.eclipse.swt.events.FocusListener;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.kalypso.contribs.eclipse.swt.events.DoubleModifyListener;
import org.kalypso.contribs.java.lang.NumberUtils;
import org.kalypso.model.wspm.core.profil.IProfile;
import org.kalypso.model.wspm.core.profil.changes.ProfileChangeHint;
import org.kalypso.model.wspm.tuhh.core.profile.sinuositaet.SINUOSITAET_GERINNE_ART;
import org.kalypso.model.wspm.tuhh.core.profile.sinuositaet.SINUOSITAET_KENNUNG;
import org.kalypso.model.wspm.tuhh.core.profile.sinuositaet.SinuositaetProfileObject;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;
import org.kalypso.model.wspm.ui.view.AbstractProfilView;

/**
 * @author Kim Werner
 */
public class SinuositaetPanel extends AbstractProfilView
{
  private FormToolkit m_toolkit = null;

  protected Composite m_propPanel;

  protected ComboViewer m_kennung;

  protected ComboViewer m_gerinne;

  protected Text m_lf;

  protected Text m_sn;

  protected final SinuositaetProfileObject m_sinuositaet;

  public SinuositaetPanel( final IProfile profile )
  {
    super( profile );

    final SinuositaetProfileObject[] sins = profile.getProfileObjects( SinuositaetProfileObject.class );
    m_sinuositaet = sins.length == 0 ? null : sins[0];
  }

  @Override
  protected Control doCreateControl( final Composite parent, final FormToolkit toolkit )
  {
    m_toolkit = toolkit;
    m_propPanel = m_toolkit.createComposite( parent );

    if( m_propPanel == null )
      m_toolkit.createText( m_propPanel, Messages.getString( "SinuositaetPanel_0" ) ); //$NON-NLS-1$
    else
    {
      m_propPanel.setLayout( new GridLayout( 2, false ) );
      createPropertyPanel();
    }

    updateControls();

    return m_propPanel;
  }

  protected void createPropertyPanel( )
  {
    final Display display = m_propPanel.getDisplay();
    final Color goodColor = display.getSystemColor( SWT.COLOR_BLACK );
    final Color badColor = display.getSystemColor( SWT.COLOR_RED );
    final DoubleModifyListener doubleModifyListener = new DoubleModifyListener( goodColor, badColor );

    m_toolkit.createLabel( m_propPanel, m_sinuositaet.getPropertyLabel( SinuositaetProfileObject.PROPERTY_KENNUNG ) );
    m_kennung = new ComboViewer( m_propPanel );
    m_kennung.setContentProvider( new ArrayContentProvider() );
    m_kennung.setInput( SINUOSITAET_KENNUNG.values() );
    m_kennung.addSelectionChangedListener( new ISelectionChangedListener()
    {
      @Override
      public void selectionChanged( final SelectionChangedEvent event )
      {
        final IStructuredSelection selection = (IStructuredSelection)event.getSelection();
        final SINUOSITAET_KENNUNG kenn = (SINUOSITAET_KENNUNG)selection.getFirstElement();
        if( kenn == null )
          return;

        m_sinuositaet.setKennung( kenn.name() );
      }
    } );
    m_kennung.getCombo().setLayoutData( new GridData( GridData.FILL, GridData.CENTER, true, false ) );
    m_toolkit.adapt( m_kennung.getCombo() );

    m_toolkit.createLabel( m_propPanel, m_sinuositaet.getPropertyLabel( SinuositaetProfileObject.PROPERTY_GERINNE_ART ) );
    m_gerinne = new ComboViewer( m_propPanel );
    m_gerinne.setContentProvider( new ArrayContentProvider() );
    m_gerinne.setInput( SINUOSITAET_GERINNE_ART.values() );
    m_gerinne.addSelectionChangedListener( new ISelectionChangedListener()
    {
      @Override
      public void selectionChanged( final SelectionChangedEvent event )
      {
        final IStructuredSelection selection = (IStructuredSelection)event.getSelection();
        final SINUOSITAET_GERINNE_ART gerA = (SINUOSITAET_GERINNE_ART)selection.getFirstElement();
        if( gerA == null )
          return;

        m_sinuositaet.setGerinneArt( gerA.name() );
      }
    } );
    m_gerinne.getCombo().setLayoutData( new GridData( GridData.FILL, GridData.CENTER, true, false ) );
    m_toolkit.adapt( m_gerinne.getCombo() );

    m_toolkit.createLabel( m_propPanel, m_sinuositaet.getPropertyLabel( SinuositaetProfileObject.PROPERTY_LF ) );
    m_lf = m_toolkit.createText( m_propPanel, "", SWT.TRAIL | SWT.SINGLE | SWT.BORDER ); //$NON-NLS-1$
    m_lf.setLayoutData( new GridData( GridData.FILL, GridData.CENTER, true, false ) );
    m_lf.addModifyListener( doubleModifyListener );
    m_lf.addFocusListener( new FocusListener()
    {
      @Override
      public void focusLost( final FocusEvent e )
      {
        if( e.widget instanceof Text && NumberUtils.isDouble( ((Text)e.widget).getText() ) )
        {
          final Double val = NumberUtils.parseDouble( ((Text)e.widget).getText() );
          m_sinuositaet.setLf( val );
        }
      }

      @Override
      public void focusGained( final FocusEvent e )
      {
        if( e.widget instanceof Text )
        {
          ((Text)e.widget).selectAll();
        }
      }
    } );

    m_toolkit.createLabel( m_propPanel, m_sinuositaet.getPropertyLabel( SinuositaetProfileObject.PROPERTY_SN ) );
    m_sn = m_toolkit.createText( m_propPanel, "", SWT.TRAIL | SWT.SINGLE | SWT.BORDER ); //$NON-NLS-1$
    m_sn.setLayoutData( new GridData( GridData.FILL, GridData.CENTER, true, false ) );
    m_sn.addModifyListener( doubleModifyListener );
    m_sn.addFocusListener( new FocusListener()
    {
      @Override
      public void focusLost( final FocusEvent e )
      {
        if( e.widget instanceof Text && NumberUtils.isDouble( ((Text)e.widget).getText() ) )
        {
          final Double val = NumberUtils.parseDouble( ((Text)e.widget).getText() );
          m_sinuositaet.setSn( val );
        }
      }

      @Override
      public void focusGained( final FocusEvent e )
      {
        if( e.widget instanceof Text )
        {
          ((Text)e.widget).selectAll();
        }
      }
    } );

    m_propPanel.layout();
  }

  protected void updateControls( )
  {
    m_gerinne.setSelection( new StructuredSelection( m_sinuositaet.getGerinneArt() ) );
    m_kennung.setSelection( new StructuredSelection( m_sinuositaet.getKennung() ) );
    m_sn.setText( String.format( "%.4f", m_sinuositaet.getSn() ) ); //$NON-NLS-1$
    m_lf.setText( String.format( "%.4f", m_sinuositaet.getLf() ) ); //$NON-NLS-1$
  }

  @Override
  public void onProfilChanged( final ProfileChangeHint hint )
  {
    if( hint.isObjectDataChanged() )
    {
      final Control control = getControl();
      if( control != null && !control.isDisposed() )
      {
        control.getDisplay().asyncExec( new Runnable()
        {
          @Override
          public void run( )
          {
            updateControls();
          }
        } );
      }
    }
  }
}