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
package org.kalypso.model.wspm.pdb.ui.internal.content;

import java.util.Arrays;

import org.eclipse.core.databinding.DataBindingContext;
import org.eclipse.core.databinding.beans.BeansObservables;
import org.eclipse.core.databinding.observable.value.IObservableValue;
import org.eclipse.jface.databinding.swt.ISWTObservableValue;
import org.eclipse.jface.databinding.swt.SWTObservables;
import org.eclipse.jface.layout.GridLayoutFactory;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.forms.widgets.ImageHyperlink;
import org.eclipse.ui.forms.widgets.ScrolledForm;
import org.eclipse.ui.forms.widgets.Section;
import org.kalypso.contribs.eclipse.jface.action.ActionHyperlink;
import org.kalypso.contribs.eclipse.swt.widgets.ControlUtils;
import org.kalypso.model.wspm.pdb.connect.IPdbSettings;
import org.kalypso.model.wspm.pdb.connect.PdbConnectException;
import org.kalypso.model.wspm.pdb.connect.PdbSettings;
import org.kalypso.model.wspm.pdb.ui.internal.WspmPdbUiImages;
import org.kalypso.model.wspm.pdb.ui.internal.WspmPdbUiImages.IMAGE;
import org.kalypso.model.wspm.pdb.ui.internal.i18n.Messages;
import org.kalypso.model.wspm.pdb.ui.internal.preferences.OpenConnectionData;

/**
 * @author Gernot Belger
 */
public class NonConnectedControl extends Composite
{
  private final DataBindingContext m_binding;

  private Composite m_connectionPanel;

  private final OpenConnectionData m_data;

  private final PdbView m_view;

  private final FormToolkit m_toolkit;

  private ScrolledForm m_form;

  NonConnectedControl( final FormToolkit toolkit, final Composite parent, final OpenConnectionData data, final PdbView view )
  {
    super( parent, SWT.NONE );

    m_toolkit = toolkit;
    m_data = data;
    m_view = view;

    toolkit.adapt( this );
    GridLayoutFactory.fillDefaults().applyTo( this );

    m_binding = new DataBindingContext();

    createSettingsChooser( toolkit, this );

    final Composite lowerPanel = toolkit.createComposite( this );
    GridLayoutFactory.swtDefaults().applyTo( lowerPanel );
    lowerPanel.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    createAutoConnectCheck( toolkit, lowerPanel );
    createSettingsConfigHyperlink( lowerPanel );

    updateControl();
  }

  private void createSettingsChooser( final FormToolkit toolkit, final Composite parent )
  {
    final Section section = toolkit.createSection( parent, Section.TITLE_BAR | Section.DESCRIPTION );
    section.setLayout( new FillLayout() );
    section.setText( Messages.getString( "NonConnectedControl.0" ) ); //$NON-NLS-1$
    section.setDescription( Messages.getString( "NonConnectedControl.1" ) ); //$NON-NLS-1$

    toolkit.adapt( section );
    section.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );

    m_form = m_toolkit.createScrolledForm( section );

    section.setClient( m_form );

    final Composite body = m_form.getBody();
    GridLayoutFactory.swtDefaults().applyTo( body );

    m_connectionPanel = body;
  }

  private void createAutoConnectCheck( final FormToolkit toolkit, final Composite parent )
  {
    final Button button = toolkit.createButton( parent, Messages.getString( "NonConnectedControl.2" ), SWT.CHECK ); //$NON-NLS-1$
    button.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );

    final ISWTObservableValue target = SWTObservables.observeSelection( button );
    final IObservableValue model = BeansObservables.observeValue( m_data, OpenConnectionData.PROPERTY_AUTOCONNECT );

    m_binding.bindValue( target, model );
  }

  private void createSettingsConfigHyperlink( final Composite parent )
  {
    final ConfigureConnectionsAction action = new ConfigureConnectionsAction()
    {
      @Override
      protected void updateViewer( )
      {
        updateControl();
      }
    };
    action.setImageDescriptor( WspmPdbUiImages.getImageDescriptor( IMAGE.CONNECT_TO_PDB ) );

    final ImageHyperlink hyperlink = ActionHyperlink.createHyperlink( m_toolkit, parent, SWT.NONE, action );
    hyperlink.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
  }

  protected void updateControl( )
  {
    ControlUtils.disposeChildren( m_connectionPanel );

    try
    {
      final IPdbSettings[] settings = PdbSettings.getSettings();
      Arrays.sort( settings );

      for( final IPdbSettings setting : settings )
      {
        final ConnectPdbAction action = new ConnectPdbAction( m_view, setting );
        final ImageHyperlink hyperlink = ActionHyperlink.createHyperlink( m_toolkit, m_connectionPanel, SWT.PUSH | SWT.WRAP | SWT.CENTER, action );
        hyperlink.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
      }
    }
    catch( final PdbConnectException e )
    {
      e.printStackTrace();
      // TODO: show status composite?
    }

    m_form.reflow( true );
  }
}