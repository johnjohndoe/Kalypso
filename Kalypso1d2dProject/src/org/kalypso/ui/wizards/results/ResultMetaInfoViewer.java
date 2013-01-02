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
package org.kalypso.ui.wizards.results;

import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.ui.forms.widgets.FormText;
import org.eclipse.ui.forms.widgets.ScrolledForm;
import org.kalypso.commons.databinding.IDataBinding;
import org.kalypso.contribs.eclipse.swt.widgets.ControlUtils;
import org.kalypso.kalypso1d2d.internal.i18n.Messages;
import org.kalypso.kalypsosimulationmodel.core.resultmeta.IResultMeta;

/**
 * @author Thomas Jung
 */
public class ResultMetaInfoViewer
{
  private final ResultInfoBuilder m_infoBuilder = new ResultInfoBuilder();

  private Object m_input;

  private FormText m_infoArea;

  private final IResultControlFactory m_factory;

  private ScrolledForm m_form;

  private Composite m_factoryPanel;

  private final IDataBinding m_binding;

  public ResultMetaInfoViewer( final IDataBinding binding, final IResultControlFactory factory )
  {
    m_binding = binding;
    m_factory = factory;
  }

  public Control createControl( final Composite parent )
  {
    final Group control = new Group( parent, SWT.NONE );

    control.setLayout( new FillLayout() );
    control.setText( Messages.getString( "org.kalypso.ui.wizards.results.ResultMetaInfoViewer.0" ) ); //$NON-NLS-1$

    m_form = new ScrolledForm( control );
    m_form.setExpandHorizontal( true );
    m_form.setExpandVertical( true );

    final Composite body = m_form.getBody();
    body.setLayout( new GridLayout() );

    m_infoArea = new FormText( body, SWT.WRAP | SWT.READ_ONLY );
    m_infoArea.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );

    m_factoryPanel = new Composite( body, SWT.NONE );
    m_factoryPanel.setLayout( new FillLayout() );
    m_factoryPanel.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, false ) );

    return control;
  }

  public void setInput( final Object input )
  {
    m_input = input;

    refresh();
  }

  private void refresh( )
  {
    if( m_form == null )
      return;

    /* Empty old stuff */
    ControlUtils.disposeChildren( m_factoryPanel );

    final String infoText = m_infoBuilder.format( m_input );
    m_infoArea.setText( infoText, true, false );

    if( m_input instanceof IResultMeta && m_factory != null )
    {
      final IResultMeta result = (IResultMeta)m_input;
      final IResultControl createThemeCreator = m_factory.createThemeConstructor( result );
      createThemeCreator.createControl( m_binding, m_factoryPanel );
      m_factoryPanel.layout( true );
    }

    m_form.reflow( true );
  }
}