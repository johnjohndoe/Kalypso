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
package org.kalypso.ui.rrm.internal.timeseries.view.filter;

import org.apache.commons.lang3.StringUtils;
import org.eclipse.core.databinding.DataBindingContext;
import org.eclipse.core.databinding.beans.PojoObservables;
import org.eclipse.core.databinding.observable.value.IObservableValue;
import org.eclipse.jface.databinding.swt.ISWTObservableValue;
import org.eclipse.jface.databinding.swt.SWTObservables;
import org.eclipse.jface.layout.GridLayoutFactory;
import org.eclipse.jface.viewers.StructuredViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.kalypso.contribs.eclipse.swt.widgets.ControlUtils;
import org.kalypso.contribs.eclipse.swt.widgets.SelectAllFocusListener;
import org.kalypso.ui.rrm.internal.i18n.Messages;

/**
 * Helper that allow to search for specific string.
 *
 * @author Dirk Kuch
 */
public class TimeseriesBrowserTextSearchFilterControl extends Composite
{

  static final RGB YELLOW = new RGB( 255, 255, 230 );

  private final TimeseriesBrowserTextSearchFilter m_filter;

  private final DataBindingContext m_binding;

  private final Color m_yellow;

  private Text m_nameField;

  public TimeseriesBrowserTextSearchFilterControl( final Composite parent, final FormToolkit toolkit )
  {
    super( parent, SWT.NONE );

    ControlUtils.addDisposeListener( this );
    toolkit.adapt( this );

    m_yellow = new Color( parent.getDisplay(), YELLOW );
    m_filter = new TimeseriesBrowserTextSearchFilter( StringUtils.EMPTY );
    m_binding = new DataBindingContext();

    setLayout( GridLayoutFactory.fillDefaults().create() );

    createContents( this, toolkit );
  }

  @Override
  public void dispose( )
  {
    m_yellow.dispose();

    super.dispose();
  }

  public void setViewer( final StructuredViewer viewer )
  {
    m_filter.setViewer( viewer );
    viewer.addFilter( m_filter );
  }

  private void createContents( final Composite parent, final FormToolkit toolkit )
  {
    createSearchStringField( parent, toolkit );
  }

  private void createSearchStringField( final Composite parent, final FormToolkit toolkit )
  {
    m_nameField = new Text( parent, SWT.BORDER | SWT.SEARCH | SWT.ICON_CANCEL );
    m_nameField.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    m_nameField.setMessage( Messages.getString("TextSearchFilterControl_0") ); //$NON-NLS-1$
    m_nameField.addFocusListener( new SelectAllFocusListener() );

    addResetListener( m_nameField );

    final ISWTObservableValue target = SWTObservables.observeText( m_nameField, new int[] { SWT.Modify, SWT.DefaultSelection } );
    final IObservableValue model = PojoObservables.observeValue( m_filter, TimeseriesBrowserTextSearchFilter.PROPERTY_STRING );
    m_binding.bindValue( target, model );

    if( toolkit != null )
      toolkit.adapt( m_nameField, true, true );

    m_nameField.setBackground( m_yellow );
  }

  private void addResetListener( final Text field )
  {
    // Does actually not work on windows... :-(
    field.addSelectionListener( new SelectionAdapter()
    {
      @Override
      public void widgetDefaultSelected( final SelectionEvent e )
      {
        if( e.detail == SWT.ICON_CANCEL )
          field.setText( StringUtils.EMPTY );
      }
    } );
  }

  public void reset( )
  {
    m_nameField.setText( StringUtils.EMPTY );
  }

}