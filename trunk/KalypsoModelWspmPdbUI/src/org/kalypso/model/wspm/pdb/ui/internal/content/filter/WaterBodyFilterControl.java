/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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
package org.kalypso.model.wspm.pdb.ui.internal.content.filter;

import org.apache.commons.lang.StringUtils;
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
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.kalypso.contribs.eclipse.swt.widgets.SelectAllFocusListener;
import org.kalypso.model.wspm.pdb.ui.internal.admin.waterbody.WaterBodyStrings;

/**
 * Helper that allow to search for specific water bodies.
 * 
 * @author Gernot Belger
 */
public class WaterBodyFilterControl extends Composite
{
  private final WaterBodiesFilter m_filter;

  private final DataBindingContext m_binding;

  public WaterBodyFilterControl( final FormToolkit toolkit, final Composite parent )
  {
    super( parent, SWT.NONE );

    if( toolkit != null )
      toolkit.adapt( this );

    m_filter = new WaterBodiesFilter( StringUtils.EMPTY, StringUtils.EMPTY );

    GridLayoutFactory.swtDefaults().numColumns( 6 ).applyTo( this );
    m_binding = new DataBindingContext();

    createContents( toolkit, this );
  }

  public void setViewer( final StructuredViewer viewer )
  {
    m_filter.setViewer( viewer );
    viewer.addFilter( m_filter );
  }

  private void createContents( final FormToolkit toolkit, final Composite parent )
  {
    createGknField( toolkit, parent );
    createNameField( toolkit, parent );
  }

  private void createGknField( final FormToolkit toolkit, final Composite parent )
  {
    final Label label = new Label( parent, SWT.NONE );
    label.setText( WaterBodyStrings.STR_GEWÄSSERKENNZIFFER );

    final Text gknField = new Text( parent, SWT.BORDER | SWT.SEARCH | SWT.ICON_CANCEL );
    gknField.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false, 2, 1 ) );
    gknField.addFocusListener( new SelectAllFocusListener() );

    addResetListener( gknField );

    final ISWTObservableValue target = SWTObservables.observeText( gknField, new int[] { SWT.Modify, SWT.DefaultSelection } );
    final IObservableValue model = PojoObservables.observeValue( m_filter, WaterBodiesFilter.PROPERTY_GKN );
    m_binding.bindValue( target, model );

    if( toolkit != null )
    {
      toolkit.adapt( label, false, false );
      toolkit.adapt( gknField, true, true );
    }
  }

  private void createNameField( final FormToolkit toolkit, final Composite parent )
  {
    final Label label = new Label( parent, SWT.NONE );
    label.setText( WaterBodyStrings.STR_NAME );

    final Text nameField = new Text( parent, SWT.BORDER | SWT.SEARCH | SWT.ICON_CANCEL );
    nameField.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false, 2, 1 ) );
    nameField.addFocusListener( new SelectAllFocusListener() );

    addResetListener( nameField );

    final ISWTObservableValue target = SWTObservables.observeText( nameField, new int[] { SWT.Modify, SWT.DefaultSelection } );
    final IObservableValue model = PojoObservables.observeValue( m_filter, WaterBodiesFilter.PROPERTY_NAME );
    m_binding.bindValue( target, model );

    if( toolkit != null )
    {
      toolkit.adapt( label, false, false );
      toolkit.adapt( nameField, true, true );
    }
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

}