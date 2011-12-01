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
package org.kalypso.ui.rrm.internal.map.editRelation;

import org.apache.commons.lang3.StringUtils;
import org.eclipse.core.databinding.DataBindingContext;
import org.eclipse.core.databinding.beans.BeansObservables;
import org.eclipse.core.databinding.observable.value.IObservableValue;
import org.eclipse.core.databinding.observable.value.IValueChangeListener;
import org.eclipse.core.databinding.observable.value.ValueChangeEvent;
import org.eclipse.jface.databinding.viewers.IViewerObservableValue;
import org.eclipse.jface.databinding.viewers.ViewersObservables;
import org.eclipse.jface.layout.GridLayoutFactory;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.ComboViewer;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.forms.widgets.ScrolledForm;
import org.kalypso.contribs.eclipse.swt.widgets.ControlUtils;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.ui.rrm.internal.i18n.Messages;

/**
 * @author Gernot Belger
 */
public class EditRelationViewer extends Composite
{
  private final DataBindingContext m_binding;

  private final EditRelationData m_data;

  private ScrolledForm m_radioControl;

  private final FormToolkit m_toolkit;

  public EditRelationViewer( final Composite parent, final FormToolkit toolkit, final EditRelationData data )
  {
    super( parent, SWT.NONE );

    m_toolkit = toolkit;
    m_data = data;

    toolkit.adapt( this );
    GridLayoutFactory.swtDefaults().numColumns( 2 ).applyTo( this );

    m_binding = new DataBindingContext();

    createModeCombo( this, toolkit );
    createTree( this, toolkit );

    final IObservableValue inputValue = BeansObservables.observeValue( data, EditRelationData.PROPERTY_INPUT );
    inputValue.addValueChangeListener( new IValueChangeListener()
    {
      @Override
      public void handleValueChange( final ValueChangeEvent event )
      {
        updateControl();
      }
    } );

    updateControl();
  }

  private void createTree( final Composite parent, final FormToolkit toolkit )
  {
    m_radioControl = toolkit.createScrolledForm( parent );
    m_radioControl.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true, 2, 1 ) );
    m_radioControl.getBody().setLayout( new GridLayout() );
  }

  private void createModeCombo( final Composite parent, final FormToolkit toolkit )
  {
    toolkit.createLabel( parent, Messages.getString( "EditRelationViewer_0" ) ); //$NON-NLS-1$

    final ComboViewer viewer = new ComboViewer( parent, SWT.READ_ONLY | SWT.DROP_DOWN );
    final Control control = viewer.getControl();
    control.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    toolkit.adapt( control, true, true );

    viewer.setLabelProvider( new LabelProvider() );
    viewer.setContentProvider( new ArrayContentProvider() );
    viewer.setInput( EditRelationMode.values() );

    final IViewerObservableValue target = ViewersObservables.observeSinglePostSelection( viewer );
    final IObservableValue model = BeansObservables.observeValue( m_data, EditRelationData.PROPERTY_MODIFICATION_MODE );
    m_binding.bindValue( target, model );
  }

  protected void updateControl( )
  {
    final EditRelationData data = m_data;
    final EditRelationInput input = data.getInput();

    final Composite body = m_radioControl.getBody();
    ControlUtils.disposeChildren( body );

    if( input == null )
      return;

    final IEditRelationType[] elements = input.getElements();
    IFeatureType lastSourceType = null;
    for( final IEditRelationType relation : elements )
    {
      /* Separator between different source types */
      final IFeatureType sourceType = relation.getSourceType();
      if( lastSourceType != null && sourceType != lastSourceType )
      {
        final Label separator = m_toolkit.createLabel( body, StringUtils.EMPTY, SWT.SEPARATOR | SWT.HORIZONTAL );
        separator.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
      }

      final Button button = m_toolkit.createButton( body, relation.toString(), SWT.RADIO );
      if( relation == elements[0] )
      {
        button.setSelection( true );
        data.setRelation( relation );
      }

      button.addSelectionListener( new SelectionAdapter()
      {
        @Override
        public void widgetSelected( final SelectionEvent e )
        {
          handleRelationSelection( relation );
        }

        @Override
        public void widgetDefaultSelected( final SelectionEvent e )
        {
          handleRelationSelection( relation );
        }
      } );

      lastSourceType = sourceType;
    }

    m_radioControl.reflow( true );
  }

  protected void handleRelationSelection( final IEditRelationType relation )
  {
    m_data.setRelation( relation );
    m_data.recalculateAllowedFeatures();
  }
}