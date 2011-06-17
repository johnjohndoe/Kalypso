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

import org.apache.commons.lang.StringUtils;
import org.eclipse.core.databinding.DataBindingContext;
import org.eclipse.core.databinding.beans.BeansObservables;
import org.eclipse.core.databinding.observable.value.IObservableValue;
import org.eclipse.jface.databinding.swt.ISWTObservableValue;
import org.eclipse.jface.databinding.swt.SWTObservables;
import org.eclipse.jface.databinding.viewers.IViewerObservableValue;
import org.eclipse.jface.databinding.viewers.ViewersObservables;
import org.eclipse.jface.layout.GridLayoutFactory;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.CheckStateChangedEvent;
import org.eclipse.jface.viewers.CheckboxTreeViewer;
import org.eclipse.jface.viewers.ComboViewer;
import org.eclipse.jface.viewers.ICheckStateListener;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.kalypso.contribs.eclipse.jface.viewers.tree.TreeViewerUtilities;
import org.kalypso.contribs.eclipse.jface.viewers.tree.TreeVisiterAbortException;
import org.kalypso.ui.rrm.i18n.Messages;

/**
 * @author Gernot Belger
 */
public class EditRelationViewer extends Composite
{
  private final EditRelationOptionsContentProvider m_contentProvider = new EditRelationOptionsContentProvider();

  private final EditRelationOptionsLabelProvider m_labelProvider = new EditRelationOptionsLabelProvider();

  private final DataBindingContext m_binding;

  private final EditRelationData m_data;

  public EditRelationViewer( final Composite parent, final FormToolkit toolkit, final EditRelationData data )
  {
    super( parent, SWT.NONE );

    m_data = data;

    toolkit.adapt( this );
    GridLayoutFactory.swtDefaults().numColumns( 2 ).applyTo( this );

    m_binding = new DataBindingContext();

    createModeCombo( this, toolkit );
    createInfoControls( this, toolkit );
    createTree( this, toolkit );
  }

  private void createTree( final Composite parent, final FormToolkit toolkit )
  {
    final CheckboxTreeViewer viewer = new CheckboxTreeViewer( parent );
    final Control treeControl = viewer.getControl();
    toolkit.adapt( treeControl, true, true );
    treeControl.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true, 2, 1 ) );
    viewer.setContentProvider( m_contentProvider );
    viewer.setLabelProvider( m_labelProvider );
    viewer.setCheckStateProvider( new EditRelationCheckStateProvider( m_data ) );

    viewer.setAutoExpandLevel( 2 );

    final EditRelationData data = m_data;
    final EditRelationOptionsContentProvider contentProvider = m_contentProvider;
    viewer.addCheckStateListener( new ICheckStateListener()
    {
      @Override
      public void checkStateChanged( final CheckStateChangedEvent event )
      {
        final Object element = event.getElement();

        final boolean checked = event.getChecked();
        try
        {
          TreeViewerUtilities.accept( contentProvider, element, new SetCheckedTreeVisitor( checked, data ) );
        }
        catch( final TreeVisiterAbortException ex )
        {
          ex.printStackTrace();
        }

        viewer.refresh( element, true );
      }
    } );

    final IObservableValue target = ViewersObservables.observeInput( viewer );
    final IObservableValue model = BeansObservables.observeValue( m_data, EditRelationData.PROPERTY_INPUT );

    m_binding.bindValue( target, model );
  }

  private void createModeCombo( final Composite parent, final FormToolkit toolkit )
  {
    toolkit.createLabel( parent, "Edit Mode" );

    final ComboViewer viewer = new ComboViewer( parent, SWT.READ_ONLY | SWT.DROP_DOWN );
    final Control control = viewer.getControl();
    control.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    toolkit.adapt( control, true, true );

    viewer.setLabelProvider( new LabelProvider() );
    viewer.setContentProvider( new ArrayContentProvider() );
    viewer.setInput( EditRelationData.MODE.values() );

    final IViewerObservableValue target = ViewersObservables.observeSinglePostSelection( viewer );
    final IObservableValue model = BeansObservables.observeValue( m_data, EditRelationData.PROPERTY_MODIFICATION_MODE );
    m_binding.bindValue( target, model );
  }

  private void createInfoControls( final Composite parent, final FormToolkit toolkit )
  {
    toolkit.createLabel( parent, Messages.getString( "org.kalypso.ogc.gml.map.widgets.editrelation.EditRelationWidget.11" ) ); //$NON-NLS-1$
    createInfo( parent, toolkit, EditRelationData.PROPERTY_INFO_FROM );

    toolkit.createLabel( parent, Messages.getString( "org.kalypso.ogc.gml.map.widgets.editrelation.EditRelationWidget.17" ) ); //$NON-NLS-1$
    createInfo( parent, toolkit, EditRelationData.PROPERTY_INFO_FROM );

    toolkit.createLabel( parent, Messages.getString( "org.kalypso.ogc.gml.map.widgets.editrelation.EditRelationWidget.25" ) ); //$NON-NLS-1$
    createInfo( parent, toolkit, EditRelationData.PROPERTY_PROBLEM_MESSAGE );
  }

  private void createInfo( final Composite parent, final FormToolkit toolkit, final String property )
  {
    final Text field = toolkit.createText( parent, StringUtils.EMPTY, SWT.READ_ONLY | SWT.MULTI | SWT.BORDER | SWT.WRAP ); //$NON-NLS-1$
    field.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );

    final ISWTObservableValue target = SWTObservables.observeText( field );
    final IObservableValue model = BeansObservables.observeValue( m_data, property );
    m_binding.bindValue( target, model );
  }
}