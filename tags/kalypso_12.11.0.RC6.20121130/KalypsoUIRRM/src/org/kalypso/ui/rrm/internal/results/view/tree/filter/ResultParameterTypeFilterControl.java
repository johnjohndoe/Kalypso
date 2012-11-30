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
package org.kalypso.ui.rrm.internal.results.view.tree.filter;

import java.util.Collections;
import java.util.Comparator;
import java.util.Set;
import java.util.TreeSet;

import org.apache.commons.lang3.StringUtils;
import org.eclipse.core.databinding.DataBindingContext;
import org.eclipse.core.databinding.beans.PojoObservables;
import org.eclipse.core.databinding.observable.value.IObservableValue;
import org.eclipse.jface.databinding.viewers.IViewerObservableValue;
import org.eclipse.jface.databinding.viewers.ViewerProperties;
import org.eclipse.jface.layout.GridLayoutFactory;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.ComboViewer;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.StructuredViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.kalypso.contribs.eclipse.swt.widgets.ControlUtils;
import org.kalypso.ui.rrm.internal.results.view.base.IHydrologyResultReference;
import org.kalypso.ui.rrm.internal.results.view.base.KalypsoHydrologyResults.RRM_RESULT;

/**
 * @author Dirk Kuch
 */
public class ResultParameterTypeFilterControl extends Composite
{
  static final RGB YELLOW = new RGB( 255, 255, 230 );

  private final ResultParameterTypeFilter m_filter = new ResultParameterTypeFilter( StringUtils.EMPTY );

  private final DataBindingContext m_binding = new DataBindingContext();

  Set<Object> m_parameterTypes = new TreeSet<>( new Comparator<Object>()
  {
    @Override
    public int compare( final Object o1, final Object o2 )
    {
      return o1.toString().compareTo( o2.toString() );
    }
  } );

  private ComboViewer m_viewer;

  public ResultParameterTypeFilterControl( final Composite parent, final FormToolkit toolkit )
  {
    super( parent, SWT.NONE );

    ControlUtils.addDisposeListener( this );
    toolkit.adapt( this );

    m_parameterTypes.add( StringUtils.EMPTY );
    Collections.addAll( m_parameterTypes, RRM_RESULT.values() );

    GridLayoutFactory.swtDefaults().applyTo( this );
    createContents( this );
  }

  @Override
  public void dispose( )
  {
    super.dispose();
  }

  public void setViewer( final StructuredViewer viewer )
  {
    m_filter.setViewer( viewer );
    viewer.addFilter( m_filter );
  }

  private void createContents( final Composite parent )
  {
    createParameterTypeCombo( parent );
  }

  private void createParameterTypeCombo( final Composite parent )
  {
    m_viewer = new ComboViewer( parent, SWT.READ_ONLY | SWT.SINGLE );
    m_viewer.getCombo().setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    m_viewer.setLabelProvider( new LabelProvider() );
    m_viewer.setContentProvider( new ArrayContentProvider() );
    m_viewer.setInput( m_parameterTypes.toArray() );

    final IViewerObservableValue targetValue = ViewerProperties.singleSelection().observe( m_viewer );
    final IObservableValue modelValue = PojoObservables.observeValue( m_filter, ResultParameterTypeFilter.PROPERTY_TYPE );

    m_binding.bindValue( targetValue, modelValue );

    m_viewer.setSelection( new StructuredSelection( StringUtils.EMPTY ) );
  }

  public void reset( )
  {
    m_viewer.setSelection( new StructuredSelection( StringUtils.EMPTY ) );
  }

//  public void setParameterType( final String type )
//  {
//    m_viewer.setSelection( new StructuredSelection( type ) );
//    m_viewer.getCombo().setEnabled( false );
//  }

  public boolean doSelect( final IHydrologyResultReference reference )
  {
    return m_filter.doSelect( reference );
  }
}