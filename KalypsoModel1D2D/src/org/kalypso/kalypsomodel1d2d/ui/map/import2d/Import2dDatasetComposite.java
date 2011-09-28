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
package org.kalypso.kalypsomodel1d2d.ui.map.import2d;

import org.apache.commons.lang3.StringUtils;
import org.eclipse.core.databinding.DataBindingContext;
import org.eclipse.core.databinding.beans.BeansObservables;
import org.eclipse.core.databinding.observable.value.IObservableValue;
import org.eclipse.jface.databinding.swt.ISWTObservableValue;
import org.eclipse.jface.databinding.swt.SWTObservables;
import org.eclipse.jface.layout.GridLayoutFactory;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.kalypso.commons.databinding.DataBinder;
import org.kalypso.core.status.StatusComposite;
import org.kalypso.core.status.StatusCompositeValue;

/**
 * @author Gernot Belger
 */
public class Import2dDatasetComposite extends Composite
{
  private final Import2dDataset m_dataSet;

  private final DataBindingContext m_binding;

  public Import2dDatasetComposite( final Composite parent, final FormToolkit toolkit, final Import2dDataset statistics )
  {
    super( parent, SWT.NONE );

    m_dataSet = statistics;

    toolkit.adapt( this );

    GridLayoutFactory.swtDefaults().numColumns( 2 ).applyTo( this );

    m_binding = new DataBindingContext();

    createReadStatusControls( this, toolkit );
    createElementCountControls( this, toolkit );
  }

  private void createReadStatusControls( final Composite parent, final FormToolkit toolkit )
  {
    final StatusComposite statusComposite = new StatusComposite( toolkit, parent, SWT.NONE );
    statusComposite.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false, 2, 1 ) );

    final StatusCompositeValue target = new StatusCompositeValue( statusComposite );
    final IObservableValue model = BeansObservables.observeValue( m_dataSet, Import2dDataset.PROPERTY_LAST_READ_STATUS );

    final DataBinder binder = new DataBinder( target, model );
    binder.apply( m_binding );
  }

  private void createElementCountControls( final Composite parent, final FormToolkit toolkit )
  {
    toolkit.createLabel( parent, "Available Elements" );

    final Text field = toolkit.createText( parent, StringUtils.EMPTY, SWT.READ_ONLY );
    field.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );

    final ISWTObservableValue target = SWTObservables.observeText( field );
    final IObservableValue model = BeansObservables.observeValue( m_dataSet, Import2dDataset.PROPERTY_ELEMENT_COUNT );

    final DataBinder binder = new DataBinder( target, model );
    binder.apply( m_binding );
  }
}