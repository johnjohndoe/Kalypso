/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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
package org.kalypso.ui.rrm.internal.timeseries.view;

import org.apache.commons.lang3.StringUtils;
import org.eclipse.core.databinding.beans.BeansObservables;
import org.eclipse.core.databinding.observable.value.IObservableValue;
import org.eclipse.jface.databinding.swt.ISWTObservableValue;
import org.eclipse.jface.databinding.swt.SWTObservables;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.kalypso.commons.databinding.DataBinder;
import org.kalypso.commons.databinding.IDataBinding;
import org.kalypso.core.status.StatusComposite;
import org.kalypso.core.status.StatusCompositeValue;
import org.kalypso.model.hydrology.timeseries.binding.ITimeseries;
import org.kalypso.ui.rrm.internal.utils.ParameterTypeUtils;
import org.kalypso.ui.rrm.internal.utils.featureBinding.FeatureBean;
import org.kalypso.ui.rrm.internal.utils.featureBinding.FeatureBeanComposite;

/**
 * @author Gernot Belger
 */
public class TimeseriesComposite extends FeatureBeanComposite<ITimeseries>
{
  public TimeseriesComposite( final Composite parent, final FeatureBean<ITimeseries> featureBean, final IDataBinding binding, final boolean editable )
  {
    super( parent, featureBean, binding, editable );
  }

  @Override
  protected void createContents( )
  {
    // createPropertyControl( Timeseries.QN_DESCRIPTION );
    // createPropertyControl( Timeseries.QN_NAME );
    createParameterTypeControl();
    createPropertyControl( ITimeseries.PROPERTY_QUALITY );

    createTimestepControl();

    createTimeseriesDataValidationControl();

    // TODO: Check consistency (i.e. existence) of data file

    // TODO: show timestep
    // TODO: utility that changes the timestep
    // TODO: copy timeseries
  }

  private void createParameterTypeControl( )
  {
    createPropertyLabel( this, ITimeseries.PROPERTY_PARAMETER_TYPE );

    final Text field = createPropertyTextField( this );

    final String parameterType = (String) getBean().getProperty( ITimeseries.PROPERTY_PARAMETER_TYPE );
    final String parameterLabel = ParameterTypeUtils.formatParameterType( parameterType );

    field.setText( parameterLabel );
  }

  private void createTimestepControl( )
  {
    final FormToolkit toolkit = getToolkit();

    toolkit.createLabel( this, "Timestep" );

    final Text field = toolkit.createText( this, StringUtils.EMPTY, SWT.BORDER | SWT.SINGLE );
    field.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    field.setEnabled( false );

    final ISWTObservableValue target = SWTObservables.observeText( field, SWT.Modify );
    final IObservableValue model = BeansObservables.observeValue( getBean(), TimeseriesBean.PROPERTY_PERIOD_TEXT );

    final DataBinder binder = new DataBinder( target, model );

    getBinding().bindValue( binder );
  }

  private void createTimeseriesDataValidationControl( )
  {
    final FormToolkit toolkit = getToolkit();

    toolkit.createLabel( this, "Data File" );

    final StatusComposite statusComposite = new StatusComposite( toolkit, this, SWT.NONE );
    statusComposite.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );

    final IObservableValue target = new StatusCompositeValue( statusComposite );
    final IObservableValue model = BeansObservables.observeValue( getBean(), TimeseriesBean.PROPERTY_DATA_STATUS );

    final DataBinder binder = new DataBinder( target, model );

    getBinding().bindValue( binder );
  }
}