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
package org.kalypso.ui.rrm.internal.timeseries.view;

import org.eclipse.core.databinding.beans.BeansObservables;
import org.eclipse.core.databinding.observable.value.IObservableValue;
import org.eclipse.jface.databinding.swt.ISWTObservableValue;
import org.eclipse.jface.databinding.swt.SWTObservables;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.kalypso.commons.databinding.DataBinder;
import org.kalypso.commons.databinding.IDataBinding;
import org.kalypso.ogc.sensor.timeseries.TimeseriesUtils;
import org.kalypso.ui.rrm.internal.timeseries.binding.Timeseries;
import org.kalypso.ui.rrm.internal.timeseries.view.featureBinding.FeatureBean;
import org.kalypso.ui.rrm.internal.timeseries.view.featureBinding.FeatureBeanComposite;

/**
 * @author Gernot Belger
 */
public class TimeseriesNewComposite extends FeatureBeanComposite<Timeseries>
{
  public TimeseriesNewComposite( final Composite parent, final FeatureBean<Timeseries> featureBean, final IDataBinding binding )
  {
    super( parent, featureBean, binding, true );
  }

  @Override
  protected void createContents( )
  {
    createParameterTypeControl();

    createPropertyControl( Timeseries.PROPERTY_QUALITY );

    createTimestepControl();
  }

  private void createParameterTypeControl( )
  {
    new Label( this, SWT.NONE ).setText( "Parameter Type" );

    final Text field = new Text( this, SWT.BORDER | SWT.SINGLE );
    field.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    field.setEnabled( false );

    final String parameterType = (String) getBean().getProperty( Timeseries.PROPERTY_PARAMETER_TYPE );
    final String parameterName = TimeseriesUtils.getName( parameterType );
    final String parameterUnit = TimeseriesUtils.getUnit( parameterType );
    final String parameterLabel = String.format( "%s (%s)", parameterName, parameterUnit );
    field.setText( parameterLabel );
  }

  private void createTimestepControl( )
  {
    new Label( this, SWT.NONE ).setText( "Timestep" );

    // FIXME: instead amount + field chooser; use binding?

    final Text field = new Text( this, SWT.BORDER | SWT.SINGLE );
    field.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );

    final ISWTObservableValue target = SWTObservables.observeText( field, SWT.Modify );
    final IObservableValue model = BeansObservables.observeValue( getBean(), TimeseriesBean.PROPERTY_PERIOD_TEXT );

    final DataBinder binder = new DataBinder( target, model );

    getBinding().bindValue( binder );
  }
}