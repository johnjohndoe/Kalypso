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
package org.kalypso.ui.rrm.internal.timeseries.view.evaporation;

import org.eclipse.core.databinding.beans.BeansObservables;
import org.eclipse.core.databinding.observable.value.IObservableValue;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.databinding.swt.ISWTObservableValue;
import org.eclipse.jface.databinding.swt.SWTObservables;
import org.eclipse.jface.layout.GridLayoutFactory;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.kalypso.commons.databinding.jface.wizard.DatabindingWizardPage;
import org.kalypso.commons.databinding.validation.StringFilenameValidator;
import org.kalypso.model.hydrology.binding.timeseries.IParameterTypeProvider;
import org.kalypso.model.hydrology.binding.timeseries.IStation;
import org.kalypso.model.hydrology.operation.evaporation.IEvaporationCalculator;
import org.kalypso.ui.rrm.internal.i18n.Messages;
import org.kalypso.ui.rrm.internal.timeseries.QualityUniqueValidator;

/**
 * @author Dirk Kuch
 */
public class EvaporationParameterPage extends WizardPage
{
  private final CalculateEvaporationData m_data;

  private DatabindingWizardPage m_binding;

  protected EvaporationParameterPage( final CalculateEvaporationData data )
  {
    super( Messages.getString( "EvaporationParameterPage_0" ) ); //$NON-NLS-1$

    m_data = data;

    setTitle( Messages.getString( "EvaporationParameterPage_1" ) ); //$NON-NLS-1$
    setDescription( Messages.getString( "EvaporationParameterPage_2" ) ); //$NON-NLS-1$
  }

  @Override
  public void createControl( final Composite parent )
  {
    initializeDialogUnits( parent );

    m_binding = new DatabindingWizardPage( this, null );

    final Composite panel = new Composite( parent, SWT.NULL );
    GridLayoutFactory.swtDefaults().applyTo( panel );
    setControl( panel );

    final Group group = new Group( panel, SWT.NULL );
    GridLayoutFactory.swtDefaults().numColumns( 2 ).applyTo( group );
    group.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, false ) );
    group.setText( Messages.getString( "EvaporationParameterPage_6" ) ); //$NON-NLS-1$

    createTextControl( group, Messages.getString( "EvaporationParameterPage_7" ), CalculateEvaporationData.PROPERTY_LATITUDE ); //$NON-NLS-1$

    createTimeseriesGroup( panel );
  }

  private void createTextControl( final Composite parent, final String label, final String property )
  {
    final Label lab = new Label( parent, SWT.NULL );
    lab.setText( label );

    final Text text = new Text( parent, SWT.BORDER );
    text.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, false ) );

    /* binding */
    final ISWTObservableValue target = SWTObservables.observeText( text, SWT.Modify );
    final IObservableValue model = BeansObservables.observeValue( m_data, property );

    m_binding.bindValue( target, model );
  }

  private void createTimeseriesGroup( final Composite page )
  {
    final Group group = new Group( page, SWT.NULL );
    GridLayoutFactory.swtDefaults().numColumns( 2 ).applyTo( group );
    group.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, false ) );
    group.setText( Messages.getString("EvaporationParameterPage.1") ); //$NON-NLS-1$

    final Label lab = new Label( group, SWT.NULL );
    lab.setText( Messages.getString( "EvaporationParameterPage_10" ) ); //$NON-NLS-1$

    final Text text = new Text( group, SWT.BORDER );
    text.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, false ) );

    final ISWTObservableValue target = SWTObservables.observeText( text, new int[] { SWT.Modify } );
    final IObservableValue model = BeansObservables.observeValue( m_data, CalculateEvaporationData.PROPERTY_QUALITY );

    final StringFilenameValidator filename = new StringFilenameValidator( IStatus.ERROR, Messages.getString( "EvaporationParameterPage_11" ) ); //$NON-NLS-1$

    // REMARK: chosen calculator and hence produced parameter type varies during wizard live-cycle
    final CalculateEvaporationData data = m_data;
    final IParameterTypeProvider parameterTypeProvider = new IParameterTypeProvider()
    {
      @Override
      public String getParameterType( )
      {
        final IEvaporationCalculator calculator = data.getCalculator();
        if( calculator == null )
          return null;

        return calculator.getParameterType();
      }
    };

    final IStation station = m_data.getStation();
    final QualityUniqueValidator exists = new QualityUniqueValidator( station, null, parameterTypeProvider );

    m_binding.bindValue( target, model, filename, exists );
  }
}