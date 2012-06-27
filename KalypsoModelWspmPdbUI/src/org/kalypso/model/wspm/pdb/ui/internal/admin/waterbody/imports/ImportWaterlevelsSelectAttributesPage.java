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
package org.kalypso.model.wspm.pdb.ui.internal.admin.waterbody.imports;

import java.io.IOException;
import java.math.BigDecimal;
import java.util.Calendar;
import java.util.Date;

import org.eclipse.core.databinding.beans.BeanProperties;
import org.eclipse.core.databinding.beans.BeansObservables;
import org.eclipse.core.databinding.observable.value.IObservableValue;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.databinding.swt.ISWTObservableValue;
import org.eclipse.jface.databinding.swt.SWTObservables;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.DateTime;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.kalypso.commons.databinding.DataBinder;
import org.kalypso.commons.databinding.IDataBinding;
import org.kalypso.commons.databinding.property.value.DateTimeSelectionProperty;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.model.wspm.pdb.db.mapping.WaterlevelFixation;
import org.kalypso.model.wspm.pdb.ui.internal.WspmPdbUiPlugin;
import org.kalypso.model.wspm.pdb.ui.internal.i18n.Messages;
import org.kalypso.shape.ShapeFile;
import org.kalypso.shape.ShapeType;
import org.kalypso.shape.dbf.DBaseException;

/**
 * @author Gernot Belger
 */
public class ImportWaterlevelsSelectAttributesPage extends AbstractSelectAttributesPage
{
  private final ImportWaterLevelsData m_data;

  protected ImportWaterlevelsSelectAttributesPage( final String pageName, final ImportWaterLevelsData data )
  {
    super( pageName );

    setDescription( Messages.getString( "ImportWaterlevelsSelectAttributesPage.0" ) ); //$NON-NLS-1$

    m_data = data;
  }

  @Override
  protected ShapeFile openShape( ) throws IOException, DBaseException
  {
    return m_data.openShape();
  }

  @Override
  protected void createAttributeControls( final Composite parent, final IDataBinding binding )
  {
    createStationControl( parent );
    createWaterlevelControl( parent );
    createDischargeControl( parent, binding );
    createMeasurementControl( parent, binding );
    createDescriptionControl( parent, binding );
  }

  @Override
  protected void setAttributeInfos( final ImportAttributeInfo< ? >[] infos )
  {
    m_data.setAttributeInfos( infos );
  }

  private void createStationControl( final Composite parent )
  {
    createAttributeControl( WaterlevelFixationStrings.STATION, WaterlevelFixation.PROPERTY_STATION, parent, false );

    /* No default control: code must be taken from shape */
    new Label( parent, SWT.NONE );
  }

  private void createWaterlevelControl( final Composite parent )
  {
    createAttributeControl( WaterlevelFixationStrings.WATERLEVEL, WaterlevelFixation.PROPERTY_WATERLEVEL, parent, false );

    /* No default control: code must be taken from shape */
    new Label( parent, SWT.NONE );
  }

  private void createDischargeControl( final Composite parent, final IDataBinding binding )
  {
    final ImportAttributeInfo<BigDecimal> info = createAttributeControl( WaterlevelFixationStrings.DISCHARGE, WaterlevelFixation.PROPERTY_DISCHARGE, parent, true );

    final Text text = new Text( parent, SWT.BORDER );
    text.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    text.setMessage( Messages.getString( "ImportWaterlevelsSelectAttributesPage.1" ) ); //$NON-NLS-1$

    final ISWTObservableValue targetValue = SWTObservables.observeText( text, SWT.Modify );
    final IObservableValue modelValue = BeanProperties.value( ImportAttributeInfo.PROPERTY_DEFAULT_VALUE, BigDecimal.class ).observe( info );
    final DataBinder binder = new DataBinder( targetValue, modelValue );
    binding.bindValue( binder );

    final ISWTObservableValue targetEnablement = SWTObservables.observeEnabled( text );
    final IObservableValue modelEnablement = BeansObservables.observeValue( info, ImportAttributeInfo.PROPERTY_ENABLEMENT );
    binding.bindValue( targetEnablement, modelEnablement );
  }

  private void createMeasurementControl( final Composite parent, final IDataBinding binding )
  {
    final ImportAttributeInfo<Date> info = createAttributeControl( WaterlevelFixationStrings.MEASUREMENT, WaterlevelFixation.PROPERTY_MEASURMENT_DATE, parent, true );

    final DateTime measurementField = new DateTime( parent, SWT.DATE | SWT.MEDIUM | SWT.DROP_DOWN );
    measurementField.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );

    measurementField.setToolTipText( Messages.getString( "ImportWaterlevelsSelectAttributesPage.2" ) ); //$NON-NLS-1$

    final Calendar cal = Calendar.getInstance( KalypsoCorePlugin.getDefault().getTimeZone() );
    final Date measurementDate = m_data.getEvent().getMeasurementDate();
    if( measurementDate != null )
      cal.setTime( measurementDate );
    final DateTimeSelectionProperty selectionProperty = new DateTimeSelectionProperty( cal );

    // Unable ot leave it null, else NPE later
    info.setDefaultValue( cal.getTime() );

    final IObservableValue modelValue = BeanProperties.value( ImportAttributeInfo.PROPERTY_DEFAULT_VALUE, Date.class ).observe( info );

    final IObservableValue targetValue = selectionProperty.observe( measurementField );
    binding.bindValue( targetValue, modelValue );

    final ISWTObservableValue targetEnablement = SWTObservables.observeEnabled( measurementField );
    final IObservableValue modelEnablement = BeansObservables.observeValue( info, ImportAttributeInfo.PROPERTY_ENABLEMENT );
    binding.bindValue( targetEnablement, modelEnablement );
  }

  private void createDescriptionControl( final Composite parent, final IDataBinding binding )
  {
    final ImportAttributeInfo<String> info = createAttributeControl( WaterlevelFixationStrings.DESCRIPTION, WaterlevelFixation.PROPERTY_DESCRIPTION, parent, true );

    final Text text = new Text( parent, SWT.BORDER );
    text.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    text.setMessage( Messages.getString( "ImportWaterlevelsSelectAttributesPage.3" ) ); //$NON-NLS-1$

    final ISWTObservableValue targetValue = SWTObservables.observeText( text, SWT.Modify );
    final ISWTObservableValue targetEnablement = SWTObservables.observeEnabled( text );

    final IObservableValue modelValue = BeansObservables.observeValue( info, ImportAttributeInfo.PROPERTY_DEFAULT_VALUE );
    final IObservableValue modelEnablement = BeansObservables.observeValue( info, ImportAttributeInfo.PROPERTY_ENABLEMENT );

    binding.bindValue( targetValue, modelValue );
    binding.bindValue( targetEnablement, modelEnablement );
  }

  @Override
  protected IStatus checkGeometry( final ShapeFile shapeFile )
  {
    final ShapeType shapeType = shapeFile.getShapeType();
    final String label = shapeType.getLabel();
    switch( shapeType )
    {
      case POINT:
      case POINTZ:
        return new Status( IStatus.OK, WspmPdbUiPlugin.PLUGIN_ID, String.format( "%s", label ) ); //$NON-NLS-1$

      default:
        return new Status( IStatus.ERROR, WspmPdbUiPlugin.PLUGIN_ID, String.format( Messages.getString( "ImportWaterlevelsSelectAttributesPage.5" ), label ) ); //$NON-NLS-1$
    }
  }
}