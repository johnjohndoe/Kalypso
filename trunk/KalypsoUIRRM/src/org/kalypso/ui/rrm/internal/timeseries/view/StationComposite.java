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

import java.util.Set;
import java.util.TreeSet;

import org.apache.commons.lang3.StringUtils;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.jface.layout.GridLayoutFactory;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.kalypso.commons.databinding.IDataBinding;
import org.kalypso.commons.databinding.validation.FileNameIsUniqueValidator;
import org.kalypso.commons.databinding.validation.MultiValidator;
import org.kalypso.commons.databinding.validation.StringFilenameValidator;
import org.kalypso.commons.java.lang.Objects;
import org.kalypso.contribs.eclipse.jface.action.ActionHyperlink;
import org.kalypso.model.hydrology.binding.timeseries.IHydrologicalStation;
import org.kalypso.model.hydrology.binding.timeseries.IMeteorologicalStation;
import org.kalypso.model.hydrology.binding.timeseries.IStation;
import org.kalypso.model.hydrology.binding.timeseries.IStationCollection;
import org.kalypso.ui.rrm.internal.i18n.Messages;
import org.kalypso.ui.rrm.internal.timeseries.view.actions.EditStationLocationAction;
import org.kalypso.ui.rrm.internal.utils.featureBinding.FeatureBean;
import org.kalypso.ui.rrm.internal.utils.featureBinding.FeatureBeanComposite;
import org.kalypso.ui.rrm.internal.utils.featureBinding.FeatureBeanWizardPages;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;

/**
 * @author Gernot Belger
 */
public class StationComposite extends FeatureBeanComposite<IStation>
{
  public StationComposite( final Composite parent, final FeatureBean<IStation> station, final IDataBinding binding, final boolean editable )
  {
    super( parent, station, binding, editable );
  }

  @Override
  protected void createContents( )
  {
    final FeatureBean<IStation> bean = getBean();

    final IStation station = bean.getFeature();

    final StationTimeseriesFolderCollector collector = new StationTimeseriesFolderCollector( station );
    collector.execute( new NullProgressMonitor() );

    final String stationName = station != null ? station.getDescription() : StringUtils.EMPTY;

    final StringFilenameValidator filenameValidator = new StringFilenameValidator( IStatus.ERROR, Messages.getString( "StationComposite_0" ) ); //$NON-NLS-1$
    final FileNameIsUniqueValidator uniqueValudator = new FileNameIsUniqueValidator( collector.getResult(), stationName, IStatus.ERROR, Messages.getString( "StationComposite_1" ) ); //$NON-NLS-1$

    final MultiValidator validator = new MultiValidator( filenameValidator, uniqueValudator );

    createPropertyTextFieldControl( IStation.QN_DESCRIPTION, validator ); // -> folder name
    createPropertyTextFieldControl( IStation.QN_NAME );
    createPropertyComboTextControl( IStation.PROPERTY_GROUP, findGroups( station ) );
    createPropertyTextFieldControl( IStation.PROPERTY_COMMENT );

    createLocationControl();

    if( station instanceof IHydrologicalStation )
    {
      createPropertyTextFieldControl( IHydrologicalStation.PROPERTY_GAUGE_ZERO );
    }
    else if( station instanceof IMeteorologicalStation )
    {
      createPropertyTextFieldControl( IMeteorologicalStation.PROPERTY_ALTITUDE );
    }

    createMeasurementControl();
    createTimeseriesControl();
  }

  private String[] findGroups( final IStation base )
  {
    if( Objects.isNull( base ) )
      return new String[] {};

    final Feature parent = base.getOwner();
    if( !(parent instanceof IStationCollection) )
      return new String[] {};

    final Set<String> groups = new TreeSet<>();

    final IStationCollection collection = (IStationCollection) parent;
    final IFeatureBindingCollection<IStation> stations = collection.getStations();
    for( final IStation station : stations )
    {
      final String group = station.getGroup();
      if( StringUtils.isNotEmpty( group ) )
        groups.add( group );
    }

    return groups.toArray( new String[] {} );
  }

  private void createLocationControl( )
  {
    final FormToolkit toolkit = getToolkit();
    final FeatureBean<IStation> bean = getBean();

    createPropertyLabel( this, IStation.PROPERTY_LOCATION );

    final Composite panel = FeatureBeanWizardPages.createComposite( this, toolkit );
    GridLayoutFactory.fillDefaults().numColumns( 2 ).applyTo( panel );
    panel.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );

    final Text field = createPropertyTextField( panel );
    field.setEnabled( false );

    bindTextField( field, IStation.PROPERTY_LOCATION );

    if( isEditable() )
      ActionHyperlink.createHyperlink( toolkit, panel, SWT.PUSH, new EditStationLocationAction( bean ) );
    else
      FeatureBeanWizardPages.createLabel( panel, toolkit, StringUtils.EMPTY );
  }

  private void createMeasurementControl( )
  {
    // TODO Auto-generated method stub
  }

  private void createTimeseriesControl( )
  {
    // TODO Auto-generated method stub
  }
}
