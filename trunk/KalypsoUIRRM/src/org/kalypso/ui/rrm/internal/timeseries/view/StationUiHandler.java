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

import org.apache.commons.lang3.StringUtils;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.action.ToolBarManager;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.kalypso.commons.databinding.IDataBinding;
import org.kalypso.contribs.eclipse.jface.action.ActionHyperlink;
import org.kalypso.ui.rrm.internal.UIRrmImages;
import org.kalypso.ui.rrm.internal.UIRrmImages.DESCRIPTORS;
import org.kalypso.ui.rrm.internal.timeseries.binding.HydrologicalStation;
import org.kalypso.ui.rrm.internal.timeseries.binding.MeteorologicalStation;
import org.kalypso.ui.rrm.internal.timeseries.binding.Station;
import org.kalypso.ui.rrm.internal.timeseries.binding.Timeseries;
import org.kalypso.ui.rrm.internal.timeseries.view.featureBinding.FeatureBean;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;

/**
 * @author Gernot Belger
 */
public class StationUiHandler extends AbstractTimeseriesNodeUiHandler
{
  private final Station m_station;

  private final TimeseriesTreeContext m_context;

  public StationUiHandler( final TimeseriesTreeContext context, final Station station )
  {
    m_context = context;
    m_station = station;
  }

  @Override
  public String getTypeLabel( )
  {
    return "Station";
  }

  @Override
  public String getIdentifier( )
  {
    return m_station.getName();
  }

  @Override
  public String getTreeLabel( )
  {
    final String identifier = getIdentifier();
    if( StringUtils.isBlank( identifier ) )
      return m_station.getDescription();

    return String.format( "%s (%s)", m_station.getDescription(), identifier );
  }

  @Override
  public ImageDescriptor getTreeImage( )
  {
    if( m_station instanceof MeteorologicalStation )
      return UIRrmImages.id( DESCRIPTORS.STATION_METEOROLOGICAL );

    if( m_station instanceof HydrologicalStation )
      return UIRrmImages.id( DESCRIPTORS.STATION_HYDROLOGICAL );

    return UIRrmImages.id( DESCRIPTORS.STATION );
  }

  @Override
  protected Control createPropertiesControl( final Composite parent, final IDataBinding binding, final ToolBarManager sectionToolbar )
  {
    final FeatureBean<Station> bean = new FeatureBean<>( m_station );

    final StationComposite stationControl = new StationComposite( parent, bean, binding, false );

    sectionToolbar.add( new EditStationAction( m_context, m_station, stationControl ) );

    return stationControl;
  }

  @Override
  protected void createHyperlinks( final FormToolkit toolkit, final Composite actionPanel )
  {
    ActionHyperlink.createHyperlink( toolkit, actionPanel, SWT.PUSH, new ImportTimeseriesAction( m_station, null ) );

    /* Delete timeseries */
    final IFeatureBindingCollection<Timeseries> timeseries = m_station.getTimeseries();
    final Timeseries[] allTimeseries = timeseries.toArray( new Timeseries[timeseries.size()] );

    final String deleteMessage = String.format( "Delete all timeseries from of station '%s'?", getTreeLabel() );
    final TimeseriesNode pseudoParent = new TimeseriesNode( m_context, null, null, null );
    final IAction deleteAction = new DeleteTimeseriesAction( pseudoParent, deleteMessage, allTimeseries );
    ActionHyperlink.createHyperlink( toolkit, actionPanel, SWT.PUSH, deleteAction );

    /* Delete station */
    final IAction deleteStationAction = new DeleteStationAction( m_context, m_station );
    ActionHyperlink.createHyperlink( toolkit, actionPanel, SWT.PUSH, deleteStationAction );
  }
}