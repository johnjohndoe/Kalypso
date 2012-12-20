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
import org.kalypso.model.hydrology.binding.timeseries.IHydrologicalStation;
import org.kalypso.model.hydrology.binding.timeseries.IMeteorologicalStation;
import org.kalypso.model.hydrology.binding.timeseries.IStation;
import org.kalypso.model.hydrology.binding.timeseries.ITimeseries;
import org.kalypso.ui.rrm.internal.UIRrmImages;
import org.kalypso.ui.rrm.internal.UIRrmImages.DESCRIPTORS;
import org.kalypso.ui.rrm.internal.i18n.Messages;
import org.kalypso.ui.rrm.internal.timeseries.view.actions.CalculateEvaporationAction;
import org.kalypso.ui.rrm.internal.timeseries.view.actions.DeleteStationAction;
import org.kalypso.ui.rrm.internal.timeseries.view.actions.DeleteTimeseriesAction;
import org.kalypso.ui.rrm.internal.timeseries.view.actions.EditStationAction;
import org.kalypso.ui.rrm.internal.timeseries.view.actions.ImportTimeseriesAction;
import org.kalypso.ui.rrm.internal.timeseries.view.actions.NewHydrologicalStationAction;
import org.kalypso.ui.rrm.internal.timeseries.view.actions.NewMeteorologicalStationAction;
import org.kalypso.ui.rrm.internal.utils.featureBinding.FeatureBean;
import org.kalypso.ui.rrm.internal.utils.featureTree.AbstractTreeNodeUiHandler;
import org.kalypso.ui.rrm.internal.utils.featureTree.ITreeNodeModel;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;

/**
 * @author Gernot Belger
 */
public class StationUiHandler extends AbstractTreeNodeUiHandler
{
  private final IStation m_station;

  private final ITreeNodeModel m_model;

  public StationUiHandler( final ITreeNodeModel model, final IStation station )
  {
    m_model = model;
    m_station = station;
  }

  @Override
  public String getTypeLabel( )
  {
    return Messages.getString( "StationUiHandler_0" ); //$NON-NLS-1$
  }

  @Override
  public String getTreeLabel( )
  {
    final String identifier = m_station.getName();
    if( StringUtils.isBlank( identifier ) )
      return m_station.getDescription();

    return String.format( "%s (%s)", m_station.getDescription(), identifier ); //$NON-NLS-1$
  }

  @Override
  public ImageDescriptor getTreeImage( )
  {
    if( m_station instanceof IMeteorologicalStation )
      return UIRrmImages.id( DESCRIPTORS.STATION_METEOROLOGICAL );

    if( m_station instanceof IHydrologicalStation )
      return UIRrmImages.id( DESCRIPTORS.STATION_HYDROLOGICAL );

    return UIRrmImages.id( DESCRIPTORS.STATION );
  }

  @Override
  protected Control createPropertiesControl( final Composite parent, final IDataBinding binding, final ToolBarManager sectionToolbar )
  {
    final FeatureBean<IStation> bean = new FeatureBean<>( m_station );

    final StationComposite stationControl = new StationComposite( parent, bean, binding, false );

    sectionToolbar.add( new EditStationAction( m_model, m_station, stationControl ) );
    sectionToolbar.add( new DeleteStationAction( m_station ) );

    return stationControl;
  }

  @Override
  protected void createHyperlinks( final FormToolkit toolkit, final Composite actionPanel )
  {
    final String group = m_station.getGroup();

    ActionHyperlink.createHyperlink( toolkit, actionPanel, SWT.PUSH, new NewMeteorologicalStationAction( m_model, group ) );
    ActionHyperlink.createHyperlink( toolkit, actionPanel, SWT.PUSH, new NewHydrologicalStationAction( m_model, group ) );

    ActionHyperlink.createHyperlink( toolkit, actionPanel, SWT.PUSH, new ImportTimeseriesAction( m_model, m_station, null ) );

    /* Delete timeseries */
    final IFeatureBindingCollection<ITimeseries> timeseries = m_station.getTimeseries();
    final ITimeseries[] allTimeseries = timeseries.toArray( new ITimeseries[timeseries.size()] );

    final String deleteMessage = String.format( Messages.getString( "StationUiHandler_2" ), getTreeLabel() ); //$NON-NLS-1$
    final IAction deleteAction = new DeleteTimeseriesAction( m_model, deleteMessage, allTimeseries );
    ActionHyperlink.createHyperlink( toolkit, actionPanel, SWT.PUSH, deleteAction );

    if( m_station instanceof IMeteorologicalStation )
      ActionHyperlink.createHyperlink( toolkit, actionPanel, SWT.PUSH, new CalculateEvaporationAction( m_model, m_station ) );
  }

}