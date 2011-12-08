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
import org.eclipse.jface.action.ToolBarManager;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.kalypso.commons.databinding.IDataBinding;
import org.kalypso.contribs.eclipse.jface.action.ActionHyperlink;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ui.rrm.internal.UIRrmImages;
import org.kalypso.ui.rrm.internal.timeseries.binding.Station;
import org.kalypso.ui.rrm.internal.timeseries.view.featureBinding.FeatureBean;

/**
 * @author Gernot Belger
 */
public class StationUiHandler extends AbstractTimeseriesNodeUiHandler
{
  private final Station m_station;

  private final CommandableWorkspace m_workspace;

  public StationUiHandler( final CommandableWorkspace workspace, final Station station )
  {
    m_workspace = workspace;
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
    return UIRrmImages.id( UIRrmImages.DESCRIPTORS.STATION );
  }

  @Override
  protected Control createPropertiesControl( final Composite parent, final IDataBinding binding, final ToolBarManager sectionToolbar )
  {
    final FeatureBean<Station> bean = new FeatureBean<>( m_station );

    final StationComposite stationControl = new StationComposite( parent, bean, binding, false );

    sectionToolbar.add( new EditStationAction( m_workspace, m_station, stationControl ) );

    return stationControl;
  }

  @Override
  protected void createHyperlinks( final FormToolkit toolkit, final Composite actionPanel )
  {
    ActionHyperlink.createHyperlink( toolkit, actionPanel, SWT.PUSH, new ImportTimeseriesAction( m_station, null ) );
  }
}