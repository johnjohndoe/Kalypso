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

import java.util.Locale;

import org.apache.commons.lang3.StringUtils;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.action.ToolBarManager;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.joda.time.Period;
import org.joda.time.format.PeriodFormat;
import org.kalypso.commons.databinding.IDataBinding;
import org.kalypso.contribs.eclipse.jface.action.ActionHyperlink;
import org.kalypso.ui.rrm.internal.UIRrmImages;
import org.kalypso.ui.rrm.internal.timeseries.binding.Timeseries;
import org.kalypso.ui.rrm.internal.timeseries.view.featureBinding.FeatureBean;

/**
 * @author Gernot Belger
 */
public class TimeseriesUiHandler extends AbstractTimeseriesNodeUiHandler
{
  private final Timeseries m_timeseries;

  private final TimeseriesTreeContext m_context;

  public TimeseriesUiHandler( final TimeseriesTreeContext context, final Timeseries timeseries )
  {
    m_context = context;
    m_timeseries = timeseries;
  }

  @Override
  public String getTypeLabel( )
  {
    return "Timeseries";
  }

  @Override
  public String getIdentifier( )
  {
    return StringUtils.EMPTY;
  }

  @Override
  public String getTreeLabel( )
  {
    final Period timestep = m_timeseries.getTimestep();

    final String quality = m_timeseries.getQuality();
    final String periodName = String.format( "%s", PeriodFormat.wordBased( Locale.getDefault() ).print( timestep ) );

    if( StringUtils.isBlank( quality ) )
      return periodName;

    return String.format( "%s (%s)", periodName, quality );
  }

  @Override
  public ImageDescriptor getTreeImage( )
  {
    return UIRrmImages.id( UIRrmImages.DESCRIPTORS.TIMESERIES );
  }

  @Override
  protected Control createPropertiesControl( final Composite parent, final IDataBinding binding, final ToolBarManager sectionToolbar )
  {
    final FeatureBean<Timeseries> timeseriesBean = new TimeseriesBean( m_timeseries );

    final Composite timeseriesControl = new TimeseriesComposite( parent, timeseriesBean, binding, false );

    return timeseriesControl;
  }

  @Override
  protected void createHyperlinks( final FormToolkit toolkit, final Composite actionPanel )
  {
    // TODO: utility that changes the timestep
    // TODO: copy timeseries

    /* Delete timeseries */
    final String stationLabel = m_timeseries.getParent().getDescription();
    final String deleteMessage = String.format( "Delete timeseries '%s' from station '%s'?", getTreeLabel(), stationLabel );
    final IAction deleteAction = new DeleteTimeseriesAction( m_context, deleteMessage, m_timeseries );
    ActionHyperlink.createHyperlink( toolkit, actionPanel, SWT.PUSH, deleteAction );
  }
}