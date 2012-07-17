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

import org.eclipse.jface.action.ToolBarManager;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.kalypso.commons.databinding.IDataBinding;
import org.kalypso.contribs.eclipse.jface.action.ActionHyperlink;
import org.kalypso.model.hydrology.binding.timeseries.ITimeseries;
import org.kalypso.model.hydrology.timeseries.Timeserieses;
import org.kalypso.ui.rrm.internal.UIRrmImages;
import org.kalypso.ui.rrm.internal.i18n.Messages;
import org.kalypso.ui.rrm.internal.timeseries.view.actions.DeleteTimeseriesAction;
import org.kalypso.ui.rrm.internal.timeseries.view.actions.EditTimeseriesAction;
import org.kalypso.ui.rrm.internal.timeseries.view.actions.ExtendAndOverwriteTimeseriesAction;
import org.kalypso.ui.rrm.internal.timeseries.view.actions.ExtendTimeseriesAction;
import org.kalypso.ui.rrm.internal.timeseries.view.actions.ReplaceTimeseriesAction;
import org.kalypso.ui.rrm.internal.utils.featureBinding.FeatureBean;
import org.kalypso.ui.rrm.internal.utils.featureTree.AbstractTreeNodeUiHandler;
import org.kalypso.ui.rrm.internal.utils.featureTree.ITreeNodeModel;

/**
 * @author Gernot Belger
 */
public class TimeseriesUiHandler extends AbstractTreeNodeUiHandler
{
  private final ITimeseries m_timeseries;

  private final ITreeNodeModel m_model;

  private IDataBinding m_binding;

  public TimeseriesUiHandler( final ITreeNodeModel model, final ITimeseries timeseries )
  {
    m_model = model;
    m_timeseries = timeseries;
  }

  @Override
  public String getTypeLabel( )
  {
    return Messages.getString( "TimeseriesUiHandler_0" ); //$NON-NLS-1$
  }

  @Override
  public String getTreeLabel( )
  {
    return Timeserieses.getTreeLabel( m_timeseries );
  }

  @Override
  public ImageDescriptor getTreeImage( )
  {
    return UIRrmImages.id( UIRrmImages.DESCRIPTORS.TIMESERIES );
  }

  @Override
  protected Control createPropertiesControl( final Composite parent, final IDataBinding binding, final ToolBarManager sectionToolbar )
  {
    m_binding = binding;
    final FeatureBean<ITimeseries> timeseriesBean = new TimeseriesBean( m_timeseries );
    // final String stationLabel = m_timeseries.getOwner().getDescription();
    // final String deleteMessage = String.format( Messages.getString( "TimeseriesUiHandler_1" ), getTreeLabel(), stationLabel ); //$NON-NLS-1$
    //
    // sectionToolbar.add( new EditTimeseriesAction( timeseriesBean, binding ) );
    // sectionToolbar.add( new DeleteTimeseriesAction( m_model, deleteMessage, m_timeseries ) );

    return new TimeseriesComposite( parent, timeseriesBean, binding, false );
  }

  @Override
  protected void createHyperlinks( final FormToolkit toolkit, final Composite actionPanel )
  {
    final FeatureBean<ITimeseries> timeseriesBean = new TimeseriesBean( m_timeseries );
    final String stationLabel = m_timeseries.getOwner().getDescription();
    final String deleteMessage = String.format( Messages.getString( "TimeseriesUiHandler_1" ), getTreeLabel(), stationLabel ); //$NON-NLS-1$

    ActionHyperlink.createHyperlink( toolkit, actionPanel, SWT.PUSH, new EditTimeseriesAction( timeseriesBean, m_binding ) );
    ActionHyperlink.createHyperlink( toolkit, actionPanel, SWT.PUSH, new ExtendTimeseriesAction( m_model, m_timeseries ) );
    ActionHyperlink.createHyperlink( toolkit, actionPanel, SWT.PUSH, new ExtendAndOverwriteTimeseriesAction( m_model, m_timeseries ) );
    ActionHyperlink.createHyperlink( toolkit, actionPanel, SWT.PUSH, new ReplaceTimeseriesAction( m_model, m_timeseries ) );
    ActionHyperlink.createHyperlink( toolkit, actionPanel, SWT.PUSH, new DeleteTimeseriesAction( m_model, deleteMessage, m_timeseries ) );

    // TODO: utility that changes the timestep
    // TODO: copy timeseries
  }
}