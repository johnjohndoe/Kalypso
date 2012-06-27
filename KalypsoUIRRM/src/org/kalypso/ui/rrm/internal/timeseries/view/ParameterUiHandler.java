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

import org.eclipse.jface.action.IAction;
import org.eclipse.jface.action.ToolBarManager;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.kalypso.commons.databinding.IDataBinding;
import org.kalypso.contribs.eclipse.jface.action.ActionHyperlink;
import org.kalypso.model.hydrology.binding.timeseries.IStation;
import org.kalypso.model.hydrology.binding.timeseries.ITimeseries;
import org.kalypso.ui.rrm.internal.UIRrmImages;
import org.kalypso.ui.rrm.internal.i18n.Messages;
import org.kalypso.ui.rrm.internal.timeseries.view.actions.DeleteTimeseriesAction;
import org.kalypso.ui.rrm.internal.timeseries.view.actions.ImportTimeseriesAction;
import org.kalypso.ui.rrm.internal.utils.ParameterTypeUtils;
import org.kalypso.ui.rrm.internal.utils.featureTree.AbstractTreeNodeUiHandler;
import org.kalypso.ui.rrm.internal.utils.featureTree.ITreeNodeModel;

/**
 * @author Gernot Belger
 */
public class ParameterUiHandler extends AbstractTreeNodeUiHandler
{
  private final String m_parameterType;

  private final ITimeseries[] m_timeseries;

  private final IStation m_station;

  private final ITreeNodeModel m_model;

  public ParameterUiHandler( final ITreeNodeModel model, final IStation station, final String parameterType, final ITimeseries[] timeseries )
  {
    m_model = model;
    m_station = station;
    m_parameterType = parameterType;
    m_timeseries = timeseries;
  }

  @Override
  public String getTypeLabel( )
  {
    return Messages.getString("ParameterUiHandler_0"); //$NON-NLS-1$
  }

  @Override
  public String getTreeLabel( )
  {
    return ParameterTypeUtils.formatParameterType( m_parameterType );
  }

  @Override
  public ImageDescriptor getTreeImage( )
  {
    final String imageLocation = UIRrmImages.DESCRIPTORS.PARAMETER_TYPE_BASE.getImagePath() + "_" + m_parameterType + ".png"; //$NON-NLS-1$ //$NON-NLS-2$
    return UIRrmImages.id( imageLocation );
  }

  @Override
  protected Control createPropertiesControl( final Composite parent, final IDataBinding binding, final ToolBarManager sectionToolbar )
  {
    return new ParameterComposite( parent, binding, m_parameterType );
  }

  @Override
  protected void createHyperlinks( final FormToolkit toolkit, final Composite actionPanel )
  {
    ActionHyperlink.createHyperlink( toolkit, actionPanel, SWT.PUSH, new ImportTimeseriesAction( m_model, m_station, m_parameterType ) );

    /* Delete timeseries */
    final String stationLabel = m_station.getDescription();
    final String deleteMessage = String.format( Messages.getString("ParameterUiHandler_1"), getTreeLabel(), stationLabel ); //$NON-NLS-1$
    final IAction deleteAction = new DeleteTimeseriesAction( m_model, deleteMessage, m_timeseries );
    ActionHyperlink.createHyperlink( toolkit, actionPanel, SWT.PUSH, deleteAction );
  }
}