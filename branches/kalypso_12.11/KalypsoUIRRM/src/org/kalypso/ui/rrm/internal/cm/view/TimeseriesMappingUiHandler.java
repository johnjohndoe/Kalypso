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
package org.kalypso.ui.rrm.internal.cm.view;

import org.eclipse.jface.action.ToolBarManager;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.kalypso.commons.databinding.IDataBinding;
import org.kalypso.model.hydrology.binding.timeseriesMappings.ITimeseriesMapping;
import org.kalypso.ui.rrm.internal.UIRrmImages;
import org.kalypso.ui.rrm.internal.UIRrmImages.DESCRIPTORS;
import org.kalypso.ui.rrm.internal.cm.view.action.DeleteMappingAction;
import org.kalypso.ui.rrm.internal.cm.view.action.EditMappingAction;
import org.kalypso.ui.rrm.internal.i18n.Messages;
import org.kalypso.ui.rrm.internal.utils.featureBinding.FeatureBean;
import org.kalypso.ui.rrm.internal.utils.featureTree.AbstractTreeNodeUiHandler;
import org.kalypso.ui.rrm.internal.utils.featureTree.ITreeNodeModel;

/**
 * @author Gernot Belger
 */
public class TimeseriesMappingUiHandler extends AbstractTreeNodeUiHandler
{
  private final ITreeNodeModel m_model;

  private final ITimeseriesMapping m_mapping;

  public TimeseriesMappingUiHandler( final ITreeNodeModel model, final ITimeseriesMapping mapping )
  {
    m_model = model;
    m_mapping = mapping;
  }

  @Override
  public String getTypeLabel( )
  {
    return Messages.getString("TimeseriesMappingUiHandler_0"); //$NON-NLS-1$
  }

  @Override
  public String getTreeLabel( )
  {
    return m_mapping.getDescription();
  }

  @Override
  public ImageDescriptor getTreeImage( )
  {
    return UIRrmImages.id( DESCRIPTORS.TIMESERIES_MAPPING );
  }

  @Override
  protected Control createPropertiesControl( final Composite parent, final IDataBinding binding, final ToolBarManager sectionToolbar )
  {
    sectionToolbar.add( new EditMappingAction( m_model, m_mapping ) );
    sectionToolbar.add( new DeleteMappingAction( m_mapping ) );

    final FeatureBean<ITimeseriesMapping> bean = new FeatureBean<>( m_mapping );

    return new TimeseriesMappingComposite( parent, binding, bean, false );
  }

  @Override
  protected void createHyperlinks( final FormToolkit toolkit, final Composite actionPanel )
  {
    // TODO Auto-generated method stub

  }
}