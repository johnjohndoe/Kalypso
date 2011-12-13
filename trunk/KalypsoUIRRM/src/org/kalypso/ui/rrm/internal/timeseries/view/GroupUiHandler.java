/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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
import org.kalypso.ui.rrm.internal.UIRrmImages;
import org.kalypso.ui.rrm.internal.UIRrmImages.DESCRIPTORS;
import org.kalypso.ui.rrm.internal.utils.featureTree.AbstractTreeNodeUiHandler;
import org.kalypso.ui.rrm.internal.utils.featureTree.ITreeNodeModel;

/**
 * @author Gernot Belger
 */
public class GroupUiHandler extends AbstractTreeNodeUiHandler
{
  static final String UNDEFINED_GROUP_LABEL = "<undefined group>";
  private final String m_group;

  private final ITreeNodeModel m_model;

  public GroupUiHandler( final ITreeNodeModel model, final String group )
  {
    m_model = model;
    m_group = group;
  }

  @Override
  public String getTypeLabel( )
  {
    return "Group";
  }

  @Override
  public String getTreeLabel( )
  {
    if( StringUtils.isBlank( m_group ) )
      return UNDEFINED_GROUP_LABEL;

    return m_group;
  }

  @Override
  public ImageDescriptor getTreeImage( )
  {
    return UIRrmImages.id( DESCRIPTORS.GROUP );
  }

  @Override
  protected Control createPropertiesControl( final Composite parent, final IDataBinding binding, final ToolBarManager sectionToolbar )
  {
    final Control composite = new GroupComposite( parent, binding, m_group );

    // TODO: edit action for group in toolbar

    return composite;
  }

  @Override
  protected void createHyperlinks( final FormToolkit toolkit, final Composite actionPanel )
  {
    ActionHyperlink.createHyperlink( toolkit, actionPanel, SWT.PUSH, new NewMeteorologicalStationAction( m_model, m_group ) );
    ActionHyperlink.createHyperlink( toolkit, actionPanel, SWT.PUSH, new NewHydrologicalStationAction( m_model, m_group ) );

    // TODO: import stations
    // TODO: delete stations
  }
}