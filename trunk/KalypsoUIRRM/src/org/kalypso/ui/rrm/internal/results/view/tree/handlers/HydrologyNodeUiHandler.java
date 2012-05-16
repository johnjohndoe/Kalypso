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
package org.kalypso.ui.rrm.internal.results.view.tree.handlers;

import org.eclipse.jface.action.ToolBarManager;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.kalypso.commons.databinding.IDataBinding;
import org.kalypso.model.hydrology.binding.model.nodes.Node;
import org.kalypso.ui.rrm.internal.UIRrmImages;
import org.kalypso.ui.rrm.internal.UIRrmImages.DESCRIPTORS;
import org.kalypso.ui.rrm.internal.utils.featureTree.AbstractTreeNodeUiHandler;

/**
 * @author Dirk Kuch
 */
public class HydrologyNodeUiHandler extends AbstractTreeNodeUiHandler
{
  private final Node m_hydrologyNode;

  public HydrologyNodeUiHandler( final Node hydrologyNode )
  {
    m_hydrologyNode = hydrologyNode;
  }

  @Override
  public String getTypeLabel( )
  {
    return m_hydrologyNode.getName();
  }

  @Override
  public String getTreeLabel( )
  {
    return m_hydrologyNode.getName();
  }

  @Override
  public ImageDescriptor getTreeImage( )
  {
    return UIRrmImages.id( DESCRIPTORS.NA_NODE );
  }

  @Override
  protected Control createPropertiesControl( final Composite parent, final IDataBinding binding, final ToolBarManager sectionToolbar )
  {
    return null;
  }

  @Override
  protected void createHyperlinks( final FormToolkit toolkit, final Composite actionPanel )
  {
  }

}
