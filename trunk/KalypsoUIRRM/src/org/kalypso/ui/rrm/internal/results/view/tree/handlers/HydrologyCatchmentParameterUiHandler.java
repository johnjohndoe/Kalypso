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
import org.kalypso.model.hydrology.binding.model.Catchment;
import org.kalypso.ui.rrm.internal.results.view.base.KalypsoHydrologyResults.CATCHMENT_RESULT_TYPE;
import org.kalypso.ui.rrm.internal.utils.featureTree.AbstractTreeNodeUiHandler;

/**
 * @author Dirk Kuch
 */
public class HydrologyCatchmentParameterUiHandler extends AbstractTreeNodeUiHandler
{
  private final Catchment m_catchment;

  private final CATCHMENT_RESULT_TYPE m_type;

  public HydrologyCatchmentParameterUiHandler( final Catchment catchment, final CATCHMENT_RESULT_TYPE type )
  {
    m_catchment = catchment;
    m_type = type;
  }

  @Override
  public String getTypeLabel( )
  {
    return m_type.getLabel();
  }

  @Override
  public String getTreeLabel( )
  {
    return m_type.getLabel();
  }

  @Override
  public ImageDescriptor getTreeImage( )
  {
    return m_type.getImage();
  }

  /**
   * @see org.kalypso.ui.rrm.internal.utils.featureTree.AbstractTreeNodeUiHandler#createPropertiesControl(org.eclipse.swt.widgets.Composite,
   *      org.kalypso.commons.databinding.IDataBinding, org.eclipse.jface.action.ToolBarManager)
   */
  @Override
  protected Control createPropertiesControl( final Composite parent, final IDataBinding binding, final ToolBarManager sectionToolbar )
  {
    // TODO Auto-generated method stub
    return null;
  }

  /**
   * @see org.kalypso.ui.rrm.internal.utils.featureTree.AbstractTreeNodeUiHandler#createHyperlinks(org.eclipse.ui.forms.widgets.FormToolkit,
   *      org.eclipse.swt.widgets.Composite)
   */
  @Override
  protected void createHyperlinks( final FormToolkit toolkit, final Composite actionPanel )
  {
    // TODO Auto-generated method stub

  }

}
