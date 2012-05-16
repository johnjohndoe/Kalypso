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
package org.kalypso.ui.rrm.internal.results.view.tree;

import org.eclipse.core.resources.IFolder;
import org.eclipse.jface.action.ToolBarManager;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.kalypso.commons.databinding.IDataBinding;
import org.kalypso.commons.java.lang.Objects;
import org.kalypso.model.hydrology.project.RrmSimulation;
import org.kalypso.ui.rrm.internal.KalypsoUIRRMPlugin;
import org.kalypso.ui.rrm.internal.UIRrmImages;
import org.kalypso.ui.rrm.internal.utils.featureTree.AbstractTreeNodeUiHandler;

/**
 * @author Dirk Kuch
 */
public class HydrologyCalculationCaseGroupUiHandler extends AbstractTreeNodeUiHandler
{
  private final IFolder m_calculationCaseResultFolder;

  private final RrmSimulation m_simulation;

  public HydrologyCalculationCaseGroupUiHandler( final IFolder calculationCaseResultFolder )
  {
    m_calculationCaseResultFolder = calculationCaseResultFolder;
    m_simulation = null;
  }

  public HydrologyCalculationCaseGroupUiHandler( final RrmSimulation simulation )
  {
    m_simulation = simulation;
    m_calculationCaseResultFolder = null;
  }

  @Override
  public String getTypeLabel( )
  {
    if( Objects.isNotNull( m_calculationCaseResultFolder ) )
      return m_calculationCaseResultFolder.getName();

    return m_simulation.getName();
  }

  @Override
  public String getTreeLabel( )
  {
    if( Objects.isNotNull( m_calculationCaseResultFolder ) )
      return m_calculationCaseResultFolder.getName();

    return m_simulation.getName();
  }

  @Override
  public ImageDescriptor getTreeImage( )
  {
    if( Objects.isNotNull( m_calculationCaseResultFolder ) )
      return KalypsoUIRRMPlugin.getDefault().getImageProvider().getImageDescriptor( UIRrmImages.DESCRIPTORS.CALC_CASE_FOLDER );

    return KalypsoUIRRMPlugin.getDefault().getImageProvider().getImageDescriptor( UIRrmImages.DESCRIPTORS.SIMULATION );
  }

  @Override
  protected Control createPropertiesControl( final Composite parent, final IDataBinding binding, final ToolBarManager sectionToolbar )
  {
    // TODO Auto-generated method stub
    return null;
  }

  @Override
  protected void createHyperlinks( final FormToolkit toolkit, final Composite actionPanel )
  {
    // TODO Auto-generated method stub

  }

}
