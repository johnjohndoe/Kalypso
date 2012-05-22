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
package org.kalypso.ui.rrm.internal.results.view.tree.handlers;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.ToolBarManager;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.forms.widgets.ImageHyperlink;
import org.kalypso.commons.databinding.IDataBinding;
import org.kalypso.contribs.eclipse.jface.action.ActionHyperlink;
import org.kalypso.model.hydrology.project.RrmSimulation;
import org.kalypso.ui.rrm.internal.simulations.actions.OpenOutputZipAction;
import org.kalypso.ui.rrm.internal.simulations.actions.OpenTextLogAction;
import org.kalypso.ui.rrm.internal.utils.featureTree.AbstractTreeNodeUiHandler;

/**
 * @author Dirk Kuch
 */
public abstract class AbstractResultTreeNodeUiHandler extends AbstractTreeNodeUiHandler
{

  private final RrmSimulation m_simulation;

  public AbstractResultTreeNodeUiHandler( final RrmSimulation simulation )
  {
    m_simulation = simulation;
  }

  protected RrmSimulation getSimulation( )
  {
    return m_simulation;
  }

  @Override
  public final String getTypeLabel( )
  {
    return String.format( "Simulation: %s", getSimulation().getName() );
  }

  @Override
  protected Control createPropertiesControl( final Composite parent, final IDataBinding binding, final ToolBarManager sectionToolbar )
  {
    return null;
  }

  @Override
  protected void createHyperlinks( final FormToolkit toolkit, final Composite actionPanel )
  {
    final List<Action> actions = new ArrayList<Action>();
    actions.add( new OpenTextLogAction( "Open calculation log", "Displays the calculation log.", m_simulation.getCalculationLog() ) );
    actions.add( new OpenOutputZipAction( "Open error log (calculation core)", "Displays the error log.", m_simulation, true ) );
    actions.add( new OpenOutputZipAction( "Open output log (calculation core)", "Displays the output log.", m_simulation, false ) );
    actions.add( new OpenTextLogAction( "Open Mass Balance", "Displays the mass balance.", m_simulation.getBilanzTxt() ) );
    actions.add( new OpenTextLogAction( "Open Statistics", "Displays the statistics.", m_simulation.getStatisticsCsv() ) );

    /* Create the image hyperlinks. */
    for( final Action action : actions )
    {
      final ImageHyperlink imageHyperlink = ActionHyperlink.createHyperlink( null, actionPanel, SWT.NONE, action );
      imageHyperlink.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
      imageHyperlink.setText( action.getText() );
    }

  }

}
