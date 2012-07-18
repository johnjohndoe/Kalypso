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

import java.util.LinkedHashSet;
import java.util.Set;

import org.apache.commons.lang3.ArrayUtils;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.resource.ImageDescriptor;
import org.kalypso.commons.java.lang.Objects;
import org.kalypso.model.hydrology.project.RrmCalculationResult;
import org.kalypso.model.hydrology.project.RrmSimulation;
import org.kalypso.ui.rrm.internal.KalypsoUIRRMPlugin;
import org.kalypso.ui.rrm.internal.UIRrmImages;
import org.kalypso.ui.rrm.internal.i18n.Messages;
import org.kalypso.ui.rrm.internal.results.view.ResultManagementView;
import org.kalypso.ui.rrm.internal.simulations.actions.DeleteRrmCalcualtionAction;
import org.kalypso.ui.rrm.internal.simulations.actions.RenameRrmCalcualtionAction;

/**
 * @author Dirk Kuch
 */
public class HydrologyCalculationCaseGroupUiHandler extends AbstractResultTreeNodeUiHandler
{
  public HydrologyCalculationCaseGroupUiHandler( final RrmSimulation simulation, final RrmCalculationResult calculation, final ResultManagementView view )
  {
    super( simulation, calculation, view );
  }

  public HydrologyCalculationCaseGroupUiHandler( final RrmSimulation simulation, final ResultManagementView view )
  {
    super( simulation, null, view );
  }

  @Override
  public String getTreeLabel( )
  {
    final RrmCalculationResult calculation = getCalculation();
    if( Objects.isNotNull( calculation ) )
    {
      if( calculation.isCurrent() )
        return Messages.getString("HydrologyCalculationCaseGroupUiHandler.0"); //$NON-NLS-1$

      return calculation.getName();
    }

    return getSimulation().getName();
  }

  @Override
  protected String getTreeCompareLabel( )
  {
    final String label = getTreeLabel();

    final RrmCalculationResult calculation = getCalculation();
    if( Objects.isNotNull( calculation ) )
    {
      // FIXME: would be nice to sort by time instead

      if( calculation.isCurrent() )
        return String.format( "ZZZ_%s", label ); //$NON-NLS-1$
    }

    return String.format( "AAA_%s", label ); //$NON-NLS-1$
  }

  @Override
  public ImageDescriptor getTreeImage( )
  {
    if( Objects.isNotNull( getCalculation() ) )
      return KalypsoUIRRMPlugin.getDefault().getImageProvider().getImageDescriptor( UIRrmImages.DESCRIPTORS.CALC_CASE_FOLDER );

    return KalypsoUIRRMPlugin.getDefault().getImageProvider().getImageDescriptor( UIRrmImages.DESCRIPTORS.SIMULATION );
  }

  @Override
  protected Action[] getAdditionalActions( )
  {
    final Set<Action> actions = new LinkedHashSet<>();

    if( getCalculation() != null )
      actions.add( new RenameRrmCalcualtionAction( getView(), getCalculation() ) );

    final RrmCalculationResult[] calculations = doFindCalculations( getView().getSelection() );
    if( ArrayUtils.isNotEmpty( calculations ) )
      actions.add( new DeleteRrmCalcualtionAction( getView(), calculations ) );

    return actions.toArray( new Action[] {} );
  }
}
