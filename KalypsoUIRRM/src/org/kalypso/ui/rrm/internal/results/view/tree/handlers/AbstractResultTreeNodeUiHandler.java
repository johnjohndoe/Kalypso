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
import java.util.Collections;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.ToolBarManager;
import org.eclipse.jface.layout.GridLayoutFactory;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.forms.widgets.ImageHyperlink;
import org.kalypso.commons.databinding.IDataBinding;
import org.kalypso.contribs.eclipse.jface.action.ActionHyperlink;
import org.kalypso.contribs.eclipse.jface.wizard.IUpdateable;
import org.kalypso.model.hydrology.project.RrmCalculationResult;
import org.kalypso.model.hydrology.project.RrmSimulation;
import org.kalypso.ui.rrm.internal.i18n.MessageConstants;
import org.kalypso.ui.rrm.internal.i18n.Messages;
import org.kalypso.ui.rrm.internal.results.view.ResultManagementView;
import org.kalypso.ui.rrm.internal.results.view.base.CalculationFeatureBean;
import org.kalypso.ui.rrm.internal.results.view.base.HydrologyResultReference;
import org.kalypso.ui.rrm.internal.results.view.base.RrmResultBean;
import org.kalypso.ui.rrm.internal.simulations.actions.DeleteRrmCalcualtionsAction;
import org.kalypso.ui.rrm.internal.simulations.actions.OpenOutputZipAction;
import org.kalypso.ui.rrm.internal.simulations.actions.OpenStatusLogAction;
import org.kalypso.ui.rrm.internal.simulations.actions.OpenTextLogAction;
import org.kalypso.ui.rrm.internal.utils.featureTree.AbstractTreeNodeUiHandler;
import org.kalypso.ui.rrm.internal.utils.featureTree.TreeNode;

/**
 * @author Dirk Kuch
 */
public abstract class AbstractResultTreeNodeUiHandler extends AbstractTreeNodeUiHandler
{
  private final RrmSimulation m_simulation;

  private final RrmCalculationResult m_calculation;

  private final ResultManagementView m_view;

  public AbstractResultTreeNodeUiHandler( final RrmSimulation simulation, final RrmCalculationResult calculation, final ResultManagementView view )
  {
    m_simulation = simulation;
    m_calculation = calculation;
    m_view = view;
  }

  protected RrmSimulation getSimulation( )
  {
    return m_simulation;
  }

  protected RrmCalculationResult getCalculation( )
  {
    return m_calculation;
  }

  protected ResultManagementView getView( )
  {
    return m_view;
  }

  @Override
  public final String getTypeLabel( )
  {
    final RrmSimulation simulation = getSimulation();
    if( simulation == null )
      return getTreeLabel();

    return String.format( Messages.getString( "AbstractResultTreeNodeUiHandler_0" ), simulation.getName() ); //$NON-NLS-1$
  }

  @Override
  protected Control createPropertiesControl( final Composite parent, final IDataBinding binding, final ToolBarManager sectionToolbar )
  {
    return null;
  }

  @Override
  protected final void createHyperlinks( final FormToolkit toolkit, final Composite actionPanel )
  {
    if( getSimulation() == null )
      return;

    final List<Action> actions = new ArrayList<>();

    // TODL: ugly: implement specific ui handler for this case, does not belong in abstract class

    if( getCalculation() != null )
    {
      // TODO: would be nice to have different icons for different type of results

      final OpenStatusLogAction openStatusLogAction = new OpenStatusLogAction( MessageConstants.STR_ACTION_OPEN_CALC_STATUS_TEXT, MessageConstants.STR_ACTION_OPEN_CALC_STATUS_TOOLTIP, getCalculation().getCalculationStatusGml() );
      actions.add( openStatusLogAction );
      openStatusLogAction.updateStatus();

      // FIXME: better layout of actions
      actions.add( new OpenOutputZipAction( MessageConstants.STR_ACTION_OPEN_CALC_LOG_TEXT, MessageConstants.STR_ACTION_OPEN_CALC_LOG_TOOLTIP, getCalculation().getOutputZip(), false ) ); //$NON-NLS-1$ //$NON-NLS-2$
      actions.add( new OpenOutputZipAction( MessageConstants.STR_ACTION_OPEN_ERROR_LOG_TEXT, MessageConstants.STR_ACTION_OPEN_ERROR_LOG_TOOLTIP, getCalculation().getOutputZip(), true ) ); //$NON-NLS-1$ //$NON-NLS-2$
      // actions.add( new OpenOutputZipAction( "Open output log (calculation core)", "Displays the output log.", m_simulation, false ) );
      actions.add( new OpenTextLogAction( MessageConstants.STR_ACTION_OPEN_MASS_BALANCE_TEXT, MessageConstants.STR_ACTION_OPEN_MASS_BALANCE_TOOLTIP, getCalculation().getBilanzTxt() ) ); //$NON-NLS-1$ //$NON-NLS-2$
      actions.add( new OpenTextLogAction( MessageConstants.STR_ACTION_OPEN_STATISTICS_TEXT, MessageConstants.STR_ACTION_OPEN_STATISTICS_TOOLTIP, getCalculation().getStatisticsCsv(), "xls" ) ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
    }
    else
      actions.add( new DeleteRrmCalcualtionsAction( getSimulation(), getView() ) );

    Collections.addAll( actions, getAdditionalActions() );

    final Composite body = toolkit.createComposite( actionPanel );
    GridLayoutFactory.swtDefaults().numColumns( 2 ).equalWidth( true ).applyTo( body );
    body.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, false ) );

    /* Create the image hyperlinks. */
    for( final Action action : actions )
    {
      final ImageHyperlink imageHyperlink = ActionHyperlink.createHyperlink( null, body, SWT.NONE, action );
      imageHyperlink.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
      imageHyperlink.setText( action.getText() );

      if( action instanceof IUpdateable )
        ((IUpdateable)action).update();
    }

  }

  /**
   * overwrite for additional actions
   */
  protected Action[] getAdditionalActions( )
  {
    return new Action[] {};
  }

  protected RrmCalculationResult[] doFindCalculations( final IStructuredSelection selection )
  {
    final Set<RrmCalculationResult> results = new LinkedHashSet<>();

    final Iterator< ? > itr = selection.iterator();
    while( itr.hasNext() )
    {
      final TreeNode node = (TreeNode)itr.next();
      final Object data = node.getData();

      if( data instanceof CalculationFeatureBean )
      {
        final CalculationFeatureBean bean = (CalculationFeatureBean)data;
        results.add( bean.getCalculation() );
      }
      else if( data instanceof RrmResultBean )
      {
        final RrmResultBean bean = (RrmResultBean)data;
        results.add( bean.getCalculation() );
      }
      else if( data instanceof HydrologyResultReference )
      {
        final HydrologyResultReference reference = (HydrologyResultReference)data;
        results.add( reference.getCalculation() );
      }
      else if( data instanceof RrmCalculationResult )
        results.add( (RrmCalculationResult)data );
    }

    return results.toArray( new RrmCalculationResult[] {} );
  }
}
