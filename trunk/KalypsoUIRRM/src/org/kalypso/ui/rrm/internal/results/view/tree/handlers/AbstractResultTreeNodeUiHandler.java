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

import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

import org.apache.commons.lang3.StringUtils;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.ToolBarManager;
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
import org.kalypso.contribs.eclipse.swt.layout.Layouts;
import org.kalypso.model.hydrology.binding.model.Catchment;
import org.kalypso.model.hydrology.binding.model.channels.StorageChannel;
import org.kalypso.model.hydrology.binding.model.nodes.Node;
import org.kalypso.model.hydrology.project.RrmCalculationResult;
import org.kalypso.model.hydrology.project.RrmSimulation;
import org.kalypso.ui.rrm.internal.i18n.Messages;
import org.kalypso.ui.rrm.internal.results.view.ResultManagementView;
import org.kalypso.ui.rrm.internal.results.view.base.CalculationFeatureBean;
import org.kalypso.ui.rrm.internal.results.view.base.HydrologyResultReference;
import org.kalypso.ui.rrm.internal.results.view.base.RrmResultBean;
import org.kalypso.ui.rrm.internal.simulations.actions.DeleteRrmCalcualtionsAction;
import org.kalypso.ui.rrm.internal.simulations.actions.OpenOutputZipAction;
import org.kalypso.ui.rrm.internal.simulations.actions.OpenTextLogAction;
import org.kalypso.ui.rrm.internal.utils.featureTree.AbstractTreeNodeUiHandler;
import org.kalypso.ui.rrm.internal.utils.featureTree.TreeNode;
import org.kalypsodeegree.model.feature.Feature;

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

    final List<Action> actions = new ArrayList<Action>();
    if( getCalculation() != null )
    {
      actions.add( new OpenTextLogAction( Messages.getString( "AbstractResultTreeNodeUiHandler_1" ), Messages.getString( "AbstractResultTreeNodeUiHandler_2" ), getCalculation().getCalculationLog() ) ); //$NON-NLS-1$ //$NON-NLS-2$
      actions.add( new OpenOutputZipAction( Messages.getString( "AbstractResultTreeNodeUiHandler_3" ), Messages.getString( "AbstractResultTreeNodeUiHandler_4" ), getCalculation().getOutputZip(), true ) ); //$NON-NLS-1$ //$NON-NLS-2$
      // actions.add( new OpenOutputZipAction( "Open output log (calculation core)", "Displays the output log.",
// m_simulation, false ) );
      actions.add( new OpenTextLogAction( Messages.getString( "AbstractResultTreeNodeUiHandler_5" ), Messages.getString( "AbstractResultTreeNodeUiHandler_6" ), getCalculation().getBilanzTxt() ) ); //$NON-NLS-1$ //$NON-NLS-2$
      actions.add( new OpenTextLogAction( Messages.getString( "AbstractResultTreeNodeUiHandler_7" ), Messages.getString( "AbstractResultTreeNodeUiHandler_8" ), getCalculation().getStatisticsCsv() ) ); //$NON-NLS-1$ //$NON-NLS-2$

    }
    else
      actions.add( new DeleteRrmCalcualtionsAction( getSimulation(), getView() ) );

    Collections.addAll( actions, getAdditionalActions() );

    final Composite body = toolkit.createComposite( actionPanel );
    body.setLayout( Layouts.createGridLayout( 2, true ) );
    body.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, false ) );

    /* Create the image hyperlinks. */
    for( final Action action : actions )
    {
      final ImageHyperlink imageHyperlink = ActionHyperlink.createHyperlink( null, body, SWT.NONE, action );
      imageHyperlink.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
      imageHyperlink.setText( action.getText() );

      if( action instanceof IUpdateable )
        ((IUpdateable) action).update();
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
      final TreeNode node = (TreeNode) itr.next();
      final Object data = node.getData();

      if( data instanceof CalculationFeatureBean )
      {
        final CalculationFeatureBean bean = (CalculationFeatureBean) data;
        results.add( bean.getCalculation() );
      }
      else if( data instanceof RrmResultBean )
      {
        final RrmResultBean bean = (RrmResultBean) data;
        results.add( bean.getCalculation() );
      }
      else if( data instanceof HydrologyResultReference )
      {
        final HydrologyResultReference reference = (HydrologyResultReference) data;
        results.add( reference.getCalculation() );
      }
      else if( data instanceof RrmCalculationResult )
        results.add( (RrmCalculationResult) data );
    }

    return results.toArray( new RrmCalculationResult[] {} );
  }

  @Override
  protected String getTreeCompareLabel( )
  {
    if( this instanceof HydrologyCalculationCaseGroupUiHandler )
    {
      final String label = getTreeLabel();
      if( StringUtils.equalsIgnoreCase( label, "aktuell" ) ) //$NON-NLS-1$
        return String.format( "AAA_%s", label ); //$NON-NLS-1$
      else if( StringUtils.equalsIgnoreCase( label, "berechnet" ) ) //$NON-NLS-1$
        return String.format( "AAA_%s", label ); //$NON-NLS-1$

      return String.format( "ZZZ_%s", label ); //$NON-NLS-1$
    }
    else if( this instanceof HydrologyParameterSetUiHandler )
    {
      final HydrologyParameterSetUiHandler handler = (HydrologyParameterSetUiHandler) this;
      final Feature feature = handler.getFeature();

      if( feature instanceof Catchment )
      {
        return String.format( "AAA_%s", getTreeLabel() ); //$NON-NLS-1$
      }
      else if( feature instanceof Node )
      {
        return String.format( "BBB_%s", getTreeLabel() ); //$NON-NLS-1$
      }
      else if( feature instanceof StorageChannel )
      {
        return String.format( "CCC_%s", getTreeLabel() ); //$NON-NLS-1$
      }
    }
    else if( this instanceof ResultCategoryUiHandler )
    {
      return String.format( "ZZZ_%s", getTreeLabel() ); //$NON-NLS-1$
    }

    return super.getTreeCompareLabel();
  }
}
