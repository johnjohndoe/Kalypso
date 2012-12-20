/** This file is part of Kalypso
 *
 *  Copyright (c) 2012 by
 *
 *  Björnsen Beratende Ingenieure GmbH, Koblenz, Germany (Bjoernsen Consulting Engineers), http://www.bjoernsen.de
 *  Technische Universität Hamburg-Harburg, Institut für Wasserbau, Hamburg, Germany
 *  (Technical University Hamburg-Harburg, Institute of River and Coastal Engineering), http://www.tu-harburg.de/wb/
 *
 *  Kalypso is free software: you can redistribute it and/or modify it under the terms  
 *  of the GNU Lesser General Public License (LGPL) as published by the Free Software 
 *  Foundation, either version 3 of the License, or (at your option) any later version.
 *
 *  Kalypso is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied 
 *  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with Kalypso.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.kalypso.kalypsomodel1d2d.ui.calculationUnitView;

import java.util.List;

import org.apache.commons.lang3.StringUtils;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.kalypso.afgui.KalypsoAFGUIFrameworkPlugin;
import org.kalypso.contribs.eclipse.core.runtime.PluginUtilities;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.jface.wizard.IUpdateable;
import org.kalypso.core.status.StatusDialog;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.conv.results.ResultMeta1d2dHelper;
import org.kalypso.kalypsomodel1d2d.ops.CalcUnitOps;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.schema.binding.model.IControlModelGroup;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.ICalcUnitResultMeta;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IScenarioResultMeta;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.ui.map.calculation_unit.CalculationUnitDataModel;
import org.kalypso.kalypsomodel1d2d.ui.map.cmds.calcunit.DeleteCalculationUnitCmd;
import org.kalypso.kalypsomodel1d2d.ui.map.facedata.ICommonKeys;
import org.kalypso.kalypsomodel1d2d.ui.map.facedata.KeyBasedDataModelUtil;

import de.renew.workflow.connector.cases.IScenarioDataProvider;

/**
 * Delete a calculation unit.
 * 
 * @author Gernot Belger
 */
class DeleteCalcUnitAction extends Action implements IUpdateable
{
  private final TableViewer m_unitViewer;

  private final CalculationUnitDataModel m_dataModel;

  public DeleteCalcUnitAction( final TableViewer viewer, final CalculationUnitDataModel dataModel )
  {
    super( StringUtils.EMPTY, Action.AS_PUSH_BUTTON );

    m_unitViewer = viewer;
    m_dataModel = dataModel;

    setImageDescriptor( AbstractUIPlugin.imageDescriptorFromPlugin( PluginUtilities.id( KalypsoModel1D2DPlugin.getDefault() ), "icons/elcl16/19_cut_calculationunit.gif" ) );//$NON-NLS-1$

    final String text = Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.calculationUnitView.CalculationUnitMetaTable.Tooltip.BTN_REMOVE" ); //$NON-NLS-1$
    // setText( text );
    setToolTipText( text );

    setEnabled( false );
  }

  @Override
  public void update( )
  {
    final ICalculationUnit calcUnitToDelete = findCalcUnitToDelete();

    final boolean isEnabled = calcUnitToDelete != null;

    setEnabled( isEnabled );
  }

  private ICalculationUnit findCalcUnitToDelete( )
  {
    final IStructuredSelection selection = (IStructuredSelection)m_unitViewer.getSelection();

    final Object firstElement = selection.getFirstElement();

    if( firstElement instanceof ICalculationUnit )
      return (ICalculationUnit)firstElement;

    return null;
  }

  @Override
  public void runWithEvent( final Event event )
  {
    final Shell shell = event.widget.getDisplay().getActiveShell();

    final String windowTitle = getToolTipText();

    final ICalculationUnit calcUnitToDel = findCalcUnitToDelete();
    final IFEDiscretisationModel1d2d model1d2d = m_dataModel.getData( IFEDiscretisationModel1d2d.class, ICommonKeys.KEY_DISCRETISATION_MODEL );

    /* check requirements first */
    final IStatus requirementStatus = checkRequirements( model1d2d, calcUnitToDel );
    if( !requirementStatus.isOK() )
    {
      StatusDialog.open( shell, requirementStatus, windowTitle );
      return;
    }

    /* ask user if he really wants to delete */
    if( !MessageDialog.openConfirm( shell, windowTitle, Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.calculationUnitView.CalculationUnitMetaTable.14" ) ) ) //$NON-NLS-1$ //$NON-NLS-2$
      return;

    /* do it */
    final IStatus status = deleteSelected( model1d2d, calcUnitToDel );
    if( !status.isOK() )
      StatusDialog.open( shell, status, windowTitle );

    m_unitViewer.refresh();
  }

  private IStatus checkRequirements( final IFEDiscretisationModel1d2d model1d2d, final ICalculationUnit calcUnitToDel )
  {
    if( calcUnitToDel == null )
    {
      // Will not happen, as this action is disabled in this case
      return new Status( IStatus.WARNING, KalypsoModel1D2DPlugin.PLUGIN_ID, "No calculation unit selected" ); //$NON-NLS-1$
    }

    /* check if unit is used elsewhere */
    if( DeleteCalculationUnitCmd.checkIsLinkedTo( model1d2d, calcUnitToDel ) )
      return new Status( IStatus.WARNING, KalypsoModel1D2DPlugin.PLUGIN_ID, Messages.getString("DeleteCalcUnitAction.0") ); //$NON-NLS-1$

    return Status.OK_STATUS;
  }

  private IStatus deleteSelected( final IFEDiscretisationModel1d2d model1d2d, final ICalculationUnit calcUnitToDel )
  {
    try
    {
      final IStatus deleteResults = deleteResults( calcUnitToDel );
      if( !deleteResults.isOK() )
        return deleteResults;

      /* delete calc unit */
      final IScenarioDataProvider szenarioDataProvider = KalypsoAFGUIFrameworkPlugin.getDataProvider();
      final IControlModelGroup modelGroup = szenarioDataProvider.getModel( IControlModelGroup.class.getName() );

      final DeleteCalculationUnitCmd delCmd = new DeleteCalculationUnitCmd( model1d2d, modelGroup, calcUnitToDel );

      KeyBasedDataModelUtil.postCommand( m_dataModel, delCmd, ICommonKeys.KEY_COMMAND_MANAGER_DISC_MODEL );

      // reset with list from model
      final List<ICalculationUnit> calUnits = CalcUnitOps.getModelCalculationUnits( model1d2d );
      m_dataModel.setData( ICommonKeys.KEY_FEATURE_WRAPPER_LIST, calUnits );
      // set current selection to null
      m_dataModel.setData( ICommonKeys.KEY_SELECTED_FEATURE_WRAPPER, null );

      return Status.OK_STATUS;

    }
    catch( final CoreException e )
    {
      e.printStackTrace();
      return StatusUtilities.statusFromThrowable( e, getToolTipText() );
    }
  }

  private IStatus deleteResults( final ICalculationUnit calcUnitToDel ) throws CoreException
  {
    final IScenarioDataProvider dataProvider = (IScenarioDataProvider)m_dataModel.getData( ICommonKeys.KEY_DATA_PROVIDER );

    // TODO: check for existing results
    /* get result meta */
    final IScenarioResultMeta scenarioResultMeta = dataProvider.getModel( IScenarioResultMeta.class.getName() );

    /* find calc model */
    final ICalcUnitResultMeta calcUnitResultMeta = scenarioResultMeta.findCalcUnitMetaResult( calcUnitToDel.getId() );
    if( calcUnitResultMeta == null )
      return Status.OK_STATUS;

    /* there are results */

    // TODO: generate warning for user
    /* delete results */
    final IStatus status = ResultMeta1d2dHelper.removeResult( calcUnitResultMeta );
    if( status.isOK() )
      return status;

    // FIXME: original status is lost ;-(
    final String message = Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.calculationUnitView.CalculationUnitMetaTable.5" ); //$NON-NLS-1$
    return new Status( IStatus.ERROR, KalypsoModel1D2DPlugin.PLUGIN_ID, message );
  }
}