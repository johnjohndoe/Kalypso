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
package org.kalypso.kalypsomodel1d2d.ui.calculationUnitView;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.kalypso.contribs.eclipse.core.runtime.PluginUtilities;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.eclipse.jface.operation.RunnableContextHelper;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit1D2D;
import org.kalypso.kalypsomodel1d2d.ui.calculationUnitView.invariants.InvariantBConditionWithBLine;
import org.kalypso.kalypsomodel1d2d.ui.calculationUnitView.invariants.InvariantCheckBoundaryConditions;
import org.kalypso.kalypsomodel1d2d.ui.calculationUnitView.invariants.InvariantOverlappingElements;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.ui.map.calculation_unit.CalculationUnitDataModel;
import org.kalypso.kalypsomodel1d2d.ui.map.facedata.ICommonKeys;
import org.kalypso.kalypsomodel1d2d.ui.map.facedata.KeyBasedDataModelChangeListener;
import org.kalypso.kalypsomodel1d2d.ui.map.facedata.KeyBasedDataModelUtil;
import org.kalypso.kalypsosimulationmodel.core.flowrel.IFlowRelationshipModel;
import org.kalypso.ogc.gml.map.IMapPanel;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypsodeegree.model.feature.Feature;

/**
 * @author Madanagopal
 */
public class CalculationUnitProblemsComponent
{
  private final CalculationUnitDataModel m_dataModel;

  private final ISelectionChangedListener selectionChangedListener = new ISelectionChangedListener()
  {
    @Override
    public void selectionChanged( final SelectionChangedEvent event )
    {
      handleSelectionChanged( event );
    }
  };

  private ICalculationUnit m_calcUnit;

  public CalculationUnitProblemsComponent( final CalculationUnitDataModel dataModel )
  {
    m_dataModel = dataModel;
  }

  public void createControl( final FormToolkit toolkit, final Composite parent )
  {
    final TableViewer guiProblemViewer = guiProblemViewer( parent, toolkit );

    final CalculationUnitDataModel dataModel = m_dataModel;
    dataModel.addKeyBasedDataChangeListener( new KeyBasedDataModelChangeListener()
    {
      @Override
      public void dataChanged( final String key, final Object newValue )
      {
        final Display display = parent.getDisplay();
        final Runnable runnable = new Runnable()
        {
          @Override
          public void run( )
          {
            if( ICommonKeys.KEY_SELECTED_FEATURE_WRAPPER.equals( key ) && newValue instanceof ICalculationUnit )
            {
              final ICalculationUnit calcUnit = setCalcUnit( (ICalculationUnit) newValue );
              guiProblemViewer.setInput( dataModel.getValidatingMessages( calcUnit ) );
            }
          }
        };
        display.syncExec( runnable );
      }
    } );
  }

  private TableViewer guiProblemViewer( final Composite composite, final FormToolkit toolkit )
  {
    final Composite rootComposite = new Composite( composite, SWT.FLAT );
    rootComposite.setLayout( new FormLayout() );

    FormData formData = new FormData();
    formData.top = new FormAttachment( 0, 0 );
    formData.left = new FormAttachment( 0, 0 );
    rootComposite.setLayoutData( formData );

    final Label nameText = new Label( rootComposite, SWT.NONE );
    nameText.setText( "Problems :" ); //$NON-NLS-1$

    formData = new FormData();
    formData.left = new FormAttachment( 0, 5 );
    formData.top = new FormAttachment( 0, 5 );
    nameText.setLayoutData( formData );

    final Button refreshButton = new Button( rootComposite, SWT.NONE );
    final Image refreshImage = new Image( rootComposite.getDisplay(), KalypsoModel1D2DPlugin.imageDescriptorFromPlugin( PluginUtilities.id( KalypsoModel1D2DPlugin.getDefault() ), "icons/elcl16/refresh.gif" ).getImageData() ); //$NON-NLS-1$
    refreshButton.setImage( refreshImage );

    formData = new FormData();
    formData.left = new FormAttachment( nameText, 10 );
    formData.top = new FormAttachment( 0, 5 );
    refreshButton.setLayoutData( formData );

    final TableViewer problemTableViewer = new TableViewer( rootComposite, SWT.FILL | SWT.BORDER );
    problemTableViewer.setLabelProvider( new ProblemsListLabelProvider() );
    problemTableViewer.setContentProvider( new ArrayContentProvider() );
    problemTableViewer.addSelectionChangedListener( selectionChangedListener );

    final CalculationUnitDataModel dataModel = m_dataModel;
    refreshButton.addSelectionListener( new SelectionAdapter()
    {
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        final IWorkbench workbench = PlatformUI.getWorkbench();
        final ICalculationUnit calcUnit = getCalculationUnit();
        if( calcUnit != null )
        {
          validateInvariants( workbench, calcUnit, problemTableViewer.getTable().getShell() );
          problemTableViewer.setInput( dataModel.getValidatingMessages( calcUnit ) );
          problemTableViewer.refresh();
        }
      }
    } );

    final Table problemsTable = problemTableViewer.getTable();
    problemsTable.setLinesVisible( true );

    final TableColumn lineColumn = new TableColumn( problemsTable, SWT.LEFT );
    lineColumn.setWidth( 200 );

    formData = new FormData();
    formData.left = new FormAttachment( 0, 5 );
    formData.top = new FormAttachment( refreshButton, 5 );
    formData.right = new FormAttachment( 100, -5 );
    problemsTable.setLayoutData( formData );

    return problemTableViewer;
  }

  protected void validateInvariants( final IWorkbench workbench, final ICalculationUnit currentSelection, final Shell shell )
  {
    final CalculationUnitDataModel dataModel = m_dataModel;
    final ICoreRunnableWithProgress runnable = new ICoreRunnableWithProgress()
    {
      @Override
      public IStatus execute( final IProgressMonitor monitor )
      {
        monitor.beginTask( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.calculationUnitView.CalculationUnitProblemsComponent.0" ), 10 ); //$NON-NLS-1$

        if( currentSelection != null )
        {
          final List<IProblem> tempProblemList = new ArrayList<IProblem>();
          dataModel.addValidatingMessage( currentSelection, tempProblemList );

          // TODO:
          // 1) validation reports wrong problems
          // 2) refaktor in order to use some kind of rule-set for problems

          final InvariantCheckBoundaryConditions checkBC = new InvariantCheckBoundaryConditions();
          tempProblemList.addAll( checkBC.checkAllInvariants( currentSelection ) );

          final CommandableWorkspace workspace = KeyBasedDataModelUtil.getBCWorkSpace( m_dataModel );
          final Feature bcHolderFeature = workspace.getRootFeature();
          final IFlowRelationshipModel flowRelationship = (IFlowRelationshipModel) bcHolderFeature.getAdapter( IFlowRelationshipModel.class );

          final InvariantBConditionWithBLine invBConditionBLine = new InvariantBConditionWithBLine( flowRelationship );
          tempProblemList.addAll( invBConditionBLine.checkAllInvariants( currentSelection ) );

          monitor.worked( 5 );

          if( currentSelection instanceof ICalculationUnit1D2D )
          {
            final ICalculationUnit1D2D calc1D2D = (ICalculationUnit1D2D) currentSelection;

            final InvariantOverlappingElements overlappingElements = new InvariantOverlappingElements();
            tempProblemList.addAll( overlappingElements.checkAllInvariants( calc1D2D ) );

            monitor.worked( 5 );
          }
        }

        return Status.OK_STATUS;
      }
    };

    final IStatus status = RunnableContextHelper.execute( workbench.getProgressService(), true, true, runnable );
    ErrorDialog.openError( shell, Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.calculationUnitView.CalculationUnitProblemsComponent.1" ), Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.calculationUnitView.CalculationUnitProblemsComponent.2" ), status ); //$NON-NLS-1$ //$NON-NLS-2$
  }

  public ICalculationUnit getCalculationUnit( )
  {
    return m_calcUnit;
  }

  protected ICalculationUnit setCalcUnit( final ICalculationUnit newValue )
  {
    m_calcUnit = newValue;
    return newValue;
  }

  protected void handleSelectionChanged( final SelectionChangedEvent event )
  {
    final IStructuredSelection selection = (IStructuredSelection) event.getSelection();

    if( selection.isEmpty() )
      return;

    final Object firstElement = selection.getFirstElement();
    if( firstElement instanceof IProblem )
    {
      final IProblem problem = (IProblem) firstElement;

      final IMapPanel mapPanel = m_dataModel.getData( IMapPanel.class, ICommonKeys.KEY_MAP_PANEL );
      problem.navigateToProblem( mapPanel );
    }
  }
}
