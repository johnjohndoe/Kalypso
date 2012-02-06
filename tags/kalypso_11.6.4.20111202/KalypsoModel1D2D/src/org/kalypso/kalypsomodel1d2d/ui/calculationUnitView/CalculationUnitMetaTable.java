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
package org.kalypso.kalypsomodel1d2d.ui.calculationUnitView;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.CellEditor;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.TextCellEditor;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Table;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.kalypso.afgui.scenarios.SzenarioDataProvider;
import org.kalypso.contribs.eclipse.core.runtime.PluginUtilities;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.jface.dialog.DialogSettingsUtils;
import org.kalypso.contribs.eclipse.jface.viewers.DefaultTableViewer;
import org.kalypso.contribs.eclipse.jface.wizard.WizardDialog2;
import org.kalypso.contribs.eclipse.swt.widgets.DisposeButtonImageListener;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.conv.results.ResultMeta1d2dHelper;
import org.kalypso.kalypsomodel1d2d.ops.CalcUnitOps;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.ICalcUnitResultMeta;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IScenarioResultMeta;
import org.kalypso.kalypsomodel1d2d.sim.Model1D2DSimulation;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.ui.map.calculation_unit.CalculationUnitDataModel;
import org.kalypso.kalypsomodel1d2d.ui.map.calculation_unit.CalculationUnitViewerLabelProvider;
import org.kalypso.kalypsomodel1d2d.ui.map.calculation_unit.wizards.CalculationUnitPropertyWizard;
import org.kalypso.kalypsomodel1d2d.ui.map.calculation_unit.wizards.CreateCalculationUnitWizard;
import org.kalypso.kalypsomodel1d2d.ui.map.cmds.calcunit.DeleteCalculationUnitCmd;
import org.kalypso.kalypsomodel1d2d.ui.map.facedata.ICommonKeys;
import org.kalypso.kalypsomodel1d2d.ui.map.facedata.KeyBasedDataModel;
import org.kalypso.kalypsomodel1d2d.ui.map.facedata.KeyBasedDataModelChangeListener;
import org.kalypso.kalypsomodel1d2d.ui.map.facedata.KeyBasedDataModelUtil;
import org.kalypso.ogc.gml.map.IMapPanel;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper2;
import org.kalypsodeegree.model.feature.event.ModellEvent;
import org.kalypsodeegree.model.feature.event.ModellEventListener;
import org.kalypsodeegree.model.geometry.GM_Envelope;

/**
 * Calculation unit widget component that shows the table of existing calculation units, with the corresponding buttons
 * 
 * @author Madanagopal
 * @author Dejan Antanaskovic
 */
public class CalculationUnitMetaTable implements ICalculationUnitButtonIDs
{
  private final CalculationUnitDataModel m_dataModel;

  final private ISelectionChangedListener m_selectListener = new ISelectionChangedListener()
  {
    @Override
    @SuppressWarnings("synthetic-access")
    public void selectionChanged( final SelectionChangedEvent event )
    {
      try
      {
        final IStructuredSelection selection = (IStructuredSelection) event.getSelection();
        if( selection == null )
        {
          return;
        }
        final Object firstElement = selection.getFirstElement();
        if( firstElement == null )
        {
          return;
        }
        else
        {
          if( firstElement instanceof IFeatureWrapper2 )
          {
            final IFeatureWrapper2 firstElementWrapper = (IFeatureWrapper2) firstElement;
            setCurrentSelection( firstElementWrapper );
          }
        }
      }
      catch( final Throwable th )
      {
        th.printStackTrace();
      }
    }
  };

  private final Set<String> m_buttonsList = new HashSet<String>();

  private Button m_btnDeleteCalcUnit;

  private Button m_btnMaximizeCalcUnit;

  private Button m_btnCreateCalcUnit;

  private Button m_btnRunCalculation;

  private Button m_btnEditCalcUnit;

  public CalculationUnitMetaTable( final CalculationUnitDataModel dataModel, final String... buttonsList )
  {
    m_dataModel = dataModel;

    m_buttonsList.addAll( Arrays.asList( buttonsList ) );
  }

  public Control createControl( final Composite parent, final FormToolkit toolkit )
  {
    final Composite composite = toolkit.createComposite( parent, SWT.NONE );
    final GridLayout gridLayout = new GridLayout( 2, false );
    gridLayout.marginHeight = 0;
    gridLayout.marginWidth = 0;
    composite.setLayout( gridLayout );

    final TableViewer tableViewer = createTableControl( composite, toolkit );
    final Table table = tableViewer.getTable();
    toolkit.adapt( table );
    table.setLinesVisible( true );
    table.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, true ) );

    final Display display = parent.getDisplay();
    m_dataModel.addKeyBasedDataChangeListener( new KeyBasedDataModelChangeListener()
    {
      @Override
      public void dataChanged( final String key, final Object newValue )
      {
        if( ICommonKeys.KEY_FEATURE_WRAPPER_LIST.equals( key ) )
        {
          updateOnNewInput( display, tableViewer, newValue );
        }
        else if( ICommonKeys.KEY_SELECTED_FEATURE_WRAPPER.equals( key ) )
        {
          updateOnNewSelection( display, tableViewer, newValue );
        }
        else
        {
          // uninteresting key
        }
      }
    } );

    final IFEDiscretisationModel1d2d discModel = m_dataModel.getData( IFEDiscretisationModel1d2d.class, ICommonKeys.KEY_DISCRETISATION_MODEL );
    final GMLWorkspace workspace = discModel.getFeature().getWorkspace();
    final ModellEventListener modelListener = new ModellEventListener()
    {
      @Override
      public void onModellChange( final ModellEvent modellEvent )
      {
        final Runnable runnable = new Runnable()
        {
          @Override
          public void run( )
          {
            final Object newValue = m_dataModel.getData( ICommonKeys.KEY_FEATURE_WRAPPER_LIST );
            updateOnNewInput( display, tableViewer, newValue );
          }
        };
        display.syncExec( runnable );
      }
    };
    workspace.addModellListener( modelListener );

    composite.addDisposeListener( new DisposeListener()
    {
      @Override
      public void widgetDisposed( final DisposeEvent e )
      {
        workspace.removeModellListener( modelListener );
      }
    } );

    return composite;
  }

  private TableViewer createTableControl( final Composite parent, final FormToolkit toolkit )
  {
    final DefaultTableViewer tableViewer = new DefaultTableViewer( parent, SWT.FULL_SELECTION | SWT.NONE );

    tableViewer.addColumn( "Name", Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.calculationUnitView.CalculationUnitMetaTable.0" ), null, 100, 100, false, SWT.LEFT, false, false ); //$NON-NLS-1$ //$NON-NLS-2$

    tableViewer.setContentProvider( new ArrayContentProvider() );
    tableViewer.setLabelProvider( new CalculationUnitViewerLabelProvider( parent.getDisplay() ) );

    tableViewer.setInput( getCalcUnits() );
    tableViewer.addSelectionChangedListener( m_selectListener );

    final Composite btnComposite = toolkit.createComposite( parent, SWT.NONE );
    btnComposite.setLayout( new GridLayout( 1, false ) );
    btnComposite.setLayoutData( new GridData() );
    if( m_buttonsList.contains( ICalculationUnitButtonIDs.BTN_MOVE_UP ) )
    {
      final Button moveUpBtn = new Button( btnComposite, SWT.PUSH );
      moveUpBtn.setImage( AbstractUIPlugin.imageDescriptorFromPlugin( PluginUtilities.id( KalypsoModel1D2DPlugin.getDefault() ), "icons/elcl16/list_up.gif" ).createImage() );//$NON-NLS-1$
      DisposeButtonImageListener.hookToButton( moveUpBtn );
      moveUpBtn.addSelectionListener( new SelectionAdapter()
      {
        @Override
        public void widgetSelected( final SelectionEvent event )
        {
          moveSelection( -1 );
          tableViewer.refresh();
        }
      } );

      moveUpBtn.setToolTipText( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.calculationUnitView.CalculationUnitMetaTable.1" ) ); //$NON-NLS-1$
    }

    if( m_buttonsList.contains( ICalculationUnitButtonIDs.BTN_MOVE_DOWN ) )
    {
      final Button moveDownBtn = new Button( btnComposite, SWT.PUSH );
      moveDownBtn.setImage( AbstractUIPlugin.imageDescriptorFromPlugin( PluginUtilities.id( KalypsoModel1D2DPlugin.getDefault() ), "icons/elcl16/list_down.gif" ).createImage() );//$NON-NLS-1$
      DisposeButtonImageListener.hookToButton( moveDownBtn );
      moveDownBtn.addSelectionListener( new SelectionAdapter()
      {
        @Override
        public void widgetSelected( final SelectionEvent event )
        {
          moveSelection( 1 );
          tableViewer.refresh();
        }
      } );
      moveDownBtn.setToolTipText( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.calculationUnitView.CalculationUnitMetaTable.2" ) ); //$NON-NLS-1$
    }

    if( m_buttonsList.contains( ICalculationUnitButtonIDs.BTN_SHOW_AND_MAXIMIZE ) )
    {
      m_btnMaximizeCalcUnit = new Button( btnComposite, SWT.PUSH );
      m_btnMaximizeCalcUnit.setImage( AbstractUIPlugin.imageDescriptorFromPlugin( PluginUtilities.id( KalypsoModel1D2DPlugin.getDefault() ), "icons/elcl16/17_show_calculationunit.gif" ).createImage() );//$NON-NLS-1$
      DisposeButtonImageListener.hookToButton( m_btnMaximizeCalcUnit );
      m_btnMaximizeCalcUnit.addSelectionListener( new SelectionAdapter()
      {
        @SuppressWarnings("synthetic-access")
        @Override
        public void widgetSelected( final SelectionEvent event )
        {
          maximizeSelected();
        }
      } );
      m_btnMaximizeCalcUnit.setToolTipText( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.calculationUnitView.CalculationUnitMetaTable.Tooltip.BTN_SHOW_AND_MAXIMIZE" ) ); //$NON-NLS-1$
      m_btnMaximizeCalcUnit.setEnabled( false );
    }

    if( m_buttonsList.contains( ICalculationUnitButtonIDs.BTN_REMOVE ) )
    {
      m_btnDeleteCalcUnit = new Button( btnComposite, SWT.PUSH );
      m_btnDeleteCalcUnit.setImage( AbstractUIPlugin.imageDescriptorFromPlugin( PluginUtilities.id( KalypsoModel1D2DPlugin.getDefault() ), "icons/elcl16/19_cut_calculationunit.gif" ).createImage() );//$NON-NLS-1$
      DisposeButtonImageListener.hookToButton( m_btnDeleteCalcUnit );
      m_btnDeleteCalcUnit.addSelectionListener( new SelectionAdapter()
      {
        @Override
        public void widgetSelected( final SelectionEvent event )
        {
          try
          {
            if( MessageDialog.openConfirm( parent.getShell(), Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.calculationUnitView.CalculationUnitMetaTable.15" ), Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.calculationUnitView.CalculationUnitMetaTable.14" ) ) ) //$NON-NLS-1$ //$NON-NLS-2$
            {
              deleteSelected();
              tableViewer.refresh();
            }
          }
          catch( final Throwable th )
          {
            th.printStackTrace();
          }
        }
      } );
      m_btnDeleteCalcUnit.setToolTipText( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.calculationUnitView.CalculationUnitMetaTable.Tooltip.BTN_REMOVE" ) ); //$NON-NLS-1$
      m_btnDeleteCalcUnit.setEnabled( false );
    }

    if( m_buttonsList.contains( ICalculationUnitButtonIDs.BTN_ADD ) )
    {
      m_btnCreateCalcUnit = new Button( btnComposite, SWT.PUSH );
      m_btnCreateCalcUnit.setImage( AbstractUIPlugin.imageDescriptorFromPlugin( PluginUtilities.id( KalypsoModel1D2DPlugin.getDefault() ), "icons/elcl16/18_add_calculationunit.gif" ).createImage() );//$NON-NLS-1$
      DisposeButtonImageListener.hookToButton( m_btnCreateCalcUnit );
      m_btnCreateCalcUnit.addSelectionListener( new SelectionAdapter()
      {
        @Override
        public void widgetSelected( final SelectionEvent event )
        {
          try
          {
            createFeatureWrapper();
            final int newEntryPosition = tableViewer.getTable().getItemCount() - 1;
            tableViewer.getTable().select( newEntryPosition );
          }
          catch( final Throwable th )
          {
            th.printStackTrace();
          }
        }
      } );
      m_btnCreateCalcUnit.setToolTipText( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.calculationUnitView.CalculationUnitMetaTable.Tooltip.BTN_ADD" ) ); //$NON-NLS-1$
    }

    if( m_buttonsList.contains( ICalculationUnitButtonIDs.BTN_EDIT ) )
    {
      m_btnEditCalcUnit = new Button( btnComposite, SWT.PUSH );
      m_btnEditCalcUnit.setImage( AbstractUIPlugin.imageDescriptorFromPlugin( PluginUtilities.id( KalypsoModel1D2DPlugin.getDefault() ), "icons/elcl16/20_edit_calculationunit.gif" ).createImage() );//$NON-NLS-1$
      DisposeButtonImageListener.hookToButton( m_btnEditCalcUnit );
      final CalculationUnitDataModel dataModel = m_dataModel;
      m_btnEditCalcUnit.addSelectionListener( new SelectionAdapter()
      {
        @Override
        public void widgetSelected( final SelectionEvent event )
        {
          if( m_dataModel.getSelectedCalculationUnit() == null )
          {
            return;
          }

          final Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell();
          final CalculationUnitPropertyWizard calculationSubWizard = new CalculationUnitPropertyWizard( dataModel );
          calculationSubWizard.setDialogSettings( DialogSettingsUtils.getDialogSettings( KalypsoModel1D2DPlugin.getDefault(), "org.kalypso.kalypsomodel1d2d.ui.calculationUnitView.CalculationSubWizard" ) ); //$NON-NLS-1$
          final WizardDialog2 wizardDialog = new WizardDialog2( shell, calculationSubWizard );
          wizardDialog.setRememberSize( true );
          wizardDialog.open();
        }
      } );
      m_btnEditCalcUnit.setToolTipText( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.calculationUnitView.CalculationUnitMetaTable.Tooltip.BTN_EDIT" ) ); //$NON-NLS-1$
      m_btnEditCalcUnit.setEnabled( false );
    }

    if( m_buttonsList.contains( ICalculationUnitButtonIDs.BTN_CLICK_TO_CALCULATE ) )
    {
      m_btnRunCalculation = new Button( btnComposite, SWT.PUSH );
      m_btnRunCalculation.setImage( AbstractUIPlugin.imageDescriptorFromPlugin( PluginUtilities.id( KalypsoModel1D2DPlugin.getDefault() ), "icons/startCalculation.gif" ).createImage() );//$NON-NLS-1$
      DisposeButtonImageListener.hookToButton( m_btnRunCalculation );
      m_btnRunCalculation.addSelectionListener( new SelectionAdapter()
      {
        @Override
        public void widgetSelected( final SelectionEvent event )
        {
          handleRunPressed( event );
        }
      } );
      m_btnRunCalculation.setToolTipText( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.calculationUnitView.CalculationUnitMetaTable.Tooltip.BTN_CLICK_TO_CALCULATE" ) ); //$NON-NLS-1$
      m_btnRunCalculation.setEnabled( false );
    }

    final TextCellEditor textCellEditor = new TextCellEditor( tableViewer.getTable() );
    final CellEditor[] editors = new CellEditor[] { textCellEditor };
    tableViewer.setCellEditors( editors );

    return tableViewer;
  }

  protected IStatus deleteSelected( )
  {
    final KeyBasedDataModel dataModel = getDataModel();
    final ICalculationUnit calcUnitToDel = dataModel.getData( ICalculationUnit.class, ICommonKeys.KEY_SELECTED_FEATURE_WRAPPER );
    final SzenarioDataProvider caseDataProvider = (SzenarioDataProvider) getDataModel().getData( ICommonKeys.KEY_DATA_PROVIDER );
    if( calcUnitToDel != null )
    {
      // TODO: check for existing results
      try
      {
        /* get result meta */
        final IScenarioResultMeta scenarioResultMeta = caseDataProvider.getModel( IScenarioResultMeta.class );

        /* find calc model */
        final ICalcUnitResultMeta calcUnitResultMeta = scenarioResultMeta.findCalcUnitMetaResult( calcUnitToDel.getGmlID() );
        if( calcUnitResultMeta != null )
        {
          /* there are results */

          // TODO: generate warning for user
          /* delete results */
          final IStatus status = ResultMeta1d2dHelper.removeResult( calcUnitResultMeta );
          if( status != Status.OK_STATUS )
          {
            return StatusUtilities.createErrorStatus( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.calculationUnitView.CalculationUnitMetaTable.4" ) + calcUnitToDel.getName() ); //$NON-NLS-1$
          }
        }
      }
      catch( final CoreException e )
      {
        e.printStackTrace();
        return StatusUtilities.statusFromThrowable( e, Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.calculationUnitView.CalculationUnitMetaTable.5" ) + calcUnitToDel.getName() ); //$NON-NLS-1$
      }

      /* delete calc unit */
      final IFEDiscretisationModel1d2d model1d2d = dataModel.getData( IFEDiscretisationModel1d2d.class, ICommonKeys.KEY_DISCRETISATION_MODEL );
      final DeleteCalculationUnitCmd delCmd = new DeleteCalculationUnitCmd( model1d2d, calcUnitToDel )
      {
        /**
         * @see org.kalypso.kalypsomodel1d2d.ui.map.cmds.calcunit.DeleteCalculationUnit#process()
         */
        @Override
        public void process( ) throws Exception
        {
          super.process();
          // reset with list from model
          final List<ICalculationUnit> calUnits = CalcUnitOps.getModelCalculationUnits( model1d2d );
          dataModel.setData( ICommonKeys.KEY_FEATURE_WRAPPER_LIST, calUnits );
          // set current selection to null
          dataModel.setData( ICommonKeys.KEY_SELECTED_FEATURE_WRAPPER, null );
        }
      };
      KeyBasedDataModelUtil.postCommand( dataModel, delCmd, ICommonKeys.KEY_COMMAND_MANAGER_DISC_MODEL );

    }
    return Status.OK_STATUS;
  }

  protected void moveSelection( @SuppressWarnings("unused") final int delta )
  {
    throw new UnsupportedOperationException();
  }

  protected void createFeatureWrapper( )
  {
    final Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell();

    final CreateCalculationUnitWizard calculationWizard = new CreateCalculationUnitWizard( getDataModel() );
    final WizardDialog wizardDialog = new WizardDialog( shell, calculationWizard );
    wizardDialog.open();
  }

  @SuppressWarnings("unchecked")
  protected List<ICalculationUnit> getCalcUnits( )
  {
    final Object inputData = getDataModel().getData( ICommonKeys.KEY_FEATURE_WRAPPER_LIST );
    if( inputData != null )
    {
      return (List<ICalculationUnit>) inputData;
    }

    return new ArrayList<ICalculationUnit>();
  }

  private void maximizeSelected( )
  {
    final KeyBasedDataModel dataModel = getDataModel();
    final ICalculationUnit calUnitToMax = dataModel.getData( ICalculationUnit.class, ICommonKeys.KEY_SELECTED_FEATURE_WRAPPER );
    if( calUnitToMax == null )
    {
      return;
    }
    final GM_Envelope boundingBox = CalcUnitOps.getBoundingBox( calUnitToMax );
    if( boundingBox == null )
    {
      return;
    }
    final IMapPanel mapPanel = dataModel.getData( IMapPanel.class, ICommonKeys.KEY_MAP_PANEL );
    mapPanel.setBoundingBox( boundingBox );
  }

  private void setCurrentSelection( final IFeatureWrapper2 firstElement )
  {
    m_dataModel.setData( ICommonKeys.KEY_SELECTED_FEATURE_WRAPPER, firstElement );
  }

  protected IFeatureWrapper2 getCurrentSelection( )
  {
    return (IFeatureWrapper2) m_dataModel.getData( ICommonKeys.KEY_SELECTED_FEATURE_WRAPPER );
  }

  public KeyBasedDataModel getDataModel( )
  {
    return m_dataModel;
  }

  /**
   * Update the gui components to reflect the table new input
   */
  final void updateOnNewInput( final Display display, final TableViewer tableViewer, final Object input )
  {
    final Runnable changeInputRunnable = new Runnable()
    {
      /**
       * @see java.lang.Runnable#run()
       */
      @Override
      public void run( )
      {
        if( input == null )
        {
          tableViewer.setInput( new Object[] {} );
        }
        else
        {
          tableViewer.setInput( input );
        }
        final IFeatureWrapper2 currentSelection = getCurrentSelection();
        updateOnNewSelection( display, tableViewer, currentSelection );
      }
    };
    display.syncExec( changeInputRunnable );
  }

  final void updateOnNewSelection( final Display display, final TableViewer tableViewer, final Object currentSelection )
  {
    final Runnable runnable = new Runnable()
    {
      @Override
      @SuppressWarnings("synthetic-access")
      public void run( )
      {
        tableViewer.refresh();
        final boolean isEnabled = currentSelection instanceof IFeatureWrapper2;
        if( m_btnDeleteCalcUnit != null )
        {
          m_btnDeleteCalcUnit.setEnabled( isEnabled );
        }
        if( m_btnEditCalcUnit != null )
        {
          m_btnEditCalcUnit.setEnabled( isEnabled );
        }
        if( m_btnMaximizeCalcUnit != null )
        {
          m_btnMaximizeCalcUnit.setEnabled( isEnabled );
        }
        if( m_btnRunCalculation != null )
        {
          m_btnRunCalculation.setEnabled( isEnabled );
        }
      }
    };
    display.syncExec( runnable );
  }

  protected void handleRunPressed( final SelectionEvent event )
  {
    final ICalculationUnit calculationUnit = getDataModel().getData( ICalculationUnit.class, ICommonKeys.KEY_SELECTED_FEATURE_WRAPPER );

    if( calculationUnit != null )
    {
      final Shell shell = event.display.getActiveShell();
      final Model1D2DSimulation runnable = new Model1D2DSimulation( shell );
      runnable.process( calculationUnit );

      // Force fire a data change event here, so the log get updated now
      getDataModel().fireDataChanged( ICommonKeys.KEY_SELECTED_FEATURE_WRAPPER, calculationUnit );
    }
  }

}
