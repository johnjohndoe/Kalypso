/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Path;
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
import org.eclipse.swt.graphics.Image;
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
import org.kalypso.afgui.scenarios.SzenarioDataProvider;
import org.kalypso.contribs.eclipse.core.runtime.PluginUtilities;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.jface.viewers.DefaultTableViewer;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.conv.results.ResultMeta1d2dHelper;
import org.kalypso.kalypsomodel1d2d.ops.CalcUnitOps;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.ICalcUnitResultMeta;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IScenarioResultMeta;
import org.kalypso.kalypsomodel1d2d.sim.Model1D2DSimulation;
import org.kalypso.kalypsomodel1d2d.ui.map.calculation_unit.CalculationUnitViewerLabelProvider;
import org.kalypso.kalypsomodel1d2d.ui.map.calculation_unit.wizards.CreateCalculationUnitWizard;
import org.kalypso.kalypsomodel1d2d.ui.map.cmds.calcunit.DeleteCalculationUnitCmd;
import org.kalypso.kalypsomodel1d2d.ui.map.facedata.ICommonKeys;
import org.kalypso.kalypsomodel1d2d.ui.map.facedata.KeyBasedDataModel;
import org.kalypso.kalypsomodel1d2d.ui.map.facedata.KeyBasedDataModelChangeListener;
import org.kalypso.kalypsomodel1d2d.ui.map.facedata.KeyBasedDataModelUtil;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper2;
import org.kalypsodeegree.model.geometry.GM_Envelope;

/**
 * Calculation unit widget component that shows the table of existing calculation units, with the corresponding buttons
 * 
 * TODO: get images from central point in order to have them disposed
 * 
 * @author Madanagopal
 * @author Dejan Antanaskovic
 */
public class CalculationUnitMetaTable implements ICalculationUnitButtonIDs
{
  /**
   * Helper class to dispose the image of a button once the button gets disposed.
   * 
   * @author Gernot Belger
   */
  private final class DisposeButtonImageListener implements DisposeListener
  {
    private Button m_button;

    public DisposeButtonImageListener( final Button button )
    {
      m_button = button;
    }

    public void widgetDisposed( final DisposeEvent e )
    {
      m_button.getImage().dispose();
      m_button = null;
    }
  }

  private final KeyBasedDataModel m_dataModel;

  final private ISelectionChangedListener m_selectListener = new ISelectionChangedListener()
  {
    @SuppressWarnings("synthetic-access")
    public void selectionChanged( SelectionChangedEvent event )
    {
      try
      {
        final IStructuredSelection selection = (IStructuredSelection) event.getSelection();
        if( selection == null )
          return;
        final Object firstElement = selection.getFirstElement();
        if( firstElement == null )
          return;
        else
        {
          if( firstElement instanceof IFeatureWrapper2 )
          {
            final IFeatureWrapper2 firstElementWrapper = (IFeatureWrapper2) firstElement;
            setCurrentSelection( firstElementWrapper );
          }
        }
      }
      catch( Throwable th )
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

  public CalculationUnitMetaTable( final KeyBasedDataModel dataModel, final String... buttonsList )
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

    return composite;
  }

  private TableViewer createTableControl( final Composite parent, final FormToolkit toolkit )
  {
    final DefaultTableViewer tableViewer = new DefaultTableViewer( parent, SWT.FULL_SELECTION | SWT.NONE );

    tableViewer.addColumn( "Name", "Name", null, 100, 100, false, SWT.LEFT, false ); //$NON-NLS-1$

    tableViewer.setContentProvider( new ArrayContentProvider() );
    tableViewer.setLabelProvider( new CalculationUnitViewerLabelProvider( parent.getDisplay() ) );

    tableViewer.setInput( setInputContentProvider() );
    tableViewer.addSelectionChangedListener( m_selectListener );

    final Composite btnComposite = toolkit.createComposite( parent, SWT.NONE );
    btnComposite.setLayout( new GridLayout( 1, false ) );
    btnComposite.setLayoutData( new GridData() );
    if( m_buttonsList.contains( ICalculationUnitButtonIDs.BTN_MOVE_UP ) )
    {
      final Button moveUpBtn = new Button( btnComposite, SWT.PUSH );
      final Image imageUp = new Image( btnComposite.getDisplay(), KalypsoModel1D2DPlugin.imageDescriptorFromPlugin( PluginUtilities.id( KalypsoModel1D2DPlugin.getDefault() ), "icons/elcl16/list_up.gif" ).getImageData() ); //$NON-NLS-1$
      moveUpBtn.setImage( imageUp );
      moveUpBtn.addDisposeListener( new DisposeButtonImageListener( moveUpBtn ) );
      moveUpBtn.addSelectionListener( new SelectionAdapter()
      {
        @Override
        public void widgetSelected( final SelectionEvent event )
        {
          moveSelection( -1 );
          tableViewer.refresh();
        }
      } );

      moveUpBtn.setToolTipText( "Move Up #" );
    }

    if( m_buttonsList.contains( ICalculationUnitButtonIDs.BTN_MOVE_DOWN ) )
    {
      final Button moveDownBtn = new Button( btnComposite, SWT.PUSH );
      final Image imageDown = new Image( btnComposite.getDisplay(), KalypsoModel1D2DPlugin.imageDescriptorFromPlugin( PluginUtilities.id( KalypsoModel1D2DPlugin.getDefault() ), "icons/elcl16/list_down.gif" ).getImageData() ); //$NON-NLS-1$
      moveDownBtn.setImage( imageDown );
      moveDownBtn.addDisposeListener( new DisposeButtonImageListener( moveDownBtn ) );
      moveDownBtn.addSelectionListener( new SelectionAdapter()
      {
        @Override
        public void widgetSelected( final SelectionEvent event )
        {
          moveSelection( 1 );
          tableViewer.refresh();
        }
      } );
      moveDownBtn.setToolTipText( "Move Down #" );
    }

    if( m_buttonsList.contains( ICalculationUnitButtonIDs.BTN_SHOW_AND_MAXIMIZE ) )
    {
      m_btnMaximizeCalcUnit = new Button( btnComposite, SWT.PUSH );
      final Image image = new Image( btnComposite.getDisplay(), KalypsoModel1D2DPlugin.imageDescriptorFromPlugin( PluginUtilities.id( KalypsoModel1D2DPlugin.getDefault() ), "icons/elcl16/17_show_calculationunit.gif" ).getImageData() );
      m_btnMaximizeCalcUnit.setImage( image );
      m_btnMaximizeCalcUnit.addDisposeListener( new DisposeButtonImageListener( m_btnMaximizeCalcUnit ) );
      m_btnMaximizeCalcUnit.addSelectionListener( new SelectionAdapter()
      {
        @SuppressWarnings("synthetic-access")
        @Override
        public void widgetSelected( final SelectionEvent event )
        {
          maximizeSelected();
        }
      } );
      m_btnMaximizeCalcUnit.setToolTipText( Messages.getString( "CalculationUnitMetaTable.Tooltip.BTN_SHOW_AND_MAXIMIZE" ) ); //$NON-NLS-1$
      m_btnMaximizeCalcUnit.setEnabled( false );
    }

    if( m_buttonsList.contains( ICalculationUnitButtonIDs.BTN_REMOVE ) )
    {
      m_btnDeleteCalcUnit = new Button( btnComposite, SWT.PUSH );
      final Image image = new Image( btnComposite.getDisplay(), KalypsoModel1D2DPlugin.imageDescriptorFromPlugin( PluginUtilities.id( KalypsoModel1D2DPlugin.getDefault() ), "icons/elcl16/19_cut_calculationunit.gif" ).getImageData() ); //$NON-NLS-1$
      m_btnDeleteCalcUnit.setImage( image );
      m_btnDeleteCalcUnit.addDisposeListener( new DisposeButtonImageListener( m_btnDeleteCalcUnit ) );
      m_btnDeleteCalcUnit.addSelectionListener( new SelectionAdapter()
      {
        @Override
        public void widgetSelected( final SelectionEvent event )
        {
          try
          {
            if( MessageDialog.openConfirm( parent.getShell(), Messages.getString( "CalculationUnitMetaTable.15" ), Messages.getString( "CalculationUnitMetaTable.14" ) ) ) //$NON-NLS-1$ //$NON-NLS-2$
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
      m_btnDeleteCalcUnit.setToolTipText( Messages.getString( "CalculationUnitMetaTable.Tooltip.BTN_REMOVE" ) ); //$NON-NLS-1$
      m_btnDeleteCalcUnit.setEnabled( false );
    }

    if( m_buttonsList.contains( ICalculationUnitButtonIDs.BTN_ADD ) )
    {
      m_btnCreateCalcUnit = new Button( btnComposite, SWT.PUSH );
      final Image image = new Image( btnComposite.getDisplay(), KalypsoModel1D2DPlugin.imageDescriptorFromPlugin( PluginUtilities.id( KalypsoModel1D2DPlugin.getDefault() ), "icons/elcl16/18_add_calculationunit.gif" ).getImageData() ); //$NON-NLS-1$
      m_btnCreateCalcUnit.setImage( image );
      m_btnCreateCalcUnit.addDisposeListener( new DisposeButtonImageListener( m_btnCreateCalcUnit ) );
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
      m_btnCreateCalcUnit.setToolTipText( Messages.getString( "CalculationUnitMetaTable.Tooltip.BTN_ADD" ) ); //$NON-NLS-1$
    }

    if( m_buttonsList.contains( ICalculationUnitButtonIDs.BTN_CLICK_TO_CALCULATE ) )
    {
      m_btnRunCalculation = new Button( btnComposite, SWT.PUSH );
      final Image image = new Image( btnComposite.getDisplay(), KalypsoModel1D2DPlugin.imageDescriptorFromPlugin( PluginUtilities.id( KalypsoModel1D2DPlugin.getDefault() ), "icons/startCalculation.gif" ).getImageData() ); //$NON-NLS-1$
      m_btnRunCalculation.addDisposeListener( new DisposeButtonImageListener( m_btnRunCalculation ) );
      m_btnRunCalculation.setImage( image );
      m_btnRunCalculation.addSelectionListener( new SelectionAdapter()
      {
        @Override
        public void widgetSelected( final SelectionEvent event )
        {
          handleRunPressed( event );
        }
      } );
      m_btnRunCalculation.setToolTipText( Messages.getString( "CalculationUnitMetaTable.Tooltip.BTN_CLICK_TO_CALCULATE" ) ); //$NON-NLS-1$
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
          IStatus status = ResultMeta1d2dHelper.removeResult( calcUnitResultMeta );
          if( status != Status.OK_STATUS )
            return StatusUtilities.createErrorStatus( "Fehler beim Löschen der Ergebnisse des Teilmodells " + calcUnitToDel.getName() );
        }
      }
      catch( CoreException e )
      {
        e.printStackTrace();
        return StatusUtilities.statusFromThrowable( e, "Fehler beim Löschen der Ergebnisse des Teilmodells " + calcUnitToDel.getName() );
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
          List<ICalculationUnit> calUnits = CalcUnitOps.getModelCalculationUnits( model1d2d );
          dataModel.setData( ICommonKeys.KEY_FEATURE_WRAPPER_LIST, calUnits );
          // set current selection to null
          dataModel.setData( ICommonKeys.KEY_SELECTED_FEATURE_WRAPPER, null );
        }
      };
      KeyBasedDataModelUtil.postCommand( dataModel, delCmd, ICommonKeys.KEY_COMMAND_MANAGER_DISC_MODEL );

    }
    return Status.OK_STATUS;
  }

  protected void moveSelection( @SuppressWarnings("unused")
  final int delta )
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
  protected List<ICalculationUnit> setInputContentProvider( )
  {
    final Object inputData = getDataModel().getData( ICommonKeys.KEY_FEATURE_WRAPPER_LIST );
    if( inputData != null )
      return (List<ICalculationUnit>) inputData;

    return new ArrayList<ICalculationUnit>();
  }

  private void maximizeSelected( )
  {
    final KeyBasedDataModel dataModel = getDataModel();
    final ICalculationUnit calUnitToMax = dataModel.getData( ICalculationUnit.class, ICommonKeys.KEY_SELECTED_FEATURE_WRAPPER );
    if( calUnitToMax == null )
      return;
    final GM_Envelope boundingBox = CalcUnitOps.getBoundingBox( calUnitToMax );
    if( boundingBox == null )
      return;
    final MapPanel mapPanel = dataModel.getData( MapPanel.class, ICommonKeys.KEY_MAP_PANEL );
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
        IFeatureWrapper2 currentSelection = getCurrentSelection();
        updateOnNewSelection( display, tableViewer, currentSelection );
      }
    };
    display.syncExec( changeInputRunnable );
  }

  final void updateOnNewSelection( final Display display, final TableViewer tableViewer, final Object currentSelection )
  {
    final Runnable runnable = new Runnable()
    {
      @SuppressWarnings("synthetic-access")
      public void run( )
      {
        tableViewer.refresh();
        boolean isEnabled = currentSelection instanceof IFeatureWrapper2;
        if( m_btnDeleteCalcUnit != null )
          m_btnDeleteCalcUnit.setEnabled( isEnabled );
        if( m_btnMaximizeCalcUnit != null )
          m_btnMaximizeCalcUnit.setEnabled( isEnabled );
        if( m_btnRunCalculation != null )
          m_btnRunCalculation.setEnabled( isEnabled );
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

      final SzenarioDataProvider caseDataProvider = (SzenarioDataProvider) getDataModel().getData( ICommonKeys.KEY_DATA_PROVIDER );
      final IContainer scenarioFolder = caseDataProvider.getScenarioFolder();

      final IFolder unitFolder = scenarioFolder.getFolder( new Path( "results/" + calculationUnit.getGmlID() ) );

      final Model1D2DSimulation runnable = new Model1D2DSimulation( shell, caseDataProvider, scenarioFolder, unitFolder, calculationUnit.getGmlID() );
      runnable.process();

      // Force fire a data change event here, so the log get updated now
      getDataModel().fireDataChanged( ICommonKeys.KEY_SELECTED_FEATURE_WRAPPER, calculationUnit );
    }
  }

}
