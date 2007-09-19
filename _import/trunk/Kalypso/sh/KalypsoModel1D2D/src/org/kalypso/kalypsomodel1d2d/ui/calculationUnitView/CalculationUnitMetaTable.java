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

import java.util.List;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.CellEditor;
import org.eclipse.jface.viewers.IBaseLabelProvider;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.TextCellEditor;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.kalypso.contribs.eclipse.core.runtime.PluginUtilities;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.ops.CalcUnitOps;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit;
import org.kalypso.kalypsomodel1d2d.sim.CalculationUnitSimMode1D2DCalcJob;
import org.kalypso.kalypsomodel1d2d.ui.map.facedata.ICommonKeys;
import org.kalypso.kalypsomodel1d2d.ui.map.facedata.KeyBasedDataModel;
import org.kalypso.kalypsomodel1d2d.ui.map.facedata.KeyBasedDataModelChangeListener;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper2;
import org.kalypsodeegree.model.geometry.GM_Envelope;

/**
 * Calculation unit widget component that shows the table of existing calculation units, with the corresponding buttons
 * 
 * @author Madanagopal
 * @author Dejan Antanaskovic
 */
public abstract class CalculationUnitMetaTable implements ICalculationUnitButtonIDs
{
  private TableViewer tableViewer;

  private KeyBasedDataModel m_dataModel;

  private Image image;

  private Image imageDown;

  private Image imageUp;

  private Composite m_parent;

  final String bTextMaximizeSelected = "Geländemodell anzeigen und maximieren";

  final String deleteSelected = "Geländemodell löschen";

  final String calculateSelected = "Run Calculation";

  final String defaultTestDecription = "Wählen Sie ein Modell aus.";

  final String saveToolTip = "Deskription Sichern";

  final String titleDescriptionGroup = "Beschreibung";

  final private SelectionListener moveUpListener = new SelectionAdapter()
  {
    @Override
    public void widgetSelected( SelectionEvent event )
    {
      moveSelection( -1 );
      tableViewer.refresh();
    }
  };

  final private SelectionListener moveDownListener = new SelectionAdapter()
  {
    @Override
    public void widgetSelected( SelectionEvent event )
    {
      moveSelection( 1 );
      tableViewer.refresh();
    }
  };

  final private ISelectionChangedListener elevationModelSelectListener = new ISelectionChangedListener()
  {
    @SuppressWarnings("synthetic-access")
    public void selectionChanged( SelectionChangedEvent event )
    {
      try
      {
        IStructuredSelection selection = (IStructuredSelection) event.getSelection();
        if( selection == null )
        {
          System.out.println( "Selection is null" );
          return;
        }
        Object firstElement = selection.getFirstElement();
        if( firstElement == null )
        {
          return;// throw new NullPointerException( "Null Value while selection.getFirstElement() :" + firstElement );
        }
        else
        {
          if( firstElement instanceof IFeatureWrapper2 )
          {
            IFeatureWrapper2 firstElementWrapper = (IFeatureWrapper2) firstElement;
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

  private Label descriptionLabel;

  private Group descriptionGroupText;

  private Text descriptionText;

  private String[] buttonsList;

  private Button saveButton;

  private final KeyBasedDataModelChangeListener dataModelListener = new KeyBasedDataModelChangeListener()
  {
    @SuppressWarnings("synthetic-access")
    public void dataChanged( String key, Object newValue )
    {
      if( ICommonKeys.KEY_FEATURE_WRAPPER_LIST.equals( key ) )
      {
        updateOnNewInput( newValue );
      }
      else if( ICommonKeys.KEY_SELECTED_FEATURE_WRAPPER.equals( key ) )
      {
        updateOnNewSelection( newValue );
      }
      else
      {
        // uninteresting key
      }
    }
  };

  private Button m_btnDeleteCalcUnit;

  private Button m_btnMaximizeCalcUnit;

  private Button m_btnCreateCalcUnit;

  private Button m_btnRunCalculation;

  public void createControl( final KeyBasedDataModel dataModel, final FormToolkit toolkit, final Composite parent )
  {
    m_parent = parent;
    m_dataModel = dataModel;
    guiSelectFromList( parent );
    dataModel.addKeyBasedDataChangeListener( this.dataModelListener );
  }

  private void guiSelectFromList( final Composite parent )
  {
    FormData formData;

    formData = new FormData();
    formData.left = new FormAttachment( 0, 10 );
    formData.top = new FormAttachment( 0, 5 );
    formData.bottom = new FormAttachment( 100, 0 );

    tableViewer = new TableViewer( parent, SWT.FILL | SWT.BORDER );
    final Table table = tableViewer.getTable();
    tableViewer.setContentProvider( new ArrayContentProvider() );
    tableViewer.setLabelProvider( getLabelProvider( parent.getDisplay() ) );
    table.setLinesVisible( true );
    table.setLayoutData( formData );

    final TableColumn lineColumn = new TableColumn( table, SWT.LEFT );
    lineColumn.setWidth( 100 );

    tableViewer.setInput( setInputContentProvider() );
    tableViewer.addSelectionChangedListener( this.elevationModelSelectListener );

    formData = new FormData();
    formData.left = new FormAttachment( table, 5 );
    formData.bottom = new FormAttachment( 100, 0 );
    formData.top = new FormAttachment( 0, 5 );

    final Composite btnComposite = new Composite( parent, SWT.NONE );
    btnComposite.setLayout( new GridLayout( 1, false ) );
    btnComposite.setLayoutData( formData );
    if( searchForThisString( ICalculationUnitButtonIDs.BTN_MOVE_UP ) )
    {
      final Button moveUpBtn = new Button( btnComposite, SWT.PUSH );
      imageUp = new Image( btnComposite.getDisplay(), KalypsoModel1D2DPlugin.imageDescriptorFromPlugin( PluginUtilities.id( KalypsoModel1D2DPlugin.getDefault() ), "icons/elcl16/list_up.gif" ).getImageData() );
      moveUpBtn.setImage( imageUp );
      moveUpBtn.addSelectionListener( this.moveUpListener );

      moveUpBtn.setToolTipText( getBtnDescription( ICalculationUnitButtonIDs.BTN_MOVE_UP ) != null ? getBtnDescription( ICalculationUnitButtonIDs.BTN_MOVE_UP ) : "Move Up #" );
    }

    if( searchForThisString( ICalculationUnitButtonIDs.BTN_MOVE_DOWN ) )
    {
      final Button moveDownBtn = new Button( btnComposite, SWT.PUSH );
      imageDown = new Image( btnComposite.getDisplay(), KalypsoModel1D2DPlugin.imageDescriptorFromPlugin( PluginUtilities.id( KalypsoModel1D2DPlugin.getDefault() ), "icons/elcl16/list_down.gif" ).getImageData() );
      moveDownBtn.setImage( imageDown );
      moveDownBtn.addSelectionListener( this.moveDownListener );
      moveDownBtn.setToolTipText( getBtnDescription( ICalculationUnitButtonIDs.BTN_MOVE_DOWN ) != null ? getBtnDescription( ICalculationUnitButtonIDs.BTN_MOVE_DOWN ) : "Move Down #" );
    }

    if( searchForThisString( ICalculationUnitButtonIDs.BTN_SHOW_AND_MAXIMIZE ) )
    {
      m_btnMaximizeCalcUnit = new Button( btnComposite, SWT.PUSH );
      // clickToRunBtn.setToolTipText( bTextMaximizeSelected );
      image = new Image( btnComposite.getDisplay(), KalypsoModel1D2DPlugin.imageDescriptorFromPlugin( PluginUtilities.id( KalypsoModel1D2DPlugin.getDefault() ), "icons/elcl16/17_show_calculationunit.gif" ).getImageData() );
      m_btnMaximizeCalcUnit.setImage( image );
      m_btnMaximizeCalcUnit.addSelectionListener( new SelectionAdapter()
      {
        @Override
        public void widgetSelected( final SelectionEvent event )
        {
          maximizeSelected();
        }
      } );
      m_btnMaximizeCalcUnit.setToolTipText( Messages.getString( "CalculationUnitMetaTable.Tooltip.BTN_SHOW_AND_MAXIMIZE" ) );
      m_btnMaximizeCalcUnit.setEnabled( false );
    }

    if( searchForThisString( ICalculationUnitButtonIDs.BTN_REMOVE ) )
    {
      m_btnDeleteCalcUnit = new Button( btnComposite, SWT.PUSH );
      image = new Image( btnComposite.getDisplay(), KalypsoModel1D2DPlugin.imageDescriptorFromPlugin( PluginUtilities.id( KalypsoModel1D2DPlugin.getDefault() ), "icons/elcl16/19_cut_calculationunit.gif" ).getImageData() );
      m_btnDeleteCalcUnit.setImage( image );
      m_btnDeleteCalcUnit.addSelectionListener( new SelectionAdapter()
      {
        @Override
        public void widgetSelected( final SelectionEvent event )
        {
          try
          {
            if( MessageDialog.openConfirm( parent.getShell(), "Bitte bestätigen", "Wollen Sie die Berechnungseinheit wirklich löschen?" ) )
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
      m_btnDeleteCalcUnit.setToolTipText( Messages.getString( "CalculationUnitMetaTable.Tooltip.BTN_REMOVE" ) );
      m_btnDeleteCalcUnit.setEnabled( false );
    }

    if( searchForThisString( ICalculationUnitButtonIDs.BTN_ADD ) )
    {
      m_btnCreateCalcUnit = new Button( btnComposite, SWT.PUSH );
      // addButton.setToolTipText( deleteSelected );
      image = new Image( btnComposite.getDisplay(), KalypsoModel1D2DPlugin.imageDescriptorFromPlugin( PluginUtilities.id( KalypsoModel1D2DPlugin.getDefault() ), "icons/elcl16/18_add_calculationunit.gif" ).getImageData() );
      m_btnCreateCalcUnit.setImage( image );
      m_btnCreateCalcUnit.addSelectionListener( new SelectionAdapter()
      {
        @Override
        public void widgetSelected( final SelectionEvent event )
        {
          try
          {
            createFeatureWrapper();
            int newEntryPosition = tableViewer.getTable().getItemCount() - 1;
            tableViewer.getTable().select( newEntryPosition );
          }
          catch( final Throwable th )
          {
            th.printStackTrace();
          }
        }
      } );
      m_btnCreateCalcUnit.setToolTipText( Messages.getString( "CalculationUnitMetaTable.Tooltip.BTN_ADD" ) );
    }

    if( searchForThisString( ICalculationUnitButtonIDs.BTN_CLICK_TO_CALCULATE ) )
    {
      m_btnRunCalculation = new Button( btnComposite, SWT.PUSH );
      // calculateButton.setToolTipText( calculateSelected );
      image = new Image( btnComposite.getDisplay(), KalypsoModel1D2DPlugin.imageDescriptorFromPlugin( PluginUtilities.id( KalypsoModel1D2DPlugin.getDefault() ), "icons/startCalculation.gif" ).getImageData() );
      m_btnRunCalculation.setImage( image );
      m_btnRunCalculation.addSelectionListener( new SelectionAdapter()
      {
        @Override
        public void widgetSelected( final SelectionEvent event )
        {
          final ICalculationUnit calculationUnit = getDataModel().getData( ICalculationUnit.class, ICommonKeys.KEY_SELECTED_FEATURE_WRAPPER );
          if( calculationUnit != null )
          {
            final IWorkbench workbench = PlatformUI.getWorkbench();
            final IStatus result = CalculationUnitSimMode1D2DCalcJob.startCalculation( calculationUnit, workbench );
            ErrorDialog.openError( parent.getShell(), Messages.getString( "CalculationUnitPerformComponent.2" ), Messages.getString( "CalculationUnitPerformComponent.3" ), result ); //$NON-NLS-1$ //$NON-NLS-2$
          }
        }
      } );
      m_btnRunCalculation.setToolTipText( Messages.getString( "CalculationUnitMetaTable.Tooltip.BTN_CLICK_TO_CALCULATE" ) );
      m_btnRunCalculation.setEnabled( false );
    }

    final TextCellEditor textCellEditor = new TextCellEditor( table );
    final CellEditor[] editors = new CellEditor[] { textCellEditor };
    tableViewer.setCellEditors( editors );
    tableViewer.setColumnProperties( new String[] { "Name" } );
  }

  /*
   * Template Methods
   */
  protected boolean showDescription( )
  {
    return false;
  }

  protected IBaseLabelProvider getLabelProvider( final Display display )
  {
    return null;
  }

  protected void deleteSelected( ) throws Exception
  {

  }

  protected void moveSelection( final int delta )
  {

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

  public void setRequiredButtons( final String... buttonsList )
  {
    this.buttonsList = buttonsList;
  }

  /**
   * Template Method -- Override by inheriting Class
   */
  protected String getBtnDescription( final String key )
  {
    return null;
  }

  public boolean searchForThisString( final String searchStr )
  {
    for( final String i : buttonsList )
    {
      if( i.compareTo( searchStr ) == 0 )
        return true;
    }
    return false;
  }

  private void setCurrentSelection( final IFeatureWrapper2 firstElement )
  {
    m_dataModel.setData( ICommonKeys.KEY_SELECTED_FEATURE_WRAPPER, firstElement );
  }

  protected IFeatureWrapper2 getCurrentSelection( )
  {
    return (IFeatureWrapper2) m_dataModel.getData( ICommonKeys.KEY_SELECTED_FEATURE_WRAPPER );
  }

  public void createFeatureWrapper( )
  {

  }

  public KeyBasedDataModel getDataModel( )
  {
    return m_dataModel;
  }

  /**
   * Update the gui components to reflect the table new input
   */
  final void updateOnNewInput( final Object input )
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
        updateOnNewSelection( currentSelection );
      }
    };
    final Display display = m_parent.getDisplay();
    display.syncExec( changeInputRunnable );
  }

  final void updateOnNewSelection( final Object currentSelection )
  {
    final Runnable runnable = new Runnable()
    {
      @SuppressWarnings("synthetic-access")
      public void run( )
      {
        refreshTableView();
        boolean isEnabled = currentSelection instanceof IFeatureWrapper2;
        if( m_btnDeleteCalcUnit != null )
          m_btnDeleteCalcUnit.setEnabled( isEnabled );
        if( m_btnMaximizeCalcUnit != null )
          m_btnMaximizeCalcUnit.setEnabled( isEnabled );
        if( m_btnRunCalculation != null )
          m_btnRunCalculation.setEnabled( isEnabled );
      }
    };
    final Display display = m_parent.getDisplay();
    display.syncExec( runnable );
  }

  final void refreshTableView( )
  {
    final Runnable runnable = new Runnable()
    {
      public void run( )
      {
        tableViewer.refresh();
      }
    };
    final Display display = m_parent.getDisplay();
    display.syncExec( runnable );
  }

  public void refreshOtherSections( )
  {

  }

  protected List<ICalculationUnit> setInputContentProvider( )
  {
    return null;
  }

}
