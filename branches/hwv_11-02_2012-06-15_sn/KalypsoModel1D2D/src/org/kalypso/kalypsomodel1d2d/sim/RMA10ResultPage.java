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
package org.kalypso.kalypsomodel1d2d.sim;

import java.io.File;
import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import org.apache.commons.vfs.FileObject;
import org.eclipse.core.resources.IContainer;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.resource.JFaceResources;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.CheckStateChangedEvent;
import org.eclipse.jface.viewers.CheckboxTableViewer;
import org.eclipse.jface.viewers.ICheckStateListener;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.wizard.IWizardContainer;
import org.eclipse.jface.wizard.IWizardPage;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Spinner;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableItem;
import org.kalypso.afgui.model.ICommandPoster;
import org.kalypso.afgui.model.IModel;
import org.kalypso.commons.command.EmptyCommand;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.jface.operation.RunnableContextHelper;
import org.kalypso.contribs.eclipse.jface.wizard.WizardDialog2;
import org.kalypso.core.status.StatusComposite;
import org.kalypso.kalypsomodel1d2d.schema.binding.model.IControlModel1D2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IScenarioResultMeta;
import org.kalypso.kalypsomodel1d2d.sim.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.ui.geolog.IGeoLog;

import de.renew.workflow.connector.cases.ICaseDataProvider;

/**
 * @author Gernot Belger
 */
public class RMA10ResultPage extends WizardPage implements IWizardPage, ISimulation1D2DConstants
{
  private final ResultManager m_resultManager;

  public ResultManager getResultManager( )
  {
    return m_resultManager;
  }

  private IStatus m_simulationStatus;

  private final IContainer m_unitFolder;

  private IStatus m_resultStatus;

  private final ICaseDataProvider<IModel> m_caseDataProvider;

  private StatusComposite m_statusComp;

  protected boolean m_deleteAllResults;

  private boolean m_isProcessing = false;

  private CheckboxTableViewer m_resultProcessViewer;

  private Date[] m_selection = null;

  private final RMA10CalculationWizard m_parentWizard;

  protected boolean m_evaluateFullResults;

  protected RMA10ResultPage( final String pageName, final FileObject fileObjectRMA, final FileObject fileObjectSWAN, final IGeoLog geoLog, final IContainer unitFolder, final ICaseDataProvider<IModel> caseDataProvider, final RMA10CalculationWizard parentWizard ) throws CoreException
  {
    super( pageName );
    final ResultManager resultManager = new ResultManager( fileObjectRMA, fileObjectSWAN, caseDataProvider, geoLog );

    m_resultManager = resultManager;
    m_unitFolder = unitFolder;
    m_caseDataProvider = caseDataProvider;
    m_parentWizard = parentWizard;

    setTitle( Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.RMA10ResultPage.0", m_resultManager.getControlModel().getCalculationUnit().getName() ) ); //$NON-NLS-1$

    if( m_unitFolder.exists() )
      setMessage( Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.RMA10ResultPage.1" ), WARNING ); //$NON-NLS-1$
    else
      setMessage( Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.RMA10ResultPage.2" ) ); //$NON-NLS-1$
  }

  /**
   * @see org.eclipse.jface.dialogs.IDialogPage#createControl(org.eclipse.swt.widgets.Composite)
   */
  @Override
  public void createControl( final Composite parent )
  {
    final Composite composite = new Composite( parent, SWT.NONE );
    composite.setLayout( new GridLayout() );

    /* Status composite */
    final Group statusGroup = new Group( composite, SWT.NONE );
    statusGroup.setLayout( new GridLayout() );
    statusGroup.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    statusGroup.setText( Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.RMA10ResultPage.3" ) ); //$NON-NLS-1$

    m_statusComp = new StatusComposite( statusGroup, StatusComposite.DETAILS );
    m_statusComp.setLayoutData( new GridData( SWT.FILL, SWT.LEFT, true, false ) );
    m_statusComp.setStatus( StatusUtilities.createStatus( IStatus.INFO, Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.RMA10ResultPage.4" ), null ) ); //$NON-NLS-1$

    /* Control flags */
    final Group tweakGroup = new Group( composite, SWT.NONE );
    tweakGroup.setLayout( new GridLayout() );
    tweakGroup.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    tweakGroup.setText( Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.RMA10ResultPage.5" ) ); //$NON-NLS-1$

    final IControlModel1D2D controlModel = m_resultManager.getControlModel();
    m_deleteAllResults = !controlModel.getRestart();

    final Button deleteAllCheck = new Button( tweakGroup, SWT.CHECK );
    deleteAllCheck.setText( Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.RMA10ResultPage.6" ) ); //$NON-NLS-1$
    deleteAllCheck.setToolTipText( Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.RMA10ResultPage.7" ) ); //$NON-NLS-1$
    deleteAllCheck.setSelection( m_deleteAllResults );
    deleteAllCheck.addSelectionListener( new SelectionAdapter()
    {
      /**
       * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
       */
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        m_deleteAllResults = deleteAllCheck.getSelection();
      }
    } );
    // If non-restart, always all results must be deleted.
    deleteAllCheck.setEnabled( !controlModel.getRestart() );

    final Button evaluateFullCheck = new Button( tweakGroup, SWT.CHECK );
    evaluateFullCheck.setText( Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.RMA10ResultPage.18" ) ); //$NON-NLS-1$
    evaluateFullCheck.setToolTipText( Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.RMA10ResultPage.19" ) ); //$NON-NLS-1$
    evaluateFullCheck.addSelectionListener( new SelectionAdapter()
    {
      /**
       * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
       */
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        m_evaluateFullResults = evaluateFullCheck.getSelection();
      }
    } );

    /* checklist with calculated steps to choose from */

    /* Result chooser */
    final Group resultChooserGroup = new Group( composite, SWT.NONE );
    resultChooserGroup.setLayout( new GridLayout() );
    resultChooserGroup.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );
    resultChooserGroup.setText( Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.RMA10ResultPage.8" ) ); //$NON-NLS-1$

    final Composite resultChooserComp = new Composite( resultChooserGroup, SWT.BORDER );
    resultChooserComp.setLayout( new FillLayout( SWT.HORIZONTAL ) );
    resultChooserComp.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );

    final Table table = new Table( resultChooserComp, SWT.BORDER | SWT.CHECK );
    m_resultProcessViewer = new CheckboxTableViewer( table );
    m_resultProcessViewer.setContentProvider( new ArrayContentProvider() );
    m_resultProcessViewer.setLabelProvider( new LabelProvider()
    {
      /**
       * @see org.eclipse.jface.viewers.LabelProvider#getText(java.lang.Object)
       */
      @Override
      public String getText( final Object element )
      {
        if( element instanceof Date )
        {
          final Date date = (Date) element;
          if( date.equals( MAXI_DATE ) )
            return Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.RMA10ResultPage.9" ); //$NON-NLS-1$
          else if( date.equals( STEADY_DATE ) )
            return Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.RMA10ResultPage.10" ); //$NON-NLS-1$
        }
        return super.getText( element );
      }
    }

    );

    try
    {
      final Date[] calculatedSteps = m_resultManager.findCalculatedSteps();
      m_resultProcessViewer.setInput( calculatedSteps );
      m_selection = calculatedSteps;
      m_resultManager.setStepsToProcess( m_selection, m_resultManager.getControlModel() );
    }
    catch( final IOException e )
    {
      e.printStackTrace();
    }
    getContainer().updateButtons();

    /* Info View for one result */
    final ResultInfoViewer infoViewer = new ResultInfoViewer( resultChooserComp, SWT.NONE );

    if( m_selection != null )
      m_resultProcessViewer.setCheckedElements( m_selection );

    m_resultProcessViewer.addSelectionChangedListener( new ISelectionChangedListener()
    {
      @Override
      public void selectionChanged( final SelectionChangedEvent event )
      {
        handleSelectionChanged( (IStructuredSelection) event.getSelection(), infoViewer );
      }
    } );

    m_resultProcessViewer.addCheckStateListener( new ICheckStateListener()
    {
      @Override
      public void checkStateChanged( final CheckStateChangedEvent event )
      {
        updateSelection();
      }
    } );

    addSelectionButtons( m_resultProcessViewer, resultChooserGroup );

    setControl( composite );
  }

  protected void handleSelectionChanged( final IStructuredSelection selection, final ResultInfoViewer infoViewer )
  {
    infoViewer.setInput( selection.getFirstElement() );
  }

  /**
   * Add the selection and deselection buttons to the page.
   */
  private void addSelectionButtons( final CheckboxTableViewer checkboxViewer, final Composite composite )
  {
    final Composite buttonComposite = new Composite( composite, SWT.NONE );
    final GridLayout layout = new GridLayout();
    layout.numColumns = 0;
    layout.marginWidth = 0;
    layout.horizontalSpacing = convertHorizontalDLUsToPixels( IDialogConstants.HORIZONTAL_SPACING );
    buttonComposite.setLayout( layout );
    buttonComposite.setLayoutData( new GridData( SWT.END, SWT.TOP, true, false ) );

    addSpinner( m_resultProcessViewer, composite );

    final String SELECT_ALL_TITLE = Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.RMA10ResultPage.16" );// WorkbenchMessages.SelectionDialog_selectLabel;
    final String DESELECT_ALL_TITLE = Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.RMA10ResultPage.17" );// WorkbenchMessages.SelectionDialog_deselectLabel;

    final Button selectButton = createButton( buttonComposite, IDialogConstants.SELECT_ALL_ID, SELECT_ALL_TITLE, false );

    selectButton.addSelectionListener( new SelectionAdapter()
    {
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        checkboxViewer.setAllChecked( true );
        updateSelection();
      }
    } );

    final Button deselectButton = createButton( buttonComposite, IDialogConstants.DESELECT_ALL_ID, DESELECT_ALL_TITLE, false );

    deselectButton.addSelectionListener( new SelectionAdapter()
    {
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        checkboxViewer.setAllChecked( false );
        updateSelection();
      }
    } );
  }

  /**
   * Creates a new button with the given id.
   * <p>
   * The <code>Dialog</code> implementation of this framework method creates a standard push button, registers it for
   * selection events including button presses, and registers default buttons with its shell. The button id is stored as
   * the button's client data. If the button id is <code>IDialogConstants.CANCEL_ID</code>, the new button will be
   * accessible from <code>getCancelButton()</code>. If the button id is <code>IDialogConstants.OK_ID</code>, the new
   * button will be accessible from <code>getOKButton()</code>. Note that the parent's layout is assumed to be a
   * <code>GridLayout</code> and the number of columns in this layout is incremented. Subclasses may override.
   * </p>
   * 
   * @param parent
   *          the parent composite
   * @param id
   *          the id of the button (see <code>IDialogConstants.*_ID</code> constants for standard dialog button ids)
   * @param label
   *          the label from the button
   * @param defaultButton
   *          <code>true</code> if the button is to be the default button, and <code>false</code> otherwise
   * @return the new button
   * @see #getCancelButton
   * @see #getOKButton()
   */
  protected Button createButton( final Composite parent, final int id, final String label, final boolean defaultButton )
  {
    // increment the number of columns in the button bar
    ((GridLayout) parent.getLayout()).numColumns++;
    final Button button = new Button( parent, SWT.PUSH );
    button.setText( label );
    button.setFont( JFaceResources.getDialogFont() );
    button.setData( new Integer( id ) );
    if( defaultButton )
    {
      final Shell shell = parent.getShell();
      if( shell != null )
      {
        shell.setDefaultButton( button );
      }
    }
    setButtonLayoutData( button );
    return button;
  }

  protected void updateSelection( )
  {
    final Object[] selection = m_resultProcessViewer.getCheckedElements();

    final List<Date> dateList = new ArrayList<Date>();
    for( final Object element : selection )
    {
      if( element instanceof Date )
        dateList.add( (Date) element );
    }

    m_selection = dateList.toArray( new Date[dateList.size()] );

    final IControlModel1D2D controlModel = m_resultManager.getControlModel();

    try
    {
      m_resultManager.setStepsToProcess( m_selection, controlModel );
    }
    catch( final IOException e )
    {
      e.printStackTrace();
    }
    getContainer().updateButtons();
  }

  private void addSpinner( final CheckboxTableViewer resultProcessViewer, final Composite composite )
  {
    final Composite buttonComposite = new Composite( composite, SWT.NONE );
    final GridLayout layout = new GridLayout();
    layout.numColumns = 2;
    layout.marginWidth = 0;
    layout.horizontalSpacing = convertHorizontalDLUsToPixels( IDialogConstants.HORIZONTAL_SPACING );
    buttonComposite.setLayout( layout );
    buttonComposite.setLayoutData( new GridData( SWT.END, SWT.CENTER, true, false ) );

    final Label spinnerLabel = new Label( buttonComposite, SWT.NONE );
    spinnerLabel.setText( Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.RMA10ResultPage.11" ) ); //$NON-NLS-1$
    final GridData gridData = new GridData( SWT.FILL, SWT.CENTER, true, false );
    spinnerLabel.setLayoutData( gridData );

    final Spinner spinNumStepProcessing = new Spinner( buttonComposite, SWT.NONE );

    final GridData gridDataSpin = new GridData( SWT.RIGHT, SWT.CENTER, true, false );

    spinNumStepProcessing.setLayoutData( gridDataSpin );
    spinNumStepProcessing.setDigits( 0 );
    spinNumStepProcessing.setMinimum( 1 );
    spinNumStepProcessing.setMaximum( 100 );
    final Integer resultIncrement = m_parentWizard.getResultIntervalFromCalcPage();
    spinNumStepProcessing.setSelection( resultIncrement );
    updateTableSelection( resultProcessViewer, spinNumStepProcessing );
    updateSelection();

    spinNumStepProcessing.setToolTipText( Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.RMA10ResultPage.12" ) ); //$NON-NLS-1$
    spinNumStepProcessing.addSelectionListener( new SelectionAdapter()
    {
      /**
       * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
       */
      @Override
      public void widgetSelected( final SelectionEvent e )
      {

        updateTableSelection( resultProcessViewer, spinNumStepProcessing );
        updateSelection();
      }
    } );

  }

  protected void updateTableSelection( final TableViewer resultProcessViewer, final Spinner spinNumStepProcessing )
  {
    final TableItem[] items = resultProcessViewer.getTable().getItems();
    final int selection = spinNumStepProcessing.getSelection();

    final int length = items.length;
    int start = 0;

    // TODO: handle only unsteady time steps
    for( int i = 0; i < length; i++ )
    {
      final Object data = items[i].getData();
      if( data instanceof Date )
      {
        final Date date = (Date) data;
        if( date == MAXI_DATE || date == STEADY_DATE )
        {
          start++;
          // always enable steady and maxi result
          items[i].setChecked( true );
        }
      }
    }

    for( int i = start; i < length; i++ )
    {
      final double mod = ((i - start)) % selection;
      if( mod == 0 )
        items[i].setChecked( true );
      else
        items[i].setChecked( false );

      if( i == length - 1 )
        items[i].setChecked( true );
    }
  }

  public void runResultProcessing( )
  {
    m_statusComp.setStatus( StatusUtilities.createStatus( IStatus.INFO, Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.RMA10ResultPage.13" ), null ) ); //$NON-NLS-1$
    setMessage( Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.RMA10ResultPage.14" ) ); //$NON-NLS-1$

    final ProcessResultsBean bean = new ProcessResultsBean();

    // Remark: if not restart, always delete everything, in that
    // case do not ask the user either
    // TODO: make ui for the bean
    bean.evaluateFullResults = m_evaluateFullResults;
    bean.deleteAll = m_deleteAllResults;
    bean.deleteFollowers = true;

    bean.userCalculatedSteps = m_selection;
    if( m_selection == null )
    {
      try
      {
        bean.userCalculatedSteps = m_resultManager.findCalculatedSteps();
      }
      catch( final IOException e )
      {
        e.printStackTrace();
      }
    }

    /* Result processing */
    final ResultProcessingOperation processingOperation = new ResultProcessingOperation( m_resultManager, bean );

    m_isProcessing = true;

    final IWizardContainer container = getContainer();
    if( container instanceof WizardDialog2 )
    {
      final WizardDialog2 wd2 = (WizardDialog2) container;
      m_resultStatus = wd2.executeUnblocked( true, false, processingOperation );
    }
    else
    {
      m_resultStatus = RunnableContextHelper.execute( container, true, true, processingOperation );
    }

    // if anything happened during the processing, restore the original results db from disk
    if( m_resultStatus.isOK() != true )
    {
      try
      {
        // set the dirty flag of the results model
        ((ICommandPoster) m_caseDataProvider).postCommand( IScenarioResultMeta.class.getName(), new EmptyCommand( "", false ) ); //$NON-NLS-1$
      }
      catch( InvocationTargetException e )
      {
        e.printStackTrace();
      }

      m_caseDataProvider.reloadModel();
    }

    // otherwise move the new results data to the results folder
    // this operation is not cancelable
    if( m_resultStatus.isOK() )
    {
      // processing finished without problems, prepare the data-operation
      final ResultManagerOperation dataOperation = new ResultManagerOperation( m_resultManager, m_unitFolder, m_simulationStatus, m_caseDataProvider, processingOperation.getOutputDir(), processingOperation.getCalcUnitMeta(), processingOperation.getOriginalStepsToDelete() );

      if( container instanceof WizardDialog2 )
      {
        final WizardDialog2 wd2 = (WizardDialog2) container;
        m_resultStatus = wd2.executeUnblocked( false, false, dataOperation );
      }
      else
      {
        m_resultStatus = RunnableContextHelper.execute( container, true, false, dataOperation );
      }
    }

    /* The user may have changed the page meanwhile, return now to this page */
    if( container.getShell() != null )
    {
      container.showPage( this );

      m_statusComp.setStatus( m_resultStatus );

      setMessage( Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.RMA10ResultPage.15" ) ); //$NON-NLS-1$

      m_isProcessing = false;
    }
  }

  public IStatus getResultStatus( )
  {
    return m_resultStatus;
  }

  public boolean isProcessing( )
  {
    return m_isProcessing;
  }

  public File getResultDir( )
  {
    return m_resultManager.getOutputDir();
  }

}
