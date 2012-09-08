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
package org.kalypso.ui.rrm.internal.cm.view;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.dialogs.TitleAreaDialog;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.CheckStateChangedEvent;
import org.eclipse.jface.viewers.CheckboxTableViewer;
import org.eclipse.jface.viewers.ICheckStateListener;
import org.eclipse.jface.viewers.ICheckStateProvider;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.TableViewerColumn;
import org.eclipse.jface.viewers.ViewerFilter;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.forms.widgets.ScrolledForm;
import org.kalypso.afgui.KalypsoAFGUIFrameworkPlugin;
import org.kalypso.commons.databinding.IDataBinding;
import org.kalypso.commons.databinding.dialog.DatabindingTitleAreaDialog;
import org.kalypso.commons.databinding.validation.ValidationStatusUtilities;
import org.kalypso.contribs.eclipse.jface.dialog.DialogSettingsUtils;
import org.kalypso.contribs.eclipse.jface.viewers.table.ColumnsResizeControlListener;
import org.kalypso.contribs.eclipse.swt.widgets.ColumnViewerSorter;
import org.kalypso.core.status.StatusComposite;
import org.kalypso.core.status.StatusDialog;
import org.kalypso.model.hydrology.binding.cm.ICatchmentModel;
import org.kalypso.model.hydrology.binding.cm.ILinearSumGenerator;
import org.kalypso.model.hydrology.binding.cm.IMultiGenerator;
import org.kalypso.model.rcm.binding.IRainfallGenerator;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ui.rrm.internal.IUiRrmWorkflowConstants;
import org.kalypso.ui.rrm.internal.KalypsoUIRRMPlugin;
import org.kalypso.ui.rrm.internal.cm.view.comparator.CommentComparator;
import org.kalypso.ui.rrm.internal.cm.view.comparator.DescriptionComparator;
import org.kalypso.ui.rrm.internal.cm.view.comparator.ValidFromComparator;
import org.kalypso.ui.rrm.internal.cm.view.comparator.ValidToComparator;
import org.kalypso.ui.rrm.internal.cm.view.filter.ParameterTypeViewerFilter;
import org.kalypso.ui.rrm.internal.cm.view.provider.CommentColumnLabelProvider;
import org.kalypso.ui.rrm.internal.cm.view.provider.DescriptionColumnLabelProvider;
import org.kalypso.ui.rrm.internal.cm.view.provider.ValidFromColumnLabelProvider;
import org.kalypso.ui.rrm.internal.cm.view.provider.ValidToColumnLabelProvider;
import org.kalypso.ui.rrm.internal.i18n.Messages;
import org.kalypso.ui.rrm.internal.utils.featureTree.ITreeNodeModel;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;

import de.renew.workflow.connector.cases.IScenarioDataProvider;

/**
 * This dialog allows the editing of the properties of a multi catchment model.
 *
 * @author Holger Albert
 */
public class EditMultiDialog extends TitleAreaDialog
{
  private final PropertyChangeListener m_propertyListener = new PropertyChangeListener()
  {
    @Override
    public void propertyChange( final PropertyChangeEvent evt )
    {
      if( evt.getPropertyName().equals( IMultiGenerator.PROPERTY_PARAMETER_TYPE.toString() ) )
        handleParameterTypeChanged( evt );
      else
        handlePropertyChanged();
    }
  };

  /**
   * The model.
   */
  private final ITreeNodeModel m_model;

  /**
   * The bean to edit.
   */
  protected final MultiBean m_bean;

  /**
   * The main group.
   */
  private Group m_mainGroup;

  /**
   * The details group.
   */
  private Group m_detailsGroup;

  /**
   * The generator viewer.
   */
  protected CheckboxTableViewer m_generatorViewer;

  /**
   * The status composite.
   */
  protected StatusComposite m_statusComposite;

  /**
   * The data binding.
   */
  private IDataBinding m_dataBinding;

  /**
   * The dialog settings.
   */
  private final IDialogSettings m_settings;

  /**
   * The ignore next change flag.
   */
  private boolean m_ignoreNextChange;

  /**
   * The constructor.
   *
   * @param parentShell
   *          The parent shell, or null to create a top-level shell.
   * @param model
   *          The model.
   * @param bean
   *          The bean to edit.
   */
  public EditMultiDialog( final Shell shell, final ITreeNodeModel model, final MultiBean bean )
  {
    super( shell );

    m_model = model;
    m_bean = bean;

    m_mainGroup = null;
    m_detailsGroup = null;
    m_generatorViewer = null;
    m_statusComposite = null;
    m_dataBinding = null;
    m_settings = DialogSettingsUtils.getDialogSettings( KalypsoUIRRMPlugin.getDefault(), getClass().getName() );
    m_ignoreNextChange = false;

    m_bean.addPropertyChangeListener( m_propertyListener );
  }

  /**
   * @see org.eclipse.jface.dialogs.TitleAreaDialog#createDialogArea(org.eclipse.swt.widgets.Composite)
   */
  @Override
  protected Control createDialogArea( final Composite parent )
  {
    /* Set the title. */
    getShell().setText( Messages.getString( "EditMultiDialog_0" ) ); //$NON-NLS-1$
    setTitle( Messages.getString( "EditMultiDialog_1" ) ); //$NON-NLS-1$

    /* Create the control. */
    final Composite control = (Composite) super.createDialogArea( parent );

    /* Create the main composite. */
    final Composite main = new Composite( control, SWT.NONE );
    main.setLayout( new GridLayout( 2, false ) );
    final GridData mainData = new GridData( SWT.FILL, SWT.FILL, true, true );
    mainData.heightHint = 550;
    mainData.widthHint = 900;
    main.setLayoutData( mainData );

    /* Create the main sash form. */
    final SashForm mainSashForm = new SashForm( main, SWT.NONE );
    mainSashForm.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );

    /* Create the main group. */
    m_mainGroup = new Group( mainSashForm, SWT.NONE );
    m_mainGroup.setLayout( new GridLayout( 1, false ) );
    final GridData mainGroupData = new GridData( SWT.FILL, SWT.FILL, true, true );
    mainGroupData.widthHint = 250;
    m_mainGroup.setLayoutData( mainGroupData );
    m_mainGroup.setText( Messages.getString( "EditMultiDialog_2" ) ); //$NON-NLS-1$

    /* Create the content of the main group. */
    createMainContent( m_mainGroup );

    /* Create the details group. */
    m_detailsGroup = new Group( mainSashForm, SWT.NONE );
    m_detailsGroup.setLayout( new GridLayout( 1, false ) );
    m_detailsGroup.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );
    m_detailsGroup.setText( Messages.getString( "EditMultiDialog_3" ) ); //$NON-NLS-1$

    /* Create the content of the details group. */
    createDetailsContent( m_detailsGroup );

    /* Set the weights. */
    mainSashForm.setWeights( new int[] { 35, 65 } );

    return control;
  }

  /**
   * @see org.eclipse.jface.dialogs.Dialog#createButtonsForButtonBar(org.eclipse.swt.widgets.Composite)
   */
  @Override
  protected void createButtonsForButtonBar( final Composite parent )
  {
    super.createButtonsForButtonBar( parent );

    /* Update the status. */
    updateStatus();
  }

  /**
   * @see org.eclipse.jface.dialogs.Dialog#getDialogBoundsSettings()
   */
  @Override
  protected IDialogSettings getDialogBoundsSettings( )
  {
    return DialogSettingsUtils.getSection( m_settings, "bounds" ); //$NON-NLS-1$
  }

  /**
   * @see org.eclipse.jface.dialogs.Dialog#isResizable()
   */
  @Override
  protected boolean isResizable( )
  {
    return true;
  }

  /**
   * @see org.eclipse.jface.dialogs.Dialog#okPressed()
   */
  @Override
  protected void okPressed( )
  {
    /* Check the status of the data binding. */
    final IStatus bindingStatus = ValidationStatusUtilities.getFirstNonOkStatus( m_dataBinding );
    if( !bindingStatus.isOK() )
    {
      final IStatus status = new Status( IStatus.ERROR, KalypsoUIRRMPlugin.getID(), String.format( Messages.getString( "EditMultiDialog_4" ), bindingStatus.getMessage() ), bindingStatus.getException() ); //$NON-NLS-1$
      StatusDialog.open( getShell(), status, getShell().getText() );
      return;
    }

    /* Perform ok. */
    final IStatus performStatus = performOk();
    if( !performStatus.isOK() )
    {
      StatusDialog.open( getShell(), performStatus, getShell().getText() );
      return;
    }

    /* Dispose the dialog. */
    dispose();

    super.okPressed();
  }

  @Override
  protected void cancelPressed( )
  {
    /* Dispose the dialog. */
    dispose();

    super.cancelPressed();
  }

  /**
   * This function creates the content of the main group.
   *
   * @param parent
   *          The parent composite.
   */
  private void createMainContent( final Composite parent )
  {
    /* Create the form. */
    final ScrolledForm form = new ScrolledForm( parent, SWT.H_SCROLL | SWT.V_SCROLL );
    form.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );
    form.setExpandHorizontal( true );
    form.setExpandVertical( true );

    /* Get the body. */
    final Composite body = form.getBody();
    final GridLayout bodyLayout = new GridLayout( 1, false );
    bodyLayout.marginHeight = 0;
    bodyLayout.marginWidth = 0;
    body.setLayout( bodyLayout );

    /* Create the data binding. */
    m_dataBinding = new DatabindingTitleAreaDialog( this, null );

    /* Create the multi new composite. */
    final MultiNewComposite multiComposite = new MultiNewComposite( body, m_bean, m_dataBinding );
    multiComposite.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );

    /* Do a reflow and a layout. */
    form.reflow( true );
    form.layout( true, true );
  }

  /**
   * This function creates the content of the details group.
   *
   * @param parent
   *          The parent composite.
   */
  private void createDetailsContent( final Composite parent )
  {
    /* Create the generator viewer. */
    m_generatorViewer = CheckboxTableViewer.newCheckList( parent, SWT.BORDER | SWT.H_SCROLL | SWT.V_SCROLL | SWT.FULL_SELECTION | SWT.SINGLE );
    m_generatorViewer.getTable().setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );
    m_generatorViewer.getTable().setLinesVisible( true );
    m_generatorViewer.getTable().setHeaderVisible( true );
    m_generatorViewer.getTable().addControlListener( new ColumnsResizeControlListener() );
    m_generatorViewer.setContentProvider( new ArrayContentProvider() );
    m_generatorViewer.setFilters( new ViewerFilter[] { new ParameterTypeViewerFilter( (String) m_bean.getProperty( ILinearSumGenerator.PROPERTY_PARAMETER_TYPE ) ) } );

    /* Create the columns. */
    createGeneratorViewerColumns( m_generatorViewer );

    /* Set the input. */
    final ILinearSumGenerator[] generators = getGenerators();
    m_generatorViewer.setInput( generators );

    /* Add a checkstate provider. */
    m_generatorViewer.setCheckStateProvider( new ICheckStateProvider()
    {
      @Override
      public boolean isGrayed( final Object element )
      {
        return false;
      }

      @Override
      public boolean isChecked( final Object element )
      {
        final ILinearSumGenerator[] subGenerators = m_bean.getSubGenerators();
        if( subGenerators == null || subGenerators.length == 0 )
          return false;

        final List<ILinearSumGenerator> subGeneratorsList = Arrays.asList( subGenerators );
        if( subGeneratorsList.contains( element ) )
          return true;

        return false;
      }
    } );

    /* Add a listener. */
    m_generatorViewer.addCheckStateListener( new ICheckStateListener()
    {
      @Override
      public void checkStateChanged( final CheckStateChangedEvent event )
      {
        final Object[] checkedElements = m_generatorViewer.getCheckedElements();

        final List<ILinearSumGenerator> subGenerators = new ArrayList<>();
        for( final Object checkedElement : checkedElements )
          subGenerators.add( (ILinearSumGenerator) checkedElement );

        m_bean.setSubGenerators( subGenerators.toArray( new ILinearSumGenerator[] {} ) );
        m_generatorViewer.refresh();

        updateStatus();
      }
    } );

    /* Create the status composite. */
    m_statusComposite = new StatusComposite( parent, StatusComposite.DETAILS );
    m_statusComposite.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, false ) );
  }

  private void createGeneratorViewerColumns( final TableViewer viewer )
  {
    /* Create the description column. */
    final TableViewerColumn descriptionColumn = new TableViewerColumn( viewer, SWT.LEFT );
    descriptionColumn.getColumn().setText( Messages.getString( "EditMultiDialog_5" ) ); //$NON-NLS-1$
    descriptionColumn.getColumn().setWidth( 150 );
    descriptionColumn.setLabelProvider( new DescriptionColumnLabelProvider() );
    ColumnsResizeControlListener.setMinimumPackWidth( descriptionColumn.getColumn() );
    ColumnViewerSorter.registerSorter( descriptionColumn, new DescriptionComparator() );

    /* Create the comment column. */
    final TableViewerColumn commentColumn = new TableViewerColumn( viewer, SWT.LEFT );
    commentColumn.getColumn().setText( Messages.getString( "EditMultiDialog_6" ) ); //$NON-NLS-1$
    commentColumn.getColumn().setWidth( 150 );
    commentColumn.setLabelProvider( new CommentColumnLabelProvider() );
    ColumnsResizeControlListener.setMinimumPackWidth( commentColumn.getColumn() );
    ColumnViewerSorter.registerSorter( commentColumn, new CommentComparator() );

    /* Create the valid from column. */
    final TableViewerColumn validFromColumn = new TableViewerColumn( viewer, SWT.LEFT );
    validFromColumn.getColumn().setText( Messages.getString( "EditMultiDialog_7" ) ); //$NON-NLS-1$
    validFromColumn.getColumn().setWidth( 75 );
    validFromColumn.setLabelProvider( new ValidFromColumnLabelProvider() );
    ColumnsResizeControlListener.setMinimumPackWidth( validFromColumn.getColumn() );
    ColumnViewerSorter.registerSorter( validFromColumn, new ValidFromComparator() );

    /* Create the valid to column. */
    final TableViewerColumn validToColumn = new TableViewerColumn( viewer, SWT.LEFT );
    validToColumn.getColumn().setText( Messages.getString( "EditMultiDialog_8" ) ); //$NON-NLS-1$
    validToColumn.getColumn().setWidth( 75 );
    validToColumn.setLabelProvider( new ValidToColumnLabelProvider() );
    ColumnsResizeControlListener.setMinimumPackWidth( validToColumn.getColumn() );
    ColumnViewerSorter.registerSorter( validToColumn, new ValidToComparator() );

    /* Define a initial order. */
    ColumnViewerSorter.setSortState( descriptionColumn, Boolean.FALSE );
  }

  /**
   * This function returns all linear sum generators that can be linked in a multi generator.
   *
   * @return All linear sum generators that can be linked in a multi generator.
   */
  private ILinearSumGenerator[] getGenerators( )
  {
    final List<ILinearSumGenerator> results = new ArrayList<>();

    try
    {
      final IScenarioDataProvider dataProvider = KalypsoAFGUIFrameworkPlugin.getDataProvider();
      final CommandableWorkspace generatorsWorkspace = dataProvider.getCommandableWorkSpace( IUiRrmWorkflowConstants.SCENARIO_DATA_CATCHMENT_MODELS );

      final ICatchmentModel rootFeature = (ICatchmentModel) generatorsWorkspace.getRootFeature();
      final IFeatureBindingCollection<IRainfallGenerator> generators = rootFeature.getGenerators();
      for( final IRainfallGenerator generator : generators )
      {
        if( generator instanceof ILinearSumGenerator )
          results.add( (ILinearSumGenerator) generator );
      }
    }
    catch( final CoreException e )
    {
      // should never happen
      e.printStackTrace();
    }

    return results.toArray( new ILinearSumGenerator[] {} );
  }

  /**
   * This function saves the changes.
   *
   * @return A ERROR status on error or an OK status.
   */
  private IStatus performOk( )
  {
    try
    {
      final IScenarioDataProvider dataProvider = KalypsoAFGUIFrameworkPlugin.getDataProvider();
      final CommandableWorkspace generatorsWorkspace = dataProvider.getCommandableWorkSpace( IUiRrmWorkflowConstants.SCENARIO_DATA_CATCHMENT_MODELS );

      /* Apply the changes. */
      final Feature generator = m_bean.apply( generatorsWorkspace );

      /* Refresh the tree. */
      m_model.refreshTree( generator );

      return Status.OK_STATUS;
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      return new Status( IStatus.ERROR, KalypsoUIRRMPlugin.getID(), "Failed to save the model", e ); //$NON-NLS-1$
    }
  }

  /**
   * This function disposes the dialog.
   */
  private void dispose( )
  {
    m_mainGroup = null;
    m_detailsGroup = null;
    m_generatorViewer = null;
    m_statusComposite = null;
    m_dataBinding = null;
    /* HINT: Do not discard the dialog settings, will be used to save the dialog bounds . */
    m_ignoreNextChange = false;
  }

  /**
   * This function updates the status.
   */
  protected void updateStatus( )
  {
    if( m_statusComposite == null || m_statusComposite.isDisposed() )
      return;

    m_bean.updateStatus();
    m_statusComposite.setStatus( m_bean.getStatus() );
  }

  /**
   * This function handles the property changed event for the parameter type.
   *
   * @param evt
   *          The property change event.
   */
  protected void handleParameterTypeChanged( final PropertyChangeEvent evt )
  {
    /* Avoid loop, if we cancel the change. */
    if( m_ignoreNextChange == true )
    {
      m_ignoreNextChange = false;
      return;
    }

    /* Get the shell. */
    final Shell shell = getShell();

    /* Show the confirm dialog. */
    if( !MessageDialog.openConfirm( shell, shell.getText(), Messages.getString( "EditMultiDialog_9" ) ) ) //$NON-NLS-1$
    {
      m_ignoreNextChange = true;
      final MultiBean generator = m_bean;
      final Runnable revertOperation = new Runnable()
      {
        @Override
        public void run( )
        {
          generator.setProperty( IMultiGenerator.PROPERTY_PARAMETER_TYPE, evt.getOldValue() );
        }
      };
      shell.getDisplay().asyncExec( revertOperation );

      return;
    }

    /* Reset the set generators. */
    m_bean.setSubGenerators( new ILinearSumGenerator[] {} );
    updateStatus();

    /* Set the viewer filter of the generator viewer. */
    final String parameterType = (String) evt.getNewValue();
    if( m_generatorViewer != null && !m_generatorViewer.getTable().isDisposed() )
      m_generatorViewer.setFilters( new ViewerFilter[] { new ParameterTypeViewerFilter( parameterType ) } );
  }

  protected void handlePropertyChanged( )
  {
    updateStatus();
  }
}