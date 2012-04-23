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

import org.eclipse.core.databinding.Binding;
import org.eclipse.core.databinding.DataBindingContext;
import org.eclipse.core.databinding.observable.ChangeEvent;
import org.eclipse.core.databinding.observable.IChangeListener;
import org.eclipse.core.databinding.observable.IObservable;
import org.eclipse.core.databinding.observable.list.IObservableList;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.dialogs.IMessageProvider;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.dialogs.TrayDialog;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.CheckStateChangedEvent;
import org.eclipse.jface.viewers.CheckboxTableViewer;
import org.eclipse.jface.viewers.ICheckStateListener;
import org.eclipse.jface.viewers.ICheckStateProvider;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.TableViewerColumn;
import org.eclipse.jface.viewers.ViewerFilter;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.forms.widgets.Form;
import org.eclipse.ui.forms.widgets.ScrolledForm;
import org.kalypso.commons.databinding.IDataBinding;
import org.kalypso.commons.databinding.forms.DatabindingForm;
import org.kalypso.contribs.eclipse.jface.dialog.DialogSettingsUtils;
import org.kalypso.contribs.eclipse.jface.viewers.table.ColumnsResizeControlListener;
import org.kalypso.contribs.eclipse.swt.widgets.ColumnViewerSorter;
import org.kalypso.core.status.StatusComposite;
import org.kalypso.core.status.StatusDialog;
import org.kalypso.model.hydrology.cm.binding.ICatchmentModel;
import org.kalypso.model.rcm.binding.ILinearSumGenerator;
import org.kalypso.model.rcm.binding.IMultiGenerator;
import org.kalypso.model.rcm.binding.IRainfallGenerator;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
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
import org.kalypso.ui.rrm.internal.utils.featureTree.ITreeNodeModel;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;

/**
 * This dialog allows the editing of the properties of a multi catchment model.
 * 
 * @author Holger Albert
 */
public class EditMultiDialog extends TrayDialog
{
  /**
   * A property change listener for the parameter type.
   */
  private final PropertyChangeListener m_changeListener = new PropertyChangeListener()
  {
    @Override
    public void propertyChange( final PropertyChangeEvent evt )
    {
      handleParameterTypeChanged( evt );
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
   * The form.
   */
  private Form m_form;

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
  private IDialogSettings m_settings;

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

    m_form = null;
    m_mainGroup = null;
    m_detailsGroup = null;
    m_generatorViewer = null;
    m_statusComposite = null;
    m_dataBinding = null;
    m_settings = DialogSettingsUtils.getDialogSettings( KalypsoUIRRMPlugin.getDefault(), getClass().getName() );
    m_ignoreNextChange = false;

    m_bean.addPropertyChangeListener( ILinearSumGenerator.PROPERTY_PARAMETER_TYPE.toString(), m_changeListener );
  }

  /**
   * @see org.eclipse.jface.dialogs.Dialog#createDialogArea(org.eclipse.swt.widgets.Composite)
   */
  @Override
  protected Control createDialogArea( final Composite parent )
  {
    /* Set the title. */
    getShell().setText( "Edit Multi Catchment Model" );

    /* Create the main composite. */
    final Composite main = (Composite) super.createDialogArea( parent );
    main.setLayout( new GridLayout( 1, false ) );
    final GridData mainData = new GridData( SWT.FILL, SWT.FILL, true, true );
    mainData.heightHint = 550;
    mainData.widthHint = 900;
    main.setLayoutData( mainData );

    /* Create the form. */
    m_form = new Form( main, SWT.NONE );
    m_form.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );

    /* Create the data binding. */
    m_dataBinding = new DatabindingForm( m_form, null );

    /* Get the body. */
    final Composite body = m_form.getBody();
    body.setLayout( new GridLayout( 2, false ) );

    /* Create the main group. */
    m_mainGroup = new Group( body, SWT.NONE );
    m_mainGroup.setLayout( new GridLayout( 1, false ) );
    final GridData mainGroupData = new GridData( SWT.FILL, SWT.FILL, true, true );
    mainGroupData.widthHint = 250;
    m_mainGroup.setLayoutData( mainGroupData );
    m_mainGroup.setText( "Multigenerator" );

    /* Create the content of the main group. */
    createMainContent( m_mainGroup );

    /* Create the details group. */
    m_detailsGroup = new Group( body, SWT.NONE );
    m_detailsGroup.setLayout( new GridLayout( 1, false ) );
    m_detailsGroup.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );
    m_detailsGroup.setText( "Generators" );

    /* Create the content of the details group. */
    createDetailsContent( m_detailsGroup );

    /* HINT: The change listeners must be added, after the bindings were done. */
    addChangeListeners( m_dataBinding );

    return main;
  }

  /**
   * @see org.eclipse.jface.dialogs.Dialog#createButtonsForButtonBar(org.eclipse.swt.widgets.Composite)
   */
  @Override
  protected void createButtonsForButtonBar( final Composite parent )
  {
    super.createButtonsForButtonBar( parent );

    /* Do a layout. */
    m_form.layout( true, true );

    /* Validate the dialog. */
    validateDialog();
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
    /* Perform ok. */
    performOk();

    /* Dispose the dialog. */
    dispose();

    super.okPressed();
  }

  /**
   * @see org.eclipse.jface.dialogs.Dialog#cancelPressed()
   */
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
    body.setLayout( new GridLayout( 1, false ) );

    /* Create the multi new composite. */
    final MultiNewComposite composite = new MultiNewComposite( body, m_bean, m_dataBinding );
    composite.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );

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
      /**
       * @see org.eclipse.jface.viewers.ICheckStateProvider#isGrayed(java.lang.Object)
       */
      @Override
      public boolean isGrayed( final Object element )
      {
        return false;
      }

      /**
       * @see org.eclipse.jface.viewers.ICheckStateProvider#isChecked(java.lang.Object)
       */
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
      /**
       * @see org.eclipse.jface.viewers.ICheckStateListener#checkStateChanged(org.eclipse.jface.viewers.CheckStateChangedEvent)
       */
      @Override
      public void checkStateChanged( final CheckStateChangedEvent event )
      {
        final Object[] checkedElements = m_generatorViewer.getCheckedElements();

        final List<ILinearSumGenerator> subGenerators = new ArrayList<ILinearSumGenerator>();
        for( final Object checkedElement : checkedElements )
          subGenerators.add( (ILinearSumGenerator) checkedElement );

        m_bean.setSubGenerators( subGenerators.toArray( new ILinearSumGenerator[] {} ) );
        m_generatorViewer.refresh();

        validateDialog();
      }
    } );

    /* Create the status composite. */
    m_statusComposite = new StatusComposite( parent, SWT.NONE );
    m_statusComposite.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, false ) );
  }

  private void createGeneratorViewerColumns( final TableViewer viewer )
  {
    /* Create the description column. */
    final TableViewerColumn descriptionColumn = new TableViewerColumn( viewer, SWT.LEFT );
    descriptionColumn.getColumn().setText( "Description" );
    descriptionColumn.getColumn().setWidth( 150 );
    descriptionColumn.setLabelProvider( new DescriptionColumnLabelProvider() );
    ColumnsResizeControlListener.setMinimumPackWidth( descriptionColumn.getColumn() );
    ColumnViewerSorter.registerSorter( descriptionColumn, new DescriptionComparator() );

    /* Create the comment column. */
    final TableViewerColumn commentColumn = new TableViewerColumn( viewer, SWT.LEFT );
    commentColumn.getColumn().setText( "Comment" );
    commentColumn.getColumn().setWidth( 150 );
    commentColumn.setLabelProvider( new CommentColumnLabelProvider() );
    ColumnsResizeControlListener.setMinimumPackWidth( commentColumn.getColumn() );
    ColumnViewerSorter.registerSorter( commentColumn, new CommentComparator() );

    /* Create the valid from column. */
    final TableViewerColumn validFromColumn = new TableViewerColumn( viewer, SWT.LEFT );
    validFromColumn.getColumn().setText( "Valid From" );
    validFromColumn.getColumn().setWidth( 75 );
    validFromColumn.setLabelProvider( new ValidFromColumnLabelProvider() );
    ColumnsResizeControlListener.setMinimumPackWidth( validFromColumn.getColumn() );
    ColumnViewerSorter.registerSorter( validFromColumn, new ValidFromComparator() );

    /* Create the valid to column. */
    final TableViewerColumn validToColumn = new TableViewerColumn( viewer, SWT.LEFT );
    validToColumn.getColumn().setText( "Valid To" );
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
    final List<ILinearSumGenerator> results = new ArrayList<ILinearSumGenerator>();

    final CommandableWorkspace workspace = m_model.getWorkspace();
    final ICatchmentModel rootFeature = (ICatchmentModel) workspace.getRootFeature();
    final IFeatureBindingCollection<IRainfallGenerator> generators = rootFeature.getGenerators();
    for( final IRainfallGenerator generator : generators )
    {
      if( generator instanceof ILinearSumGenerator )
        results.add( (ILinearSumGenerator) generator );
    }

    return results.toArray( new ILinearSumGenerator[] {} );
  }

  /**
   * This function adds change listeners to the databinding.
   * 
   * @param dataBinding
   *          The data binding.
   */
  private void addChangeListeners( final IDataBinding dataBinding )
  {
    final DataBindingContext bindingContext = dataBinding.getBindingContext();
    final IObservableList bindings = bindingContext.getBindings();
    for( final Object object : bindings )
    {
      final Binding binding = (Binding) object;
      final IObservable target = binding.getTarget();
      target.addChangeListener( new IChangeListener()
      {
        @Override
        public void handleChange( final ChangeEvent event )
        {
          // TODO Is notified before the change, not after...
          System.out.println( "AHHHHHHHHHHHHHHHHHHHHHH" );
          validateDialog();
        }
      } );
    }
  }

  /**
   * This function updates the status.
   */
  private void updateStatus( )
  {
    if( m_statusComposite == null || m_statusComposite.isDisposed() )
      return;

    m_bean.updateStatus();
    m_statusComposite.setStatus( m_bean.getStatus() );
  }

  /**
   * This function saves the changes.
   */
  private void performOk( )
  {
    try
    {
      /* Apply the changes. */
      final Feature generator = m_bean.apply( m_model.getWorkspace() );

      /* Refresh the tree. */
      m_model.refreshTree( generator );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      final IStatus status = new Status( IStatus.ERROR, KalypsoUIRRMPlugin.getID(), "Failed to save the model", e ); //$NON-NLS-1$
      StatusDialog.open( getShell(), status, getShell().getText() );
    }
  }

  /**
   * This function disposes the dialog.
   */
  private void dispose( )
  {
    m_form = null;
    m_mainGroup = null;
    m_detailsGroup = null;
    m_generatorViewer = null;
    m_statusComposite = null;
    m_dataBinding = null;
    m_settings = null;
    m_ignoreNextChange = false;
  }

  /**
   * This function validates the dialog.
   */
  protected void validateDialog( )
  {
    /* This is independent of the other validation. */
    updateStatus();

    /* Cannot do anything. */
    if( m_form == null || m_form.isDisposed() )
      return;

    /* Get the ok button. */
    final Button okButton = getButton( IDialogConstants.OK_ID );
    if( okButton == null )
      return;

    /* Set the ok button to enabled. */
    okButton.setEnabled( true );

    /* On error in the form, the dialog may not be completed. */
    final int messageType = m_form.getMessageType();
    if( messageType == IMessageProvider.ERROR )
    {
      okButton.setEnabled( false );
      return;
    }

    /* HINT: The validation was ok. */
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
    if( !MessageDialog.openConfirm( shell, shell.getText(), "Changing the parameter type needs to update the list of generators. Unsaved changes will be lost. Continue?" ) )
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

    /* Set the viewer filter of the generator viewer. */
    final String parameterType = (String) evt.getNewValue();
    if( m_generatorViewer != null && !m_generatorViewer.getTable().isDisposed() )
      m_generatorViewer.setFilters( new ViewerFilter[] { new ParameterTypeViewerFilter( parameterType ) } );
  }
}