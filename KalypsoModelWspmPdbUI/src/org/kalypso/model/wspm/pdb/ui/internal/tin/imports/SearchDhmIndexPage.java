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
package org.kalypso.model.wspm.pdb.ui.internal.tin.imports;

import org.apache.commons.lang3.StringUtils;
import org.eclipse.core.databinding.beans.BeansObservables;
import org.eclipse.core.databinding.beans.PojoObservables;
import org.eclipse.core.databinding.observable.value.IObservableValue;
import org.eclipse.core.databinding.observable.value.IValueChangeListener;
import org.eclipse.core.databinding.observable.value.ValueChangeEvent;
import org.eclipse.jface.databinding.swt.ISWTObservableValue;
import org.eclipse.jface.databinding.swt.SWTObservables;
import org.eclipse.jface.databinding.viewers.ViewersObservables;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.viewers.ViewerColumn;
import org.eclipse.jface.viewers.ViewerFilter;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.forms.widgets.ScrolledForm;
import org.kalypso.commons.databinding.jface.wizard.DatabindingWizardPage;
import org.kalypso.contribs.eclipse.jface.viewers.ColumnViewerUtil;
import org.kalypso.contribs.eclipse.jface.viewers.ViewerColumnItem;
import org.kalypso.contribs.eclipse.jface.viewers.table.ColumnsResizeControlListener;
import org.kalypso.contribs.eclipse.swt.widgets.ColumnViewerSorter;
import org.kalypso.model.wspm.pdb.db.mapping.DhmIndex;
import org.kalypso.model.wspm.pdb.ui.internal.i18n.Messages;
import org.kalypso.model.wspm.pdb.ui.internal.tin.DhmIndexComposite;

/**
 * This page lists all {@link org.kalypso.model.wspm.pdb.db.mapping.DhmIndex} and enables the user to select one.
 * 
 * @author Holger Albert
 */
public class SearchDhmIndexPage extends WizardPage
{
  /**
   * The settings.
   */
  protected final PdbImportConnectionChooserData m_settingsData;

  /**
   * The tree viewer displays the dhm indexes.
   */
  private TreeViewer m_searchViewer;

  /**
   * The dhm index composite.
   */
  protected DhmIndexComposite m_dhmIndexComposite;

  /**
   * The constructor.
   * 
   * @param pageName
   *          The name of the page.
   * @param settingsData
   *          The settings.
   */
  public SearchDhmIndexPage( final String pageName, final PdbImportConnectionChooserData settingsData )
  {
    this( pageName, null, null, settingsData );
  }

  /**
   * The constructor.
   * 
   * @param pageName
   *          The name of the page.
   * @param title
   *          The title for this wizard page, or null if none.
   * @param titleImage
   *          The image descriptor for the title of this wizard page, or null if none.
   * @param settingsData
   *          The settings.
   */
  public SearchDhmIndexPage( final String pageName, final String title, final ImageDescriptor titleImage, final PdbImportConnectionChooserData settingsData )
  {
    super( pageName, title, titleImage );

    m_settingsData = settingsData;
    m_searchViewer = null;
    m_dhmIndexComposite = null;

    setTitle( Messages.getString("SearchDhmIndexPage_0") ); //$NON-NLS-1$
    setDescription( Messages.getString("SearchDhmIndexPage_1") ); //$NON-NLS-1$
  }

  @Override
  public void createControl( final Composite parent )
  {
    /* Create the databinding. */
    final DatabindingWizardPage dataBinding = new DatabindingWizardPage( this, null );

    /* Create the main composite. */
    final Composite main = new Composite( parent, SWT.NONE );
    final GridLayout mainLayout = new GridLayout( 1, false );
    mainLayout.marginHeight = 0;
    mainLayout.marginWidth = 0;
    main.setLayout( mainLayout );

    /* Create the filter group. */
    final Group filterGroup = new Group( main, SWT.NONE );
    filterGroup.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, false ) );
    filterGroup.setLayout( new GridLayout( 1, false ) );
    filterGroup.setText( Messages.getString("SearchDhmIndexPage_2") ); //$NON-NLS-1$

    /* Create the filter textbox. */
    final Text filterText = new Text( filterGroup, SWT.BORDER | SWT.SEARCH );
    filterText.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, false ) );

    /* Create the filter button. */
    final Button filterButton = new Button( filterGroup, SWT.CHECK );
    filterButton.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, false ) );
    filterButton.setText( Messages.getString("SearchDhmIndexPage_3") ); //$NON-NLS-1$

    /* Create a tree viewer. */
    m_searchViewer = new TreeViewer( main, SWT.BORDER | SWT.SINGLE | SWT.FULL_SELECTION | SWT.H_SCROLL | SWT.V_SCROLL );
    final GridData treeData = new GridData( SWT.FILL, SWT.FILL, true, true );
    treeData.heightHint = 200;
    m_searchViewer.getTree().setLayoutData( treeData );
    m_searchViewer.getTree().setLinesVisible( true );
    m_searchViewer.getTree().setHeaderVisible( true );
    configureTreeViewer( m_searchViewer );
    m_searchViewer.setContentProvider( new SearchDhmIndexContentProvider() );

    /* Add the column resize control listener. */
    m_searchViewer.getTree().addControlListener( new ColumnsResizeControlListener() );

    /* Create a scrolled form. */
    final ScrolledForm scrolledForm = new ScrolledForm( main );
    scrolledForm.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, false ) );
    scrolledForm.setExpandHorizontal( true );
    scrolledForm.setExpandVertical( true );

    /* Get the body. */
    final Composite body = scrolledForm.getBody();
    final GridLayout bodyLayout = new GridLayout( 1, false );
    bodyLayout.marginHeight = 0;
    bodyLayout.marginWidth = 0;
    body.setLayout( bodyLayout );

    /* Add the dhm index composite. */
    m_dhmIndexComposite = new DhmIndexComposite( body, SWT.NONE, m_settingsData.getDhmIndex(), false, dataBinding );
    m_dhmIndexComposite.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );

    /* Reflow. */
    scrolledForm.reflow( true );

    /* Create the filter. */
    final String dbCoordinateSystem = m_settingsData.getDbCoordinateSystem();
    final DhmIndexFilter filter = new DhmIndexFilter( m_searchViewer, dbCoordinateSystem, StringUtils.EMPTY, false );
    m_searchViewer.setFilters( new ViewerFilter[] { filter } );

    /* Do the data binding. */
    final ISWTObservableValue textTarget = SWTObservables.observeText( filterText, new int[] { SWT.Modify } );
    final IObservableValue textModel = PojoObservables.observeValue( filter, DhmIndexFilter.PROPERTY_FILTER_TEXT );
    dataBinding.bindValue( textTarget, textModel );

    /* Do the data binding. */
    final ISWTObservableValue buttonTarget = SWTObservables.observeSelection( filterButton );
    final IObservableValue buttonModel = PojoObservables.observeValue( filter, DhmIndexFilter.PROPERTY_FILTER_QUERY );
    dataBinding.bindValue( buttonTarget, buttonModel );

    /* Do the data binding. */
    final IObservableValue targetViewer = ViewersObservables.observeInput( m_searchViewer );
    final IObservableValue modelViewer = BeansObservables.observeValue( m_settingsData, PdbImportConnectionChooserData.PROPERTY_DHM_INDEXES );
    dataBinding.bindValue( targetViewer, modelViewer );

    /* Do the data binding. */
    final IObservableValue targetIndex = ViewersObservables.observeSinglePostSelection( m_searchViewer );
    final IObservableValue modelIndex = BeansObservables.observeValue( m_settingsData, PdbImportConnectionChooserData.PROPERTY_DHM_INDEX );
    dataBinding.bindValue( targetIndex, modelIndex );

    /* Add a listener. */
    modelIndex.addValueChangeListener( new IValueChangeListener()
    {
      @Override
      public void handleValueChange( final ValueChangeEvent event )
      {
        /* Parent is disposed. */
        if( body.isDisposed() )
          return;

        /* Get the dhm index from the selection. */
        final DhmIndex dhmIndex = (DhmIndex)event.getObservableValue().getValue();

        /* The dhm index composite needs to get the new object set. */
        if( m_dhmIndexComposite != null && !m_dhmIndexComposite.isDisposed() )
          m_dhmIndexComposite.dispose();

        /* Add the dhm index composite. */
        m_dhmIndexComposite = new DhmIndexComposite( body, SWT.NONE, dhmIndex, false, dataBinding );
        m_dhmIndexComposite.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );

        /* Reflow. */
        scrolledForm.reflow( true );
      }
    } );

    /* Set the control to the page. */
    setControl( main );
  }

  /**
   * This function creates the columns in the tree viewer.
   * 
   * @param viewer
   *          The tree viewer.
   */
  private void configureTreeViewer( final TreeViewer viewer )
  {
    /* Name. */
    // final ViewerColumn nameColumn = ColumnViewerUtil.createViewerColumn( viewer, SWT.LEFT );
    // nameColumn.setLabelProvider( new DhmIndexNameLabelProvider() );
    //
    // final ViewerColumnItem nameItem = new ViewerColumnItem( nameColumn );
    // nameItem.setText( "Name" );
    // nameItem.setResizable( false );
    //
    // ColumnsResizeControlListener.setMinimumPackWidth( nameItem.getColumn() );
    // ColumnViewerSorter.registerSorter( nameColumn, new DhmIndexNameViewerComparator() );

    /* Filename. */
    final ViewerColumn filenameColumn = ColumnViewerUtil.createViewerColumn( viewer, SWT.LEFT );
    filenameColumn.setLabelProvider( new DhmIndexFilenameLabelProvider() );

    final ViewerColumnItem filenameItem = new ViewerColumnItem( filenameColumn );
    filenameItem.setText( Messages.getString("SearchDhmIndexPage_4") ); //$NON-NLS-1$
    filenameItem.setResizable( false );

    ColumnsResizeControlListener.setMinimumPackWidth( filenameItem.getColumn() );
    ColumnViewerSorter.registerSorter( filenameColumn, new DhmIndexFilenameViewerComparator() );

    /* Description. */
    final ViewerColumn descriptionColumn = ColumnViewerUtil.createViewerColumn( viewer, SWT.LEFT );
    descriptionColumn.setLabelProvider( new DhmIndexDescriptionLabelProvider() );

    final ViewerColumnItem descriptionItem = new ViewerColumnItem( descriptionColumn );
    descriptionItem.setText( Messages.getString("SearchDhmIndexPage_5") ); //$NON-NLS-1$
    descriptionItem.setResizable( false );

    ColumnsResizeControlListener.setMinimumPackWidth( descriptionItem.getColumn() );
    ColumnViewerSorter.registerSorter( descriptionColumn, new DhmIndexDescriptionViewerComparator() );

    /* Measurement Date. */
    final ViewerColumn measurementDateColumn = ColumnViewerUtil.createViewerColumn( viewer, SWT.LEFT );
    measurementDateColumn.setLabelProvider( new DhmIndexMeasurementDateLabelProvider() );

    final ViewerColumnItem measurementDateItem = new ViewerColumnItem( measurementDateColumn );
    measurementDateItem.setText( Messages.getString("SearchDhmIndexPage_6") ); //$NON-NLS-1$
    measurementDateItem.setResizable( false );

    ColumnsResizeControlListener.setMinimumPackWidth( measurementDateItem.getColumn() );
    ColumnViewerSorter.registerSorter( measurementDateColumn, new DhmIndexMeasurementDateViewerComparator() );

    /* Measurename Accuracy. */
    final ViewerColumn measurementAccuracyColumn = ColumnViewerUtil.createViewerColumn( viewer, SWT.LEFT );
    measurementAccuracyColumn.setLabelProvider( new DhmIndexMeasurementAccuracyLabelProvider() );

    final ViewerColumnItem measurementAccuracyItem = new ViewerColumnItem( measurementAccuracyColumn );
    measurementAccuracyItem.setText( Messages.getString("SearchDhmIndexPage_7") ); //$NON-NLS-1$
    measurementAccuracyItem.setResizable( false );

    ColumnsResizeControlListener.setMinimumPackWidth( measurementAccuracyItem.getColumn() );
    ColumnViewerSorter.registerSorter( measurementAccuracyColumn, new DhmIndexMeasurementAccuracyViewerComparator() );
  }
}