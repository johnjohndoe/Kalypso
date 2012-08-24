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
package org.kalypso.model.wspm.pdb.ui.internal.tin;

import org.eclipse.core.databinding.beans.BeansObservables;
import org.eclipse.core.databinding.observable.value.IObservableValue;
import org.eclipse.jface.databinding.viewers.ViewersObservables;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.viewers.ViewerColumn;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.forms.widgets.ScrolledForm;
import org.kalypso.commons.databinding.jface.wizard.DatabindingWizardPage;
import org.kalypso.contribs.eclipse.jface.viewers.ColumnViewerUtil;
import org.kalypso.contribs.eclipse.jface.viewers.ViewerColumnItem;
import org.kalypso.contribs.eclipse.jface.viewers.table.ColumnsResizeControlListener;
import org.kalypso.contribs.eclipse.swt.widgets.ColumnViewerSorter;

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
  private final PdbImportConnectionChooserData m_settingsData;

  /**
   * The tree viewer displays the dhm indexes.
   */
  private TreeViewer m_searchViewer;

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

    setTitle( "Datei auswählen" );
    setDescription( "Auswahl der Datei der zu importierenden Höhendaten." );
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

    /* Create the filter textbox. */
    final Text filterText = new Text( main, SWT.BORDER );
    filterText.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, false ) );

    /* Create the filter button. */
    final Button filterButton = new Button( main, SWT.CHECK );
    filterButton.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, false ) );
    filterButton.setText( "Nach Kartenausschnitt filtern" );

    /* Create an empty label. */
    final Label label = new Label( main, SWT.NONE );
    label.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, false ) );

    /* Create a tree viewer. */
    m_searchViewer = new TreeViewer( main, SWT.BORDER | SWT.SINGLE | SWT.FULL_SELECTION | SWT.H_SCROLL | SWT.V_SCROLL );
    m_searchViewer.getTree().setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );
    m_searchViewer.getTree().setLinesVisible( true );
    m_searchViewer.getTree().setHeaderVisible( true );
    configureTreeViewer( m_searchViewer );
    m_searchViewer.setContentProvider( new SearchDhmIndexContentProvider() );
    m_searchViewer.setInput( m_settingsData );

    /* Add the column resize control listener. */
    m_searchViewer.getTree().addControlListener( new ColumnsResizeControlListener() );

    /* Do the data binding. */
    final IObservableValue targetInput = ViewersObservables.observeInput( m_searchViewer );
    final IObservableValue modelInput = BeansObservables.observeValue( m_settingsData, PdbImportConnectionChooserData.PROPERTY_DHM_INDEXES );
    dataBinding.bindValue( targetInput, modelInput );

    /* Create a scrolled form. */
    final ScrolledForm scrolledForm = new ScrolledForm( main );
    scrolledForm.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, false ) );
    scrolledForm.setExpandHorizontal( true );
    scrolledForm.setExpandVertical( true );

    /* Get the body. */
    final Composite body = scrolledForm.getBody();
    body.setLayout( new GridLayout( 1, false ) );

    /* Add the dhm index composite. */
    final DhmIndexComposite dhmIndexComposite = new DhmIndexComposite( body, SWT.NONE );
    dhmIndexComposite.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );

    /* Reflow. */
    scrolledForm.reflow( true );

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
    final ViewerColumn nameColumn = ColumnViewerUtil.createViewerColumn( viewer, SWT.LEFT );
    nameColumn.setLabelProvider( new DhmIndexNameLabelProvider() );

    final ViewerColumnItem nameItem = new ViewerColumnItem( nameColumn );
    nameItem.setText( "Name" );
    nameItem.setResizable( false );

    ColumnsResizeControlListener.setMinimumPackWidth( nameItem.getColumn() );
    ColumnViewerSorter.registerSorter( nameColumn, new DhmIndexNameViewerComparator() );

    /* Filename. */
    final ViewerColumn filenameColumn = ColumnViewerUtil.createViewerColumn( viewer, SWT.LEFT );
    filenameColumn.setLabelProvider( new DhmIndexFilenameLabelProvider() );

    final ViewerColumnItem filenameItem = new ViewerColumnItem( filenameColumn );
    filenameItem.setText( "Dateiname" );
    filenameItem.setResizable( false );

    ColumnsResizeControlListener.setMinimumPackWidth( filenameItem.getColumn() );
    ColumnViewerSorter.registerSorter( filenameColumn, new DhmIndexFilenameViewerComparator() );

    /* Description. */
    final ViewerColumn descriptionColumn = ColumnViewerUtil.createViewerColumn( viewer, SWT.LEFT );
    descriptionColumn.setLabelProvider( new DhmIndexDescriptionLabelProvider() );

    final ViewerColumnItem descriptionItem = new ViewerColumnItem( descriptionColumn );
    descriptionItem.setText( "Beschreibung" );
    descriptionItem.setResizable( false );

    ColumnsResizeControlListener.setMinimumPackWidth( descriptionItem.getColumn() );
    ColumnViewerSorter.registerSorter( descriptionColumn, new DhmIndexDescriptionViewerComparator() );
  }
}