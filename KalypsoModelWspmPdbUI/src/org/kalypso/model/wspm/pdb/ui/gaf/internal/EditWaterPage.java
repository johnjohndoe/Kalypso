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
package org.kalypso.model.wspm.pdb.ui.gaf.internal;

import java.util.Collections;
import java.util.List;

import org.eclipse.core.databinding.beans.BeanProperties;
import org.eclipse.core.databinding.beans.BeansObservables;
import org.eclipse.core.databinding.observable.list.WritableList;
import org.eclipse.core.databinding.observable.value.IObservableValue;
import org.eclipse.core.databinding.property.value.IValueProperty;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.databinding.viewers.IViewerObservableValue;
import org.eclipse.jface.databinding.viewers.ViewerSupport;
import org.eclipse.jface.databinding.viewers.ViewersObservables;
import org.eclipse.jface.layout.GridLayoutFactory;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.TableViewerColumn;
import org.eclipse.jface.viewers.ViewerFilter;
import org.eclipse.jface.viewers.ViewerSorter;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.Text;
import org.kalypso.commons.databinding.validation.NotNullValidator;
import org.kalypso.contribs.eclipse.jface.action.ActionHyperlink;
import org.kalypso.contribs.eclipse.jface.viewers.table.TableViewerUtilities;
import org.kalypso.contribs.eclipse.swt.widgets.ColumnViewerSorter;
import org.kalypso.model.wspm.pdb.connect.IPdbConnection;
import org.kalypso.model.wspm.pdb.connect.PdbConnectException;
import org.kalypso.model.wspm.pdb.db.mapping.WaterBodies;
import org.kalypso.ui.editor.styleeditor.binding.DataBinder;
import org.kalypso.ui.editor.styleeditor.binding.DatabindingWizardPage;

/**
 * @author Gernot Belger
 */
public class EditWaterPage extends WizardPage
{
  private static final String STR_NAME = "Name";

  private static final String STR_GEWÄSSERKENNZIFFER = "Gewässerkennziffer";

  private final IPdbConnection m_connection;

  private final ImportGafData m_data;

  private TableViewer m_waterBodiesViewer;

  private WritableList m_tableInput;

  EditWaterPage( final String pageName, final IPdbConnection connection, final ImportGafData data )
  {
    super( pageName );

    m_connection = connection;
    m_data = data;

    setTitle( "Choose Water Body" );
    setDescription( "Choose the water body into which the profiles will be imported" );
  }

  @Override
  public void createControl( final Composite parent )
  {
    final Composite panel = new Composite( parent, SWT.NONE );
    setControl( panel );
    GridLayoutFactory.swtDefaults().applyTo( panel );

    final DatabindingWizardPage binding = new DatabindingWizardPage( this, null );
    createSearchFields( panel ).setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    createWaterBodyTable( binding, panel ).setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );
    createActions( panel ).setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );

    // TODO Auto-generated method stub

    // - aus allen vorhandenen gewässern auswählen -> tabelle anzeigen?!
    // - Möglichkeit (popup), ein neues Gewässer zu erzeugen
    // -> direkt in ImportGafPage?
  }

  private Control createWaterBodyTable( final DatabindingWizardPage binding, final Composite parent )
  {
    m_waterBodiesViewer = new TableViewer( parent, SWT.BORDER | SWT.FULL_SELECTION );
    final Table table = m_waterBodiesViewer.getTable();
    table.setHeaderVisible( true );

    TableViewerUtilities.addColumnsResizeListener( m_waterBodiesViewer );

    final IValueProperty nameProperty = BeanProperties.value( WaterBodies.class, WaterBodies.PROPERTY_NAME );
    final IValueProperty gknProperty = BeanProperties.value( WaterBodies.class, WaterBodies.PROPERTY_WATERBODY );

    final TableViewerColumn gknColumn = new TableViewerColumn( m_waterBodiesViewer, SWT.LEFT );
    gknColumn.getColumn().setText( STR_GEWÄSSERKENNZIFFER );
    gknColumn.getColumn().setWidth( 100 );
    ColumnViewerSorter.registerSorter( gknColumn, new ViewerSorter() );

    final TableViewerColumn nameColumn = new TableViewerColumn( m_waterBodiesViewer, SWT.LEFT );
    nameColumn.getColumn().setText( STR_NAME );
    nameColumn.getColumn().setWidth( 100 );
    ColumnViewerSorter.registerSorter( nameColumn, new ViewerSorter() );

    final List< ? > waterBodies = loadWaterbodies();
    m_tableInput = new WritableList( waterBodies, WaterBodies.class );

    final IValueProperty[] labelProperties = new IValueProperty[] { gknProperty, nameProperty };
    ViewerSupport.bind( m_waterBodiesViewer, m_tableInput, labelProperties );

    /* selection -> data */
    final IViewerObservableValue target = ViewersObservables.observeSinglePostSelection( m_waterBodiesViewer );
    final IObservableValue model = BeansObservables.observeValue( m_data, ImportGafData.PROPERTY_WATER_BODY );

    final DataBinder dataBinder = new DataBinder( target, model );
    dataBinder.addTargetAfterGetValidator( new NotNullValidator<WaterBodies>( WaterBodies.class, IStatus.ERROR, "no water body is selected" ) );
    binding.bindValue( dataBinder );

    return table;
  }

  protected List< ? > loadWaterbodies( )
  {
    try
    {
      return m_connection.getWaterBodies();
    }
    catch( final PdbConnectException e )
    {
      e.printStackTrace();
      return Collections.singletonList( "Failed to load waterbodies: " + e.toString() );
    }
  }

  private Control createSearchFields( final Composite parent )
  {
    final Group panel = new Group( parent, SWT.NONE );
    panel.setText( "Filter" );
    GridLayoutFactory.swtDefaults().numColumns( 6 ).applyTo( panel );

    new Label( panel, SWT.NONE ).setText( STR_GEWÄSSERKENNZIFFER );

    final Text gknField = new Text( panel, SWT.SINGLE | SWT.BORDER | SWT.SEARCH | SWT.ICON_CANCEL );
    gknField.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false, 2, 1 ) );

    new Label( panel, SWT.NONE ).setText( STR_NAME );

    final Text nameField = new Text( panel, SWT.SEARCH | SWT.ICON_SEARCH );
    nameField.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false, 2, 1 ) );

    nameField.addSelectionListener( new SelectionAdapter()
    {
      @Override
      public void widgetDefaultSelected( final SelectionEvent e )
      {
        if( e.detail == SWT.ICON_CANCEL )
          nameField.setText( "" ); //$NON-NLS-1$
      }
    } );

    final ModifyListener searchListener = new ModifyListener()
    {
      @Override
      public void modifyText( final ModifyEvent e )
      {
        final String gknSearch = gknField.getText();
        final String nameSearch = nameField.getText();

        updateSearch( gknSearch, nameSearch );
      }
    };

    gknField.addModifyListener( searchListener );
    nameField.addModifyListener( searchListener );

    return panel;
  }

  protected void updateSearch( final String gknSearch, final String nameSearch )
  {
    final ViewerFilter filter = new WaterBodiesFilter( gknSearch, nameSearch );
    m_waterBodiesViewer.setFilters( new ViewerFilter[] { filter } );
  }

  private Control createActions( final Composite panel )
  {
    final Action addAction = new AddWaterBodyAction( m_connection, this );
    return ActionHyperlink.createHyperlink( null, panel, SWT.NONE, addAction );
  }

  void refreshWaterBodies( )
  {
    m_tableInput.clear();
    m_tableInput.addAll( loadWaterbodies() );
  }

  public WaterBodies[] getExistingWaterbodies( )
  {
    return (WaterBodies[]) m_tableInput.toArray( new WaterBodies[m_tableInput.size()] );
  }
}