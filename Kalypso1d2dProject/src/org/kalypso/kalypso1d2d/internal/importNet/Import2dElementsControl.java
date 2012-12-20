/** This file is part of Kalypso
 *
 *  Copyright (c) 2012 by
 *
 *  Björnsen Beratende Ingenieure GmbH, Koblenz, Germany (Bjoernsen Consulting Engineers), http://www.bjoernsen.de
 *  Technische Universität Hamburg-Harburg, Institut für Wasserbau, Hamburg, Germany
 *  (Technical University Hamburg-Harburg, Institute of River and Coastal Engineering), http://www.tu-harburg.de/wb/
 *
 *  Kalypso is free software: you can redistribute it and/or modify it under the terms  
 *  of the GNU Lesser General Public License (LGPL) as published by the Free Software 
 *  Foundation, either version 3 of the License, or (at your option) any later version.
 *
 *  Kalypso is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied 
 *  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with Kalypso.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.kalypso.kalypso1d2d.internal.importNet;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.ArrayList;
import java.util.Collection;

import org.eclipse.core.databinding.beans.BeansObservables;
import org.eclipse.core.databinding.observable.value.IObservableValue;
import org.eclipse.core.databinding.observable.value.IValueChangeListener;
import org.eclipse.core.databinding.observable.value.ValueChangeEvent;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.action.ToolBarManager;
import org.eclipse.jface.databinding.viewers.ViewersObservables;
import org.eclipse.jface.layout.GridLayoutFactory;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.IOpenListener;
import org.eclipse.jface.viewers.OpenEvent;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.ViewerColumn;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Table;
import org.eclipse.ui.forms.widgets.Form;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.forms.widgets.Section;
import org.kalypso.commons.databinding.IDataBinding;
import org.kalypso.commons.databinding.forms.DatabindingForm;
import org.kalypso.contribs.eclipse.jface.action.ActionButton;
import org.kalypso.contribs.eclipse.jface.action.ActionHyperlink;
import org.kalypso.contribs.eclipse.jface.viewers.ColumnViewerUtil;
import org.kalypso.contribs.eclipse.jface.viewers.ViewerColumnItem;
import org.kalypso.contribs.eclipse.jface.viewers.table.ColumnsResizeControlListener;
import org.kalypso.contribs.eclipse.jface.wizard.IUpdateable;
import org.kalypso.contribs.eclipse.swt.widgets.ColumnViewerSorter;
import org.kalypso.contribs.eclipse.swt.widgets.SectionUtils;
import org.kalypso.core.status.StatusDialog;
import org.kalypso.kalypso1d2d.internal.i18n.Messages;
import org.kalypso.ogc.gml.map.IMapPanel;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

import com.bce.gis.io.zweidm.IPolygonWithName;
import com.vividsolutions.jts.geom.Point;
import com.vividsolutions.jts.geom.Polygon;

/**
 * Control for the {@link Import2dElementsWidget}.
 * 
 * @author Gernot Belger
 */
public class Import2dElementsControl extends Composite
{
  /* Keeps actions for update */
  private final Collection<IAction> m_actions = new ArrayList<>();

  private final IDataBinding m_binding;

  private final Import2dElementsData m_data;

  private final Import2dElementsWidget m_widget;

  private TableViewer m_viewer;

  public Import2dElementsControl( final FormToolkit toolkit, final Composite parent, final Import2dElementsData data, final Import2dElementsWidget widget )
  {
    super( parent, SWT.NONE );

    m_data = data;
    m_widget = widget;

    toolkit.adapt( this );
    setLayout( new FillLayout() );

    final Form form = toolkit.createForm( this );
    final Composite body = form.getBody();

    GridLayoutFactory.fillDefaults().applyTo( body );

    m_binding = new DatabindingForm( form, toolkit );

    createContents( toolkit, body );
  }

  @Override
  public void dispose( )
  {
    if( m_binding != null )
      m_binding.dispose();

    super.dispose();
  }

  private void createContents( final FormToolkit toolkit, final Composite parent )
  {
    final Control bce2dImportSection = createBce2dImportSection( toolkit, parent );
    bce2dImportSection.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );

    final Section overviewSection = createOverviewSection( toolkit, parent );
    overviewSection.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, false ) );

    final Section elementsSection = createElementsSection( toolkit, parent );
    elementsSection.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );

    /* 'Apply To' button */
    final Composite buttonPanel = toolkit.createComposite( parent );
    GridLayoutFactory.swtDefaults().applyTo( buttonPanel );
    buttonPanel.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );

    final ConvertToModelAction convertToModelAction = new ConvertToModelAction( m_data, m_widget );
    final Button buttonConvertToModel = ActionButton.createButton( toolkit, buttonPanel, convertToModelAction );
    buttonConvertToModel.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );

    m_actions.add( convertToModelAction );

    /* event handling */
    final Shell shell = parent.getShell();
    m_data.addPropertyChangeListener( new PropertyChangeListener()
    {
      @Override
      public void propertyChange( final PropertyChangeEvent evt )
      {
        updateControlSWT( shell );
      }
    } );

    updateControl();
  }

  /**
   * Special case for old BCE-2D Import; should eventually merged into other import types in the end.
   */
  private Control createBce2dImportSection( final FormToolkit toolkit, final Composite parent )
  {
    final Composite panel = toolkit.createComposite( parent );
    GridLayoutFactory.swtDefaults().applyTo( panel );

    final IAction import2dAction = new ImportBce2dAction();
    ActionHyperlink.createHyperlink( toolkit, panel, SWT.PUSH, import2dAction );

    return panel;
  }

  private Section createOverviewSection( final FormToolkit toolkit, final Composite panel )
  {
    final Section section = toolkit.createSection( panel, Section.EXPANDED | Section.TITLE_BAR );
    section.setText( Messages.getString( "Import2dElementsWidget_0" ) ); //$NON-NLS-1$

    createDatasetActions( section );

    final Import2dDatasetComposite statisticsComposite = new Import2dDatasetComposite( section, toolkit, m_data, m_binding );
    section.setClient( statisticsComposite );

    return section;
  }

  private Section createElementsSection( final FormToolkit toolkit, final Composite panel )
  {
    final Section section = toolkit.createSection( panel, Section.EXPANDED | Section.TITLE_BAR );
    section.setText( Messages.getString( "Import2dElementsWidget_1" ) ); //$NON-NLS-1$

    final Table table = toolkit.createTable( section, SWT.BORDER | SWT.FULL_SELECTION );
    section.setClient( table );
    table.addControlListener( new ColumnsResizeControlListener() );
    table.setHeaderVisible( true );

    final TableViewer viewer = new TableViewer( table );
    m_viewer = viewer;
    viewer.setUseHashlookup( true );
    viewer.setContentProvider( new ArrayContentProvider() );

    viewer.addOpenListener( new IOpenListener()
    {
      @Override
      public void open( final OpenEvent event )
      {
        handleOpenElement();
      }
    } );

    /* Status column */
    final ViewerColumn statusColumn = ColumnViewerUtil.createViewerColumn( viewer, SWT.LEFT );
    final ViewerColumnItem statusItem = new ViewerColumnItem( statusColumn );

    statusItem.setText( "Status" ); //$NON-NLS-1$
    statusItem.setResizable( false );
    ColumnsResizeControlListener.setMinimumPackWidth( statusItem.getColumn() );

    statusColumn.setLabelProvider( new PolygonWithNameStatusLabelProvider() );
    ColumnViewerSorter.registerSorter( statusColumn, new PolygonWithNameStatusComparator() );

    /* Name column */
    final ViewerColumn nameColumn = ColumnViewerUtil.createViewerColumn( viewer, SWT.LEFT );
    final ViewerColumnItem nameItem = new ViewerColumnItem( nameColumn );

    nameItem.setText( "ID" ); //$NON-NLS-1$
    nameItem.setResizable( false );
    ColumnsResizeControlListener.setMinimumPackWidth( nameItem.getColumn() );
    nameColumn.setLabelProvider( new PolygonWithNameLabelProvider() );

    /* input binding */
    final IObservableValue targetInput = ViewersObservables.observeInput( viewer );
    final IObservableValue modelInput = BeansObservables.observeValue( m_data, Import2dElementsData.PROPERTY_ELEMENTS );
    m_binding.bindValue( targetInput, modelInput );

    /* selection binding */
    final IObservableValue targetSelection = ViewersObservables.observeSinglePostSelection( viewer );
    final IObservableValue modelSelection = BeansObservables.observeValue( m_data, Import2dElementsData.PROPERTY_SELECTED_ELEMENT );
    m_binding.bindValue( targetSelection, modelSelection );

    modelSelection.addValueChangeListener( new IValueChangeListener()
    {
      @Override
      public void handleValueChange( final ValueChangeEvent event )
      {
        handleSelectionChanged( (IPolygonWithName)event.diff.getNewValue() );
      }
    } );

    return section;
  }

  protected void handleSelectionChanged( final IPolygonWithName selection )
  {
    try
    {
      /* Get the new paned bounding box to the centroid of the geometry. */

      final IMapPanel mapPanel = m_widget.getMapPanel();
      if( mapPanel == null )
        return;

      if( selection == null )
        return;

      final Polygon polygon = selection.getPolygon();

      final Point centroid = polygon.getCentroid();
      final GM_Point centerPoint = (GM_Point)JTSAdapter.wrapWithSrid( centroid );

      final GM_Envelope boundingBox = mapPanel.getBoundingBox();
      if( boundingBox == null )
        return;

      final GM_Envelope paned = boundingBox.getPaned( centerPoint );
      m_widget.setExtent( paned );
    }
    catch( final GM_Exception e )
    {
      e.printStackTrace();
    }
  }

  protected void handleOpenElement( )
  {
    final IPolygonWithName selectedElement = m_data.getSelectedElement();
    if( selectedElement == null )
      return;

    final IStatus status = selectedElement.getStatus();
    if( status == null )
      return;

    StatusDialog.open( getShell(), status, m_widget.getName() );
  }

  private void createDatasetActions( final Section section )
  {
    final ToolBarManager manager = SectionUtils.createSectionToolbar( section );

    final Import2dImportAction importAction = new Import2dImportAction( m_data );
    final ClearDatasetAction clearAction = new ClearDatasetAction( m_data );
    final JumpToDatasetAction jumpToAction = new JumpToDatasetAction( m_data, m_widget );
    final Analys2dImportAction analyseAction = new Analys2dImportAction( m_data, this );

    manager.add( importAction );
    manager.add( clearAction );
    manager.add( jumpToAction );
    manager.add( analyseAction );

    m_actions.add( importAction );
    m_actions.add( clearAction );
    m_actions.add( jumpToAction );
    m_actions.add( analyseAction );

    manager.update( true );
  }

  void updateControlSWT( final Shell shell )
  {
    shell.getDisplay().asyncExec( new Runnable()
    {
      @Override
      public void run( )
      {
        updateControl();
      }
    } );
  }

  void updateControl( )
  {
    for( final IAction action : m_actions )
    {
      if( action instanceof IUpdateable )
        ((IUpdateable)action).update();
    }

    final IMapPanel panel = m_widget.getMapPanel();
    if( panel != null )
      panel.repaintMap();
  }

  void handleElementsValidated( )
  {
    m_viewer.update( m_data.getElements(), null );

    updateControl();
  }
}