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
package org.kalypso.kalypso1d2d.internal.importNet;

import java.awt.Graphics;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collection;

import org.eclipse.core.databinding.beans.BeansObservables;
import org.eclipse.core.databinding.observable.value.IObservableValue;
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
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Table;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.forms.widgets.Section;
import org.kalypso.commons.databinding.SimpleDataBinding;
import org.kalypso.contribs.eclipse.jface.action.ActionButton;
import org.kalypso.contribs.eclipse.jface.action.ActionHyperlink;
import org.kalypso.contribs.eclipse.jface.viewers.ColumnViewerUtil;
import org.kalypso.contribs.eclipse.jface.viewers.ViewerColumnItem;
import org.kalypso.contribs.eclipse.jface.viewers.table.ColumnsResizeControlListener;
import org.kalypso.contribs.eclipse.jface.wizard.IUpdateable;
import org.kalypso.contribs.eclipse.swt.widgets.SectionUtils;
import org.kalypso.kalypso1d2d.internal.i18n.Messages;
import org.kalypso.ogc.gml.command.ChangeExtentCommand;
import org.kalypso.ogc.gml.map.IMapPanel;
import org.kalypso.ogc.gml.map.widgets.advanced.utils.SLDPainter2;
import org.kalypso.ogc.gml.widgets.AbstractWidget;
import org.kalypso.ui.editor.mapeditor.views.IWidgetWithOptions;
import org.kalypsodeegree.KalypsoDeegreePlugin;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

import com.bce.gis.io.zweidm.IPolygonWithName;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Envelope;

/**
 * @author Gernot Belger
 */
public class Import2dElementsWidget extends AbstractWidget implements IWidgetWithOptions
{
  /* Keeps actions for update */
  private final Collection<IAction> m_actions = new ArrayList<>();

  private final Import2dElementsData m_data = new Import2dElementsData();

  private final SLDPainter2 m_elementPainter = new SLDPainter2( new URL[] { getClass().getResource( "resources/elements.sld" ) } ); //$NON-NLS-1$

  private final SLDPainter2 m_selectedElementPainter = new SLDPainter2( new URL[] { getClass().getResource( "resources/elementsSelected.sld" ) } ); //$NON-NLS-1$

  private SimpleDataBinding m_binding;

  private Composite m_control;

  public Import2dElementsWidget( )
  {
    super( "2D-Import", "Import 2D-Elements" ); //$NON-NLS-1$ //$NON-NLS-2$
  }

  @Override
  public void disposeControl( )
  {
    if( m_binding != null )
      m_binding.dispose();
  }

  @Override
  public String getPartName( )
  {
    return null;
  }

  @Override
  public Control createControl( final Composite parent, final FormToolkit toolkit )
  {
    m_binding = new SimpleDataBinding( toolkit );

    m_control = toolkit.createComposite( parent );
    GridLayoutFactory.fillDefaults().applyTo( m_control );

    final Control bce2dImportSection = createBce2dImportSection( toolkit, m_control );
    bce2dImportSection.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );

    final Section overviewSection = createOverviewSection( toolkit, m_control );
    overviewSection.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, false ) );

    final Section elementsSection = createElementsSection( toolkit, m_control );
    elementsSection.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );

    /* 'Apply To' button */
    final Composite buttonPanel = toolkit.createComposite( m_control );
    GridLayoutFactory.swtDefaults().applyTo( buttonPanel );
    buttonPanel.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );

    final ConvertToModelAction convertToModelAction = new ConvertToModelAction( m_data, this );
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

    return m_control;
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
    section.setText( Messages.getString("Import2dElementsWidget_0") ); //$NON-NLS-1$

    createDatasetActions( section );

    final Import2dDatasetComposite statisticsComposite = new Import2dDatasetComposite( section, toolkit, m_data, m_binding );
    section.setClient( statisticsComposite );

    return section;
  }

  private Section createElementsSection( final FormToolkit toolkit, final Composite panel )
  {
    final Section section = toolkit.createSection( panel, Section.EXPANDED | Section.TITLE_BAR );
    section.setText( Messages.getString("Import2dElementsWidget_1") ); //$NON-NLS-1$

    final Table table = toolkit.createTable( section, SWT.BORDER | SWT.FULL_SELECTION );
    section.setClient( table );
    table.addControlListener( new ColumnsResizeControlListener() );
    table.setHeaderVisible( true );

    final TableViewer viewer = new TableViewer( table );
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

    /* Name column */
    final ViewerColumn nameColumn = ColumnViewerUtil.createViewerColumn( viewer, SWT.LEFT );
    final ViewerColumnItem nameItem = new ViewerColumnItem( nameColumn );

    nameItem.setText( "ID" ); //$NON-NLS-1$
    nameItem.setResizable( false );
    ColumnsResizeControlListener.setMinimumPackWidth( nameItem.getColumn() );
    nameColumn.setLabelProvider( new PolygonWithNameLabelProvider() );
    // ColumnViewerSorter.registerSorter( nameColumn, new PdbNameComparator() );

    /* input binding */
    final IObservableValue targetInput = ViewersObservables.observeInput( viewer );
    final IObservableValue modelInput = BeansObservables.observeValue( m_data, Import2dElementsData.PROPERTY_ELEMENTS );
    m_binding.bindValue( targetInput, modelInput );

    /* selection binding */
    final IObservableValue targetSelection = ViewersObservables.observeSinglePostSelection( viewer );
    final IObservableValue modelSelection = BeansObservables.observeValue( m_data, Import2dElementsData.PROPERTY_SELECTED_ELEMENT );
    m_binding.bindValue( targetSelection, modelSelection );

    return section;
  }

  protected void handleOpenElement( )
  {
    final IPolygonWithName selectedElement = m_data.getSelectedElement();
    if( selectedElement == null )
      return;

    final IMapPanel mapPanel = getMapPanel();
    if( mapPanel == null )
      return;

    final String srsName = KalypsoDeegreePlugin.getDefault().getCoordinateSystem();

    final Envelope envelope = selectedElement.getEnvelope();
    final Coordinate centre = envelope.centre();
    final GM_Point centroid = GeometryFactory.createGM_Point( centre.x, centre.y, srsName );

    /* Get the new paned bounding box to the centroid of the geometry. */
    final GM_Envelope boundingBox = mapPanel.getBoundingBox();
    final GM_Envelope paned = boundingBox.getPaned( centroid );

    setExtent( paned );
  }

  private void createDatasetActions( final Section section )
  {
    final ToolBarManager manager = SectionUtils.createSectionToolbar( section );

    final Import2dImportAction importAction = new Import2dImportAction( m_data );
    final ClearDatasetAction clearAction = new ClearDatasetAction( m_data );
    final JumpToDatasetAction jumpToAction = new JumpToDatasetAction( m_data, this );

    manager.add( importAction );
    manager.add( clearAction );
    manager.add( jumpToAction );

    m_actions.add( importAction );
    m_actions.add( clearAction );
    m_actions.add( jumpToAction );

    manager.update( true );
  }

  @Override
  public void paint( final Graphics g )
  {
    final IMapPanel panel = getMapPanel();
    if( panel == null )
      return;

    final GeoTransform projection = panel.getProjection();

    final String srsName = KalypsoDeegreePlugin.getDefault().getCoordinateSystem();

    /* paint imported elements */
    final IPolygonWithName[] elements = m_data.getElements();
    for( final IPolygonWithName element : elements )
      paintElement( m_elementPainter, g, projection, element, srsName );

    /* paint elements selected in table */
    final IPolygonWithName selectedElement = m_data.getSelectedElement();
    paintElement( m_selectedElementPainter, g, projection, selectedElement, srsName );
  }

  private void paintElement( final SLDPainter2 elementPainter, final Graphics g, final GeoTransform projection, final IPolygonWithName element, final String srsName )
  {
    if( element == null )
      return;

    try
    {
      final GM_Object surface = JTSAdapter.wrap( element.getPolygon(), srsName );
      elementPainter.paint( g, projection, surface );
    }
    catch( final GM_Exception e )
    {
      e.printStackTrace();
    }
  }

  protected void updateControlSWT( final Shell shell )
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

    repaintMap();
  }

  public void setExtent( final GM_Envelope wishBBox )
  {
    final IMapPanel mapPanel = getMapPanel();

    if( mapPanel != null && wishBBox != null )
      postViewCommand( new ChangeExtentCommand( mapPanel, wishBBox ), null );
  }
}