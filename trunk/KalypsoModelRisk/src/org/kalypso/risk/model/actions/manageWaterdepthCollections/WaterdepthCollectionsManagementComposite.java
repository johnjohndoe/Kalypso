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
package org.kalypso.risk.model.actions.manageWaterdepthCollections;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import org.deegree.model.spatialschema.GeometryException;
import org.eclipse.core.runtime.Assert;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.action.ToolBarManager;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.StructuredViewer;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.ToolBar;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.kalypso.afgui.KalypsoAFGUIFrameworkPlugin;
import org.kalypso.commons.eclipse.core.runtime.PluginImageProvider;
import org.kalypso.contribs.eclipse.jface.wizard.IUpdateable;
import org.kalypso.core.status.StatusComposite;
import org.kalypso.gml.ui.KalypsoGmlUIPlugin;
import org.kalypso.gml.ui.KalypsoGmlUiImages;
import org.kalypso.ogc.gml.IKalypsoCascadingTheme;
import org.kalypso.ogc.gml.map.IMapPanel;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.ogc.gml.mapmodel.IMapPanelProvider;
import org.kalypso.risk.i18n.Messages;
import org.kalypso.risk.model.schema.binding.IAnnualCoverageCollection;
import org.kalypso.risk.model.schema.binding.IRasterDataModel;
import org.kalypso.risk.model.utils.RiskModelHelper;
import org.kalypso.ui.editor.gmleditor.part.GMLContentProvider;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree_impl.gml.binding.commons.ICoverage;
import org.kalypsodeegree_impl.model.feature.gmlxpath.GMLXPath;
import org.kalypsodeegree_impl.model.feature.gmlxpath.GMLXPathSegment;

import de.renew.workflow.connector.cases.IScenarioDataProvider;

/**
 * @author Gernot Belger
 */
public class WaterdepthCollectionsManagementComposite extends Composite
{
  /** Needed, because the widget needs to access the selection in a non swt-thread. */
  private IStructuredSelection m_currentSelection = StructuredSelection.EMPTY;

  private final Map<String, IAction> m_actionsMap = new HashMap<>();

  private TreeViewer m_eventViewer;

  protected IScenarioDataProvider m_dataProvider;

  private IRasterDataModel m_model;

  private final IMapPanelProvider m_mapProvider;

  public WaterdepthCollectionsManagementComposite( final Composite parent, final FormToolkit toolkit, final IMapPanelProvider mapProvider )
  {
    super( parent, SWT.NONE );

    m_mapProvider = mapProvider;

    super.setLayout( new GridLayout( 2, false ) );
    toolkit.adapt( this );

    try
    {
      initData();

      createControl( this, toolkit );
    }
    catch( final CoreException e )
    {
      createErrorControl( this, toolkit, e.getStatus() );
    }
  }

  private void createErrorControl( final Composite parent, final FormToolkit toolkit, final IStatus status )
  {
    final String message = Messages.getString( "org.kalypso.risk.model.actions.manageWaterdepthCollections.WaterdepthCollectionsManagementWidget.9" ); //$NON-NLS-1$
    toolkit.createLabel( parent, message ).setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false, 2, 1 ) );

    final StatusComposite statusComposite = new StatusComposite( toolkit, parent, StatusComposite.DETAILS );
    statusComposite.setStatus( status );
    statusComposite.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false, 2, 1 ) );
  }

  @Override
  public void setLayout( final Layout layout )
  {
    throw new UnsupportedOperationException();
  }

  private void createControl( final Composite parent, final FormToolkit toolkit )
  {
    // Basic Layout
    final Tree tree = toolkit.createTree( parent, SWT.SINGLE | SWT.BORDER );
    tree.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );
    m_eventViewer = new TreeViewer( tree );

    final ToolBar toolbar = new ToolBar( parent, SWT.VERTICAL );
    // final Composite treeButtonPanel = toolkit.createComposite( parent );
    // final FillLayout treeButtonPanelLayout = new FillLayout( SWT.VERTICAL );
    // treeButtonPanelLayout.spacing = 4;
    // treeButtonPanel.setLayout( treeButtonPanelLayout );
    toolbar.setLayoutData( new GridData( SWT.CENTER, SWT.BEGINNING, false, true ) );
    toolkit.adapt( toolbar );

    /* Fill contents */
    initalizeEventViewer( m_eventViewer );
    initalizeTreeActions( toolbar );

    /* Hook Events */
    m_eventViewer.addSelectionChangedListener( new ISelectionChangedListener()
    {
      @Override
      public void selectionChanged( final SelectionChangedEvent event )
      {
        final IStructuredSelection selection = (IStructuredSelection)event.getSelection();
        handleSelectionChanged( selection );
      }
    } );

    /* initialize selection */
    final IFeatureBindingCollection<IAnnualCoverageCollection> collection = m_model.getWaterlevelCoverageCollection();
    if( collection.size() > 0 )
      m_eventViewer.setSelection( new StructuredSelection( collection.get( 0 ) ), true );
  }

  protected void handleSelectionChanged( final IStructuredSelection selection )
  {
    m_currentSelection = selection;

    final IMapPanel mapPanel = m_mapProvider.getMapPanel();

    final Collection<IAction> values = m_actionsMap.values();
    for( final IAction action : values )
    {
      if( action instanceof IUpdateable )
        ((IUpdateable)action).update();
    }

    final Object selectedElement = selection.getFirstElement();

    if( selectedElement instanceof IAnnualCoverageCollection )
    {
      final IAnnualCoverageCollection coverageCollection = (IAnnualCoverageCollection)selectedElement;
      updateEventThemes( coverageCollection );
    }

    mapPanel.repaintMap();
  }

  private void updateEventThemes( final IAnnualCoverageCollection coverageCollection )
  {
    /* Check/Add event-themes to map */
    final IMapModell mapModell = getMapPanel().getMapModell();
    final IKalypsoCascadingTheme wspThemes = RiskModelHelper.getHQiTheme( mapModell );
    Assert.isNotNull( wspThemes, Messages.getString( "org.kalypso.risk.model.actions.manageWaterdepthCollections.WaterdepthCollectionsManagementWidget.35" ) ); //$NON-NLS-1$
    try
    {
      RiskModelHelper.addEventThemes( wspThemes, coverageCollection );
    }
    catch( final CoreException e )
    {
      e.printStackTrace();
      ErrorDialog.openError( getDisplay().getActiveShell(), org.kalypso.risk.i18n.Messages.getString( "org.kalypso.risk.model.actions.manageWaterdepthCollections.WaterdepthCollectionsManagementWidget.2" ), org.kalypso.risk.i18n.Messages.getString( "org.kalypso.risk.model.actions.manageWaterdepthCollections.WaterdepthCollectionsManagementWidget.3" ), e.getStatus() ); //$NON-NLS-1$ //$NON-NLS-2$
    }
  }

  private void initData( ) throws CoreException
  {
    // prepare for exception
    m_dataProvider = null;
    m_model = null;

    final IScenarioDataProvider dataProvider = KalypsoAFGUIFrameworkPlugin.getDataProvider();

    m_model = dataProvider.getModel( IRasterDataModel.class.getName() );
    m_dataProvider = dataProvider;
  }

  private void initalizeEventViewer( final StructuredViewer viewer )
  {
    final GMLContentProvider contentProvider = new GMLContentProvider( false );
    final GMLXPathSegment segment = GMLXPathSegment.forQName( IRasterDataModel.QNAME );
    final GMLXPath pathToModel = new GMLXPath( segment );
    final GMLXPath rootPath = new GMLXPath( pathToModel, IRasterDataModel.PROPERTY_WATERLEVEL_COVERAGE_COLLECTION );
    contentProvider.setRootPath( rootPath );

    viewer.setContentProvider( contentProvider );
    viewer.setLabelProvider( new FeatureNameLabelProvider() );
    viewer.addFilter( new CoverageFilterViewerFilter() );

    viewer.setInput( m_model.getWorkspace() );
  }

  private void initalizeTreeActions( final ToolBar toolbar )
  {
    final ToolBarManager manager = new ToolBarManager( toolbar );

    final IAction addEventAction = new AddEventAction( m_model, m_dataProvider, m_eventViewer );
    final IAction changeAction = new ChangeEventAction( m_model, m_dataProvider, m_eventViewer, getMapPanel() );
    final IAction removeAction = new RemoveEventAction( m_dataProvider, m_eventViewer, getMapPanel() );

    final PluginImageProvider imageProvider = KalypsoGmlUIPlugin.getImageProvider();

    final IAction moveUpAction = new MoveEventAction( m_dataProvider, m_eventViewer, -1 );
    moveUpAction.setText( Messages.getString( "org.kalypso.risk.model.actions.manageWaterdepthCollections.WaterdepthCollectionsManagementWidget.16" ) ); //$NON-NLS-1$
    moveUpAction.setImageDescriptor( imageProvider.getImageDescriptor( KalypsoGmlUiImages.DESCRIPTORS.COVERAGE_UP ) );

    final Action moveDownAction = new MoveEventAction( m_dataProvider, m_eventViewer, 1 );
    moveDownAction.setText( Messages.getString( "org.kalypso.risk.model.actions.manageWaterdepthCollections.WaterdepthCollectionsManagementWidget.18" ) ); //$NON-NLS-1$
    moveDownAction.setImageDescriptor( imageProvider.getImageDescriptor( KalypsoGmlUiImages.DESCRIPTORS.COVERAGE_DOWN ) );

    // FIXME: ugyly: the actions should manage their state themselfs
    registerAction( manager, addEventAction, "ADD" ); //$NON-NLS-1$
    registerAction( manager, changeAction, "CHANGE" ); //$NON-NLS-1$
    registerAction( manager, removeAction, "REMOVE" ); //$NON-NLS-1$
    registerAction( manager, moveUpAction, "MOVEUP" ); //$NON-NLS-1$
    registerAction( manager, moveDownAction, "MOVEDOWN" ); //$NON-NLS-1$

    manager.update( true );
  }

  private void registerAction( final ToolBarManager manager, final IAction action, final String key )
  {
    manager.add( action );
    m_actionsMap.put( key, action );
  }

  private IMapPanel getMapPanel( )
  {
    return m_mapProvider.getMapPanel();
  }

  public GM_Envelope[] getEnvelopesForPaint( )
  {
    final Collection<GM_Envelope> envelopes = new ArrayList<>();

    /* Will be called in a non-swt-thread, so we cannot access the tree directly */
    for( final Iterator< ? > iterator = m_currentSelection.iterator(); iterator.hasNext(); )
    {
      final Object selectedObject = iterator.next();

      final GM_Envelope envelope = findEnvelope( selectedObject );
      if( envelope != null )
        envelopes.add( envelope );
    }

    return envelopes.toArray( new GM_Envelope[envelopes.size()] );
  }

  private GM_Envelope findEnvelope( final Object selectedObject )
  {
    try
    {
      if( selectedObject instanceof ICoverage )
        return ((ICoverage)selectedObject).getBoundedBy();

      if( selectedObject instanceof IAnnualCoverageCollection )
        return ((IAnnualCoverageCollection)selectedObject).getCoverages().getBoundingBox();

      return null;
    }
    catch( final GeometryException e )
    {
      e.printStackTrace();
      return null;
    }
  }
}
