/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra?e 22
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

import java.awt.Color;
import java.awt.Graphics;
import java.util.HashMap;
import java.util.Map;

import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.core.runtime.Assert;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.dialogs.IInputValidator;
import org.eclipse.jface.dialogs.InputDialog;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.viewers.IContentProvider;
import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.StructuredViewer;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.ScrolledComposite;
import org.eclipse.swt.events.ControlAdapter;
import org.eclipse.swt.events.ControlEvent;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.handlers.IHandlerService;
import org.kalypso.afgui.scenarios.SzenarioDataProvider;
import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.eclipse.jface.viewers.StatusAndDelegateContentProvider;
import org.kalypso.contribs.eclipse.jface.viewers.StatusAndDelegateLabelProvider;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.gml.ui.KalypsoGmlUIPlugin;
import org.kalypso.gml.ui.KalypsoGmlUiImages;
import org.kalypso.ogc.gml.AbstractCascadingLayerTheme;
import org.kalypso.ogc.gml.CascadingThemeHelper;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.map.widgets.AbstractWidget;
import org.kalypso.risk.model.schema.binding.IAnnualCoverageCollection;
import org.kalypso.risk.model.schema.binding.IRasterDataModel;
import org.kalypso.risk.plugin.KalypsoRiskPlugin;
import org.kalypso.ui.editor.gmleditor.ui.GMLContentProvider;
import org.kalypso.ui.editor.mapeditor.views.IWidgetWithOptions;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.gml.binding.commons.ICoverage;
import org.kalypsodeegree_impl.gml.binding.commons.ICoverageCollection;
import org.kalypsodeegree_impl.model.feature.gmlxpath.GMLXPath;
import org.kalypsodeegree_impl.model.feature.gmlxpath.GMLXPathSegment;

import de.renew.workflow.contexts.ICaseHandlingSourceProvider;

/**
 * A widget with option pane, which allows the user to manage (add/remove) run-off events and to import water level data
 * for each event.
 * 
 * @author Thomas Jung
 */
public class EventManagementWidget extends AbstractWidget implements IWidgetWithOptions
{
  private TreeViewer m_eventViewer;

  protected SzenarioDataProvider m_dataProvider;

  private IRasterDataModel m_model;

  private Object[] m_treeSelection;

  private final Map<String, Button> m_buttonsMap;

  public EventManagementWidget( )
  {
    super( "Ereignisse verwalten", "Ereignisse verwalten" );
    m_buttonsMap = new HashMap<String, Button>();
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#activate(org.kalypso.commons.command.ICommandTarget,
   *      org.kalypso.ogc.gml.map.MapPanel)
   */
  @Override
  public void activate( final ICommandTarget commandPoster, final MapPanel mapPanel )
  {
    // prepare for exception
    m_dataProvider = null;
    m_model = null;

    super.activate( commandPoster, mapPanel );

    final IHandlerService service = (IHandlerService) PlatformUI.getWorkbench().getService( IHandlerService.class );
    final IEvaluationContext context = service.getCurrentState();
    final SzenarioDataProvider dataProvider = (SzenarioDataProvider) context.getVariable( ICaseHandlingSourceProvider.ACTIVE_CASE_DATA_PROVIDER_NAME );
    try
    {
      final IRasterDataModel model = dataProvider.getModel( IRasterDataModel.class );
      if( model != null )
      {
        m_dataProvider = dataProvider;
        m_model = model;
      }
    }
    catch( final CoreException e )
    {
      e.printStackTrace();
    }
  }

  /**
   * @see org.kalypso.ui.editor.mapeditor.views.IWidgetWithOptions#createControl(org.eclipse.swt.widgets.Composite,
   *      org.eclipse.ui.forms.widgets.FormToolkit)
   */
  public Control createControl( final Composite parent, final FormToolkit toolkit )
  {
    // TRICKY: Scrolling behaviour:
    // - vertical: all components have a minimum height and fill the whole area
    // - vertical: if size is too small, a global scroll-bar appears
    // - horizontal: the panel has a minimum widht, if the total area is too small, a global scrollbar appears
    // - horizontal: the components (tree/table) have their additional horizontal scrollbars which appars if the content
    // of the individual control is too big

    final ScrolledComposite sc = new ScrolledComposite( parent, SWT.V_SCROLL | SWT.H_SCROLL );
    sc.setMinWidth( 200 );
    sc.setExpandVertical( true );
    sc.setExpandHorizontal( true );

    final Composite panel = toolkit.createComposite( sc, SWT.NONE );
    panel.setLayout( new GridLayout() );

    sc.setContent( panel );
    parent.addControlListener( new ControlAdapter()
    {
      /**
       * @see org.eclipse.swt.events.ControlAdapter#controlResized(org.eclipse.swt.events.ControlEvent)
       */
      @Override
      public void controlResized( final ControlEvent e )
      {
        final Point size = panel.computeSize( SWT.DEFAULT, SWT.DEFAULT );
        panel.setSize( size );
        sc.setMinHeight( size.y );
      }
    } );

    // Basic Layout

    final Composite treePanel = toolkit.createComposite( panel, SWT.NONE );
    final GridLayout treePanelLayout = new GridLayout( 2, false );
    final GridData treePanelData = new GridData( SWT.FILL, SWT.FILL, true, false );
    treePanelData.heightHint = 200;
    treePanel.setLayoutData( treePanelData );
    treePanelLayout.marginHeight = 0;
    treePanelLayout.marginWidth = 0;
    treePanel.setLayout( treePanelLayout );

    m_eventViewer = new TreeViewer( treePanel, SWT.SINGLE | SWT.H_SCROLL | SWT.V_SCROLL | SWT.BORDER );
    final GridData eventViewerData = new GridData( SWT.FILL, SWT.FILL, true, false );
    eventViewerData.heightHint = 100;
    m_eventViewer.getControl().setLayoutData( eventViewerData );
    toolkit.adapt( m_eventViewer.getControl(), true, false );

    final Composite treeButtonPanel = toolkit.createComposite( treePanel );
    final FillLayout treeButtonPanelLayout = new FillLayout( SWT.VERTICAL );
    treeButtonPanelLayout.spacing = 4;
    treeButtonPanel.setLayout( treeButtonPanelLayout );
    treeButtonPanel.setLayoutData( new GridData( SWT.CENTER, SWT.BEGINNING, false, true ) );

    /* Fill contents */
    initalizeEventViewer( m_eventViewer );
    initalizeTreeActions( toolkit, treeButtonPanel );

    /* Hook Events */
    m_eventViewer.addSelectionChangedListener( new ISelectionChangedListener()
    {
      @SuppressWarnings("synthetic-access")
      public void selectionChanged( final SelectionChangedEvent event )
      {
        final IStructuredSelection selection = (IStructuredSelection) event.getSelection();
        m_treeSelection = selection.toArray();

        if( m_treeSelection != null && m_treeSelection.length > 0 )
        {
          if( m_treeSelection[0] instanceof Feature )
          {
            final Feature feature = (Feature) m_treeSelection[0];
            if( feature.getAdapter( ICoverage.class ) != null )
            {
              m_buttonsMap.get( "ADD" ).setEnabled( false );
              m_buttonsMap.get( "REMOVE" ).setEnabled( false );
            }
            if( feature.getAdapter( IAnnualCoverageCollection.class ) != null )
            {
              m_buttonsMap.get( "ADD" ).setEnabled( true );
              m_buttonsMap.get( "REMOVE" ).setEnabled( true );
            }
          }
        }

        parent.layout( true, true );

        final Point size = panel.computeSize( SWT.DEFAULT, SWT.DEFAULT );
        panel.setSize( size );
        sc.setMinHeight( size.y );

        getMapPanel().repaint();
      }
    } );

    final Point size = panel.computeSize( SWT.DEFAULT, SWT.DEFAULT );
    panel.setSize( size );
    sc.setMinHeight( size.y );
    // sc.setMinSize( panel.computeSize( SWT.DEFAULT, SWT.DEFAULT ) );

    return panel;
  }

  private void initalizeEventViewer( final StructuredViewer viewer )
  {
    final GMLContentProvider gmlcp = new GMLContentProvider( false );
    final IContentProvider cp = new StatusAndDelegateContentProvider( gmlcp );
    final ILabelProvider lp = new StatusAndDelegateLabelProvider( new FeatureNameLabelProvider() );
    final CoverageFilterViewerFilter coverageFilter = new CoverageFilterViewerFilter();

    viewer.setContentProvider( cp );
    viewer.setLabelProvider( lp );
    viewer.addFilter( coverageFilter );

    if( m_model == null )
      viewer.setInput( StatusUtilities.createErrorStatus( "Flood-Modell nicht geladen." ) );
    else
    {
      viewer.setInput( m_model.getWrappedFeature().getWorkspace() );

      final GMLXPathSegment segment = GMLXPathSegment.forQName( IRasterDataModel.QNAME );
      final GMLXPath pathToModel = new GMLXPath( segment );
      final GMLXPath rootPath = new GMLXPath( pathToModel, IRasterDataModel.PROPERTY_WATERLEVEL_COVERAGE_COLLECTION );
      gmlcp.setRootPath( rootPath );

      final IFeatureWrapperCollection<IAnnualCoverageCollection> collection = m_model.getWaterlevelCoverageCollection();
      if( collection.size() > 0 )
        m_eventViewer.setSelection( new StructuredSelection( collection.get( 0 ) ), true );
    }
  }

  private void initalizeTreeActions( final FormToolkit toolkit, final Composite parent )
  {
    // We are reusing images of KalypsoGmlUi here
    final ImageDescriptor addEventID = KalypsoGmlUIPlugin.getImageProvider().getImageDescriptor( KalypsoGmlUiImages.DESCRIPTORS.COVERAGE_ADD );
    final ImageDescriptor removeID = KalypsoGmlUIPlugin.getImageProvider().getImageDescriptor( KalypsoGmlUiImages.DESCRIPTORS.COVERAGE_REMOVE );

    final Action addEventAction = new Action( "AddEvent", addEventID )
    {
      /**
       * @see org.eclipse.jface.action.Action#runWithEvent(org.eclipse.swt.widgets.Event)
       */
      @Override
      public void runWithEvent( final Event event )
      {
        handleAddEvent( event );
      }
    };
    addEventAction.setDescription( "Neues Ereignis" );

    final Action removeAction = new Action( "Remove", removeID )
    {
      /**
       * @see org.eclipse.jface.action.Action#runWithEvent(org.eclipse.swt.widgets.Event)
       */
      @Override
      public void runWithEvent( final Event event )
      {
        handleRemove( event );
      }
    };
    removeAction.setDescription( "Ereignis/Wasserspiegel l?schen" );

    createButton( toolkit, parent, addEventAction, "ADD" );
    createButton( toolkit, parent, removeAction, "REMOVE" );
  }

  private void createButton( final FormToolkit toolkit, final Composite parent, final IAction action, final String key )
  {
    final Button button = toolkit.createButton( parent, null, SWT.PUSH );
    final Image image = action.getImageDescriptor().createImage( true );
    button.setImage( image );
    button.setToolTipText( action.getDescription() );
    m_buttonsMap.put( key, button );
    button.addSelectionListener( new SelectionAdapter()
    {
      /**
       * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
       */
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        final Event event = new Event();
        event.display = e.display;
        // ...

        action.runWithEvent( event );
      }
    } );

    button.addDisposeListener( new DisposeListener()
    {
      public void widgetDisposed( final DisposeEvent e )
      {
        image.dispose();
      }
    } );
  }

  protected void handleAddEvent( final Event event )
  {
    final IInputValidator inputValidator = new IInputValidator()
    {
      public String isValid( final String newText )
      {
        if( newText == null || newText.length() == 0 )
          return "Return period cannot be empty.";
        try
        {
          final int i = Integer.parseInt( newText );
          if( i <= 0 )
            return "Return period cannot be zero or negative.";
          for( final IAnnualCoverageCollection collection : m_model.getWaterlevelCoverageCollection() )
            if( collection.getReturnPeriod() == i )
              return "Flood event with this return period is already defined.";
        }
        catch( final NumberFormatException e )
        {
          return "Valid return period is a positive integer value.";
        }
        if( newText == null || newText.length() == 0 )
          return "Return period cannot be empty.";

        return null;
      }
    };

    // show input dialog
    final Shell shell = event.display.getActiveShell();
    final InputDialog dialog = new InputDialog( shell, "New flood event", "Please enter the return period of the flood event:", "", inputValidator );
    if( dialog.open() != Window.OK )
      return;

    final String eventName = "HQ " + dialog.getValue();
    final IRasterDataModel model = m_model;
    final AbstractCascadingLayerTheme wspThemes = CascadingThemeHelper.getNamedCascadingTheme( getMapPanel().getMapModell(), "HQ" );
    Assert.isNotNull( wspThemes, "Wasserspiegel-Themen nicht vorhanden" );

    final ICoreRunnableWithProgress operation = new AddEventOperation( eventName, Integer.parseInt( dialog.getValue() ), model, wspThemes, m_dataProvider );

    final IStatus resultStatus = ProgressUtilities.busyCursorWhile( operation );
    if( !resultStatus.isOK() )
      KalypsoRiskPlugin.getDefault().getLog().log( resultStatus );
    ErrorDialog.openError( shell, "Ereignis hinzuf?gen", "Fehler beim Erzeugen des Ereignisses", resultStatus );
  }

  protected void handleRemove( final Event event )
  {
    if( m_treeSelection == null )
      return;

    final AbstractCascadingLayerTheme wspThemes = CascadingThemeHelper.getNamedCascadingTheme( getMapPanel().getMapModell(), "HQ" );

    final ICoreRunnableWithProgress operation = new RemoveEventOperation( m_treeSelection, m_dataProvider, wspThemes );

    final Shell shell = event.display.getActiveShell();

    final IStatus resultStatus = ProgressUtilities.busyCursorWhile( operation );
    if( !resultStatus.isOK() )
      KalypsoRiskPlugin.getDefault().getLog().log( resultStatus );
    ErrorDialog.openError( shell, "Ereignis l?schen", "Fehler beim L?schen des Ereignisses", resultStatus );
  }

  /**
   * @see org.kalypso.ui.editor.mapeditor.views.IWidgetWithOptions#disposeControl()
   */
  public void disposeControl( )
  {
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#paint(java.awt.Graphics)
   */
  @Override
  public void paint( final Graphics g )
  {
    super.paint( g );

    if( m_treeSelection == null )
      return;

    for( final Object selectedObject : m_treeSelection )
    {
      if( selectedObject instanceof Feature )
      {
        final Object adaptedObject = adaptToKnownObject( selectedObject );

        if( adaptedObject instanceof ICoverage )
          paintEnvelope( g, ((ICoverage) adaptedObject).getWrappedFeature().getEnvelope() );
        else if( adaptedObject instanceof IAnnualCoverageCollection )
        {
          paintEnvelope( g, ((IAnnualCoverageCollection) adaptedObject).getWrappedList().getBoundingBox() );
        }

        final GM_Envelope envelope = envelopeForSelected( selectedObject );
        paintEnvelope( g, envelope );
      }
    }
  }

  private void paintEnvelope( final Graphics g, final GM_Envelope envelope )
  {
    if( envelope == null )
      return;

    final GeoTransform projection = getMapPanel().getProjection();

    final GM_Position minPoint = projection.getDestPoint( envelope.getMin() );
    final GM_Position maxPoint = projection.getDestPoint( envelope.getMax() );

    final int x = (int) Math.min( minPoint.getX(), maxPoint.getX() );
    final int y = (int) Math.min( minPoint.getY(), maxPoint.getY() );

    final int width = (int) Math.abs( minPoint.getX() - maxPoint.getX() );
    final int height = (int) Math.abs( minPoint.getY() - maxPoint.getY() );

    g.setColor( Color.RED );
    g.drawRect( x, y, width, height );
  }

  private GM_Envelope envelopeForSelected( final Object selectedObject )
  {
    final Object adaptedObject = adaptToKnownObject( selectedObject );

    if( adaptedObject instanceof ICoverageCollection )
      return ((ICoverageCollection) adaptedObject).getWrappedFeature().getEnvelope();

    if( adaptedObject instanceof ICoverage )
      return ((ICoverage) adaptedObject).getWrappedFeature().getEnvelope();

    return null;
  }

  private Object adaptToKnownObject( final Object object )
  {
    if( !(object instanceof Feature) )
      return null;

    final Feature feature = (Feature) object;

    final Object event = feature.getAdapter( IAnnualCoverageCollection.class );
    if( event != null )
      return event;

    return feature.getAdapter( ICoverage.class );
  }

}
