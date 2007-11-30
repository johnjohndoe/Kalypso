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
package org.kalypso.kalypso1d2d.pjt.map;

import java.awt.Graphics;
import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.ComboViewer;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.ListViewer;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
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
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.ISources;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.handlers.IHandlerService;
import org.kalypso.commons.command.ICommand;
import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.contribs.eclipse.jface.viewers.ViewerUtilities;
import org.kalypso.contribs.eclipse.jface.wizard.WizardDialog2;
import org.kalypso.gmlschema.GMLSchemaUtilities;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DUIImages;
import org.kalypso.kalypsomodel1d2d.schema.binding.results.IHydrograph;
import org.kalypso.kalypsomodel1d2d.schema.binding.results.IHydrographCollection;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.IKalypsoLayerModell;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.featureview.IFeatureChangeListener;
import org.kalypso.ogc.gml.featureview.control.FeatureComposite;
import org.kalypso.ogc.gml.featureview.maker.CachedFeatureviewFactory;
import org.kalypso.ogc.gml.featureview.maker.FeatureviewHelper;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.map.widgets.AbstractThemeInfoWidget;
import org.kalypso.ogc.gml.map.widgets.AbstractWidget;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.mapmodel.IKalypsoThemePredicate;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.ogc.gml.mapmodel.IMapModellListener;
import org.kalypso.ogc.gml.mapmodel.MapModellAdapter;
import org.kalypso.ogc.gml.widgets.IWidget;
import org.kalypso.ui.editor.mapeditor.views.IWidgetWithOptions;
import org.kalypso.ui.wizards.results.SelectCalcUnitForHydrographWizard;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.event.ModellEvent;
import org.kalypsodeegree.model.feature.event.ModellEventListener;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.kalypsodeegree_impl.tools.GeometryUtilities;

/**
 * A widget with option pane, which allows the user to create, edit and delete hydrographs for 1d2d result files.<BR>
 * The user can add / remove hydrograph locations for each calculation unit.
 * 
 * @author Thomas Jung
 */
public class HydrographManagmentWidget extends AbstractWidget implements IWidgetWithOptions
{
  private IHydrographCollection m_hydrographs;

  private IHydrograph m_selectedHydrograph;

  /* predicate for hydrograph themes */
  private static final IKalypsoThemePredicate HYDROGRAPH_PREDICATE = new IKalypsoThemePredicate()
  {
    public boolean decide( IKalypsoTheme theme )
    {
      if( !(theme instanceof IKalypsoFeatureTheme) )
        return false;

      final IKalypsoFeatureTheme ft = (IKalypsoFeatureTheme) theme;
      final FeatureList featureList = ft.getFeatureList();
      final Feature hydrographsFeature = featureList == null ? null : featureList.getParentFeature();

      if( hydrographsFeature == null )
        return false;

      return GMLSchemaUtilities.substitutes( hydrographsFeature.getFeatureType(), IHydrographCollection.QNAME );
    }
  };

  private final AbstractThemeInfoWidget m_infoWidget = new AbstractThemeInfoWidget( "", "" )
  {
  };

  private final ModellEventListener m_modellistener = new ModellEventListener()
  {
    public void onModellChange( ModellEvent modellEvent )
    {
      refreshControl();
    }
  };

  private ListViewer m_hydrographViewer;

  private IKalypsoFeatureTheme m_theme;

  private ComboViewer m_themeCombo;

  private final IMapModellListener m_mapModelListener = new MapModellAdapter()
  {
    /**
     * @see org.kalypso.ogc.gml.mapmodel.MapModellAdapter#themeAdded(org.kalypso.ogc.gml.mapmodel.IMapModell,
     *      org.kalypso.ogc.gml.IKalypsoTheme)
     */
    @Override
    public void themeAdded( IMapModell source, IKalypsoTheme theme )
    {
      refreshThemeCombo();
    }

    /**
     * @see org.kalypso.ogc.gml.mapmodel.MapModellAdapter#themeRemoved(org.kalypso.ogc.gml.mapmodel.IMapModell,
     *      org.kalypso.ogc.gml.IKalypsoTheme, boolean)
     */
    @Override
    public void themeRemoved( IMapModell source, IKalypsoTheme theme, boolean lastVisibility )
    {
      refreshThemeCombo();
    }
  };

  private final String m_featureTemplateGft = "resources/hydrograph.gft";

  private Button m_addHydrographCollectionButton;

  private Button m_removeHydrographCollectionButton;

  private IWidget m_delegateWidget;

  public HydrographManagmentWidget( )
  {
    super( "Ganglinien erzeugen", "Hier können Sie für das jeweils ausgewählte Teilmodell einen Längsschnitt auf Basis der berechneten Ergebnisse erzeugen." );
  }

  protected void refreshControl( )
  {
    ViewerUtilities.refresh( m_hydrographViewer, true );
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#activate(org.kalypso.commons.command.ICommandTarget,
   *      org.kalypso.ogc.gml.map.MapPanel)
   */
  @Override
  public void activate( final ICommandTarget commandPoster, final MapPanel mapPanel )
  {
    super.activate( commandPoster, mapPanel );

    /* Search for existing hydrograph themes */
    final IMapModell mapModell = mapPanel == null ? null : mapPanel.getMapModell();
    mapModell.addMapModelListener( m_mapModelListener );

    refreshThemeCombo();

    m_infoWidget.activate( commandPoster, mapPanel );
  }

  /**
   * @see org.kalypso.ui.editor.mapeditor.views.IWidgetWithOptions#createControl(org.eclipse.swt.widgets.Composite,
   *      org.eclipse.ui.forms.widgets.FormToolkit)
   */
  public Control createControl( final Composite parent, FormToolkit toolkit )
  {
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

    /* Theme selection combo + add / remove calc unit hydrograph theme buttons */
    final Composite themeSelectionPanel = toolkit.createComposite( panel, SWT.NONE );
    themeSelectionPanel.setLayout( new GridLayout( 4, false ) );
    themeSelectionPanel.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    toolkit.createLabel( themeSelectionPanel, "Teilmodell: ", SWT.NONE );
    m_themeCombo = new ComboViewer( themeSelectionPanel, SWT.READ_ONLY | SWT.DROP_DOWN );
    final GridData comboGridData = new GridData( SWT.FILL, SWT.CENTER, true, false );
    m_themeCombo.getControl().setLayoutData( comboGridData );

    // buttons
    createAddCalcUnitButtonControl( themeSelectionPanel, toolkit );
    createRemoveCalcUnitButtonControl( themeSelectionPanel, toolkit );

    /* Hydrograph table + info pane */
    final Composite hydrographPanel = toolkit.createComposite( panel, SWT.NONE );
    final GridLayout hydrographPanelLayout = new GridLayout( 2, false );
    final GridData hydrographPanelData = new GridData( SWT.FILL, SWT.FILL, true, false );
    hydrographPanelData.heightHint = 200;
    hydrographPanel.setLayoutData( hydrographPanelData );
    hydrographPanelLayout.marginHeight = 0;
    hydrographPanelLayout.marginWidth = 0;
    hydrographPanel.setLayout( hydrographPanelLayout );

    m_hydrographViewer = new ListViewer( hydrographPanel, SWT.SINGLE | SWT.H_SCROLL | SWT.V_SCROLL | SWT.BORDER );
    final GridData hydrographViewerData = new GridData( SWT.FILL, SWT.FILL, true, false );
    hydrographViewerData.heightHint = 100;
    m_hydrographViewer.getControl().setLayoutData( hydrographViewerData );
    toolkit.adapt( m_hydrographViewer.getControl(), true, false );

    final Composite hydrographButtonPanel = toolkit.createComposite( hydrographPanel );
    final FillLayout hydrographButtonPanelLayout = new FillLayout( SWT.VERTICAL );
    hydrographButtonPanelLayout.spacing = 4;
    hydrographButtonPanel.setLayout( hydrographButtonPanelLayout );
    hydrographButtonPanel.setLayoutData( new GridData( SWT.CENTER, SWT.BEGINNING, false, true ) );

    /* Info view */
    final Group hydrographInfoGroup = new Group( panel, SWT.H_SCROLL );
    hydrographInfoGroup.setLayout( new GridLayout() );
    final GridData infoGroupData = new GridData( SWT.FILL, SWT.CENTER, true, false );
    hydrographInfoGroup.setLayoutData( infoGroupData );
    toolkit.adapt( hydrographInfoGroup );
    hydrographInfoGroup.setText( "Info" );

    final CachedFeatureviewFactory featureviewFactory = new CachedFeatureviewFactory( new FeatureviewHelper() );
    featureviewFactory.addView( getClass().getResource( m_featureTemplateGft ) );
    final FeatureComposite featureComposite = new FeatureComposite( null, null, featureviewFactory );
    featureComposite.setFormToolkit( toolkit );

    featureComposite.addChangeListener( new IFeatureChangeListener()
    {
      @SuppressWarnings("synthetic-access")
      public void featureChanged( final ICommand changeCommand )
      {
        m_theme.postCommand( changeCommand, null );
        updateHydrographProperties();
      }

      public void openFeatureRequested( final Feature feature, final IPropertyType pt )
      {
      }
    } );

    // Fill contents
    initalizeHydrographViewer( m_hydrographViewer );
    initalizeHydrographActions( toolkit, hydrographButtonPanel );

    /* Hook Events */
    m_hydrographViewer.addSelectionChangedListener( new ISelectionChangedListener()
    {
      public void selectionChanged( final SelectionChangedEvent event )
      {
        handleListSelectionChanged( parent, sc, panel, hydrographInfoGroup, featureComposite, event );
      }
    } );

    m_themeCombo.addSelectionChangedListener( new ISelectionChangedListener()
    {
      @SuppressWarnings("synthetic-access")
      public void selectionChanged( final SelectionChangedEvent event )
      {
        handleThemeComboSelected( event );
      }
    } );

    initializeThemeCombo();

    if( m_hydrographs != null && m_hydrographs.size() > 0 )
      m_hydrographViewer.setSelection( new StructuredSelection( m_hydrographs.get( 0 ) ) );

    final Point size = panel.computeSize( SWT.DEFAULT, SWT.DEFAULT );
    panel.setSize( size );
    sc.setMinHeight( size.y );

    return panel;

  }

  private void createAddCalcUnitButtonControl( Composite parent, FormToolkit toolkit )
  {
    m_addHydrographCollectionButton = toolkit.createButton( parent, null, SWT.PUSH );
    m_addHydrographCollectionButton.setToolTipText( "Klicken Sie hier, um ein Teilmodell hinzuzufügen, für das Sie Ganglinien erstellen wollen." );

    final ImageDescriptor addID = KalypsoModel1D2DUIImages.ID_HYDROGRAPH_COLLECTION_ADD;
    m_addHydrographCollectionButton.setImage( addID.createImage() );

    m_addHydrographCollectionButton.addSelectionListener( new SelectionAdapter()
    {
      @SuppressWarnings("synthetic-access")
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        final SelectCalcUnitForHydrographWizard addCalcUnitWizard = new SelectCalcUnitForHydrographWizard();
        addCalcUnitWizard.init( PlatformUI.getWorkbench(), new StructuredSelection() );
        addCalcUnitWizard.setMapModel( (IKalypsoLayerModell) getMapPanel().getMapModell() );
        final IHandlerService handlerService = (IHandlerService) PlatformUI.getWorkbench().getService( IHandlerService.class );
        final IEvaluationContext context = handlerService.getCurrentState();
        final Shell shell = (Shell) context.getVariable( ISources.ACTIVE_SHELL_NAME );

        final WizardDialog2 wizardDialog2 = new WizardDialog2( shell, addCalcUnitWizard );

        if( wizardDialog2.open() == Window.OK )
        {
          refreshThemeCombo();
        }

      }
    } );

  }

  private void createRemoveCalcUnitButtonControl( Composite parent, FormToolkit toolkit )
  {
    m_removeHydrographCollectionButton = toolkit.createButton( parent, null, SWT.PUSH );
    m_removeHydrographCollectionButton.setToolTipText( "Klicken Sie hier, um das aktuelle Teilmodell inkl. Ganglinien zu entfernen." );

    final ImageDescriptor removeID = KalypsoModel1D2DUIImages.ID_HYDROGRAPH_COLLECTION_REMOVE;
    m_removeHydrographCollectionButton.setImage( removeID.createImage() );

    m_removeHydrographCollectionButton.addSelectionListener( new SelectionAdapter()
    {
      @SuppressWarnings("synthetic-access")
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        final MapPanel mapPanel = getMapPanel();
        if( mapPanel == null )
          return;

        IMapModell mapModell = mapPanel.getMapModell();
        mapModell.removeTheme( m_theme );
        refreshThemeCombo();
      }
    } );

  }

  private void initializeThemeCombo( )
  {
    m_themeCombo.setContentProvider( new ArrayContentProvider() );
    m_themeCombo.setLabelProvider( new LabelProvider() );

    refreshThemeCombo();
  }

  protected void handleThemeComboSelected( final SelectionChangedEvent event )
  {
    setHydrographs( null, null );

    final IStructuredSelection selection = (IStructuredSelection) event.getSelection();
    final Object firstElement = selection.getFirstElement();

    if( firstElement instanceof IKalypsoFeatureTheme )
    {
      final IKalypsoFeatureTheme ft = (IKalypsoFeatureTheme) firstElement;
      final FeatureList featureList = ft.getFeatureList();
      final Feature hydrographsFeature = featureList == null ? null : featureList.getParentFeature();
      if( hydrographsFeature != null )
        setHydrographs( (IHydrographCollection) hydrographsFeature.getAdapter( IHydrographCollection.class ), ft );
    }
  }

  private void setHydrographs( final IHydrographCollection hydrographs, final IKalypsoFeatureTheme theme )
  {
    // remove listener
    if( m_theme != null )
    {
      CommandableWorkspace workspace = m_theme.getWorkspace();
      if( workspace == null )
        return;
      workspace.removeModellListener( m_modellistener );
    }
    m_hydrographs = hydrographs;
    m_theme = theme;

    // add listener
    if( m_theme != null )
      m_theme.getWorkspace().addModellListener( m_modellistener );

    if( m_theme == null )
      m_infoWidget.setThemes( null );
    else
      m_infoWidget.setThemes( new IKalypsoTheme[] { m_theme } );

    // updateStylePanel();

    final ListViewer hydrgraphViewer = m_hydrographViewer;
    if( hydrgraphViewer != null && !hydrgraphViewer.getControl().isDisposed() )
    {
      hydrgraphViewer.getControl().getDisplay().syncExec( new Runnable()
      {
        public void run( )
        {
          if( !hydrgraphViewer.getControl().isDisposed() )
          {
            hydrgraphViewer.setInput( hydrographs );
            if( hydrographs != null && hydrographs.size() > 0 )
              hydrgraphViewer.setSelection( new StructuredSelection( hydrographs.get( 0 ) ), true );
          }
        }
      } );
    }
  }

  protected void handleListSelectionChanged( Composite parent, ScrolledComposite sc, Composite panel, Group hydrographInfoGroup, FeatureComposite featureComposite, SelectionChangedEvent event )
  {
    final IStructuredSelection selection = (IStructuredSelection) event.getSelection();
    m_selectedHydrograph = (IHydrograph) selection.getFirstElement();

    featureComposite.disposeControl();

    if( m_selectedHydrograph != null )
    {
      featureComposite.setFeature( m_selectedHydrograph.getWrappedFeature() );
      featureComposite.createControl( hydrographInfoGroup, SWT.NONE );
      parent.layout( true, true );
    }

    final Point size = panel.computeSize( SWT.DEFAULT, SWT.DEFAULT );
    panel.setSize( size );
    sc.setMinHeight( size.y );

    getMapPanel().repaint();

  }

  private void initalizeHydrographActions( FormToolkit toolkit, Composite parent )
  {
    final ImageDescriptor addID = KalypsoModel1D2DUIImages.ID_HYDROGRAPH_ADD;
    final ImageDescriptor selectID = KalypsoModel1D2DUIImages.ID_HYDROGRAPH_SELECT;
    final ImageDescriptor removeID = KalypsoModel1D2DUIImages.ID_HYDROGRAPH_REMOVE;
    final ImageDescriptor jumptoID = KalypsoModel1D2DUIImages.ID_HYDROGRAPH_JUMP_TO;
    final ImageDescriptor exportID = KalypsoModel1D2DUIImages.ID_HYDROGRAPH_EXPORT;

    final Action addAction = new Action( "Add Coverage", addID )
    {
      /**
       * @see org.eclipse.jface.action.Action#runWithEvent(org.eclipse.swt.widgets.Event)
       */
      @Override
      public void runWithEvent( final Event event )
      {
        handleHydrographAdded( event );
      }
    };
    addAction.setDescription( "Ganglinienort hinzufügen" );

    final Action selectAction = new Action( "Select Hydrograph", selectID )
    {
      /**
       * @see org.eclipse.jface.action.Action#runWithEvent(org.eclipse.swt.widgets.Event)
       */
      @Override
      public void runWithEvent( final Event event )
      {
        handleHydrographSelected( event );
      }
    };
    selectAction.setDescription( "Ganglinienort wählen" );

    final Action removeAction = new Action( "Remove Hydrograph", removeID )
    {
      /**
       * @see org.eclipse.jface.action.Action#runWithEvent(org.eclipse.swt.widgets.Event)
       */
      @Override
      public void runWithEvent( final Event event )
      {
        handleHydrographRemoved( event );
      }
    };
    removeAction.setDescription( "Ganglinienort löschen" );

    createButton( toolkit, parent, addAction );
    createButton( toolkit, parent, selectAction );
    createButton( toolkit, parent, removeAction );

    final Action exportAction = new Action( "Export Hydrograph", exportID )
    {
      /**
       * @see org.eclipse.jface.action.Action#runWithEvent(org.eclipse.swt.widgets.Event)
       */
      @Override
      public void runWithEvent( final Event event )
      {
        handleHydrographExport( event );
      }
    };
    exportAction.setDescription( "Ganglinie exportieren" );

    final Action jumpToAction = new Action( "Jump To Hydrograph", jumptoID )
    {
      /**
       * @see org.eclipse.jface.action.Action#run()
       */
      @Override
      public void run( )
      {
        handleHydrographJumpTo();
      }
    };
    jumpToAction.setDescription( "Springe zu Ganglinienort" );

    createButton( toolkit, parent, exportAction );
    createButton( toolkit, parent, jumpToAction );

  }

  protected void handleHydrographExport( @SuppressWarnings("unused")
  Event event )
  {
    // TODO Auto-generated method stub

  }

  protected void handleHydrographSelected( @SuppressWarnings("unused")
  Event event )
  {
    // set widget
    EditHydrographWidget widget = new EditHydrographWidget( "Ganglinienpunkte", "Ganglinienpunkte selektieren", false, IHydrograph.QNAME_PROP_LOCATION, m_theme, this );
    setDelegate( widget );

  }

  protected void handleHydrographRemoved( @SuppressWarnings("unused")
  Event event )
  {
    // set widget
    RemoveHydrographWidget widget = new RemoveHydrographWidget( "Ganglinienpunkte", "Ganglinienpunkte entfernen", false, IHydrograph.QNAME_PROP_LOCATION, m_theme );
    setDelegate( widget );
  }

  protected void handleHydrographAdded( @SuppressWarnings("unused")
  Event event )
  {
    // set widget
    CreateHydrographWidget widget = new CreateHydrographWidget( "Ganglinienpunkte", "Punkte für Ganglinien hinzufügen", IHydrograph.QNAME, m_theme );
    setDelegate( widget );
  }

  protected void handleHydrographJumpTo( )
  {
    if( m_selectedHydrograph == null )
      return;

    final GM_Object location = m_selectedHydrograph.getLocation();

    GM_Envelope scaledBox;
    GM_Envelope envelope = GeometryUtilities.getEnvelope( location );
    if( location instanceof GM_Point )
    {
      GM_Point point = (GM_Point) location;
      GM_Position position = point.getPosition();
      final double newMaxX = position.getX() + 30;
      final double newMinX = position.getX() - 30;
      final double newMaxY = position.getY() + 30;
      final double newMinY = position.getY() - 30;
      GM_Position min = GeometryFactory.createGM_Position( newMinX, newMinY );
      GM_Position max = GeometryFactory.createGM_Position( newMaxX, newMaxY );
      envelope = GeometryFactory.createGM_Envelope( min, max );
    }
    else if( location instanceof GM_Curve )
    {
      GM_Curve line = (GM_Curve) location;
      envelope = line.getEnvelope();
    }

    if( envelope != null )
    {
      scaledBox = GeometryUtilities.scaleEnvelope( envelope, 1.1 );
      getMapPanel().setBoundingBox( scaledBox );
    }
  }

  private void createButton( final FormToolkit toolkit, final Composite parent, final IAction action )
  {
    final Button button = toolkit.createButton( parent, null, SWT.PUSH );
    final Image image = action.getImageDescriptor().createImage( true );
    button.setImage( image );
    button.setToolTipText( action.getDescription() );
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

  private void initalizeHydrographViewer( final ListViewer viewer )
  {
    viewer.setContentProvider( new ArrayContentProvider() );
    viewer.setLabelProvider( new LabelProvider()
    {
      /**
       * @see org.eclipse.jface.viewers.LabelProvider#getText(java.lang.Object)
       */
      @Override
      public String getText( final Object element )
      {
        final IHydrograph hydrograph = (IHydrograph) element;
        return hydrograph.getName();
      }
    } );
    viewer.setInput( m_hydrographs );

  }

  /**
   * @see org.kalypso.ui.editor.mapeditor.views.IWidgetWithOptions#disposeControl()
   */
  public void disposeControl( )
  {
    if( m_theme != null && m_modellistener != null )
      m_theme.getWorkspace().removeModellListener( m_modellistener );
  }

  /**
   * search the map / outline for existing hydrograph themes
   */
  protected void refreshThemeCombo( )
  {
    if( m_themeCombo == null || m_themeCombo.getControl().isDisposed() )
      return;

    /* get the map */
    final MapPanel mapPanel = getMapPanel();
    final IMapModell mapModell = mapPanel == null ? null : mapPanel.getMapModell();
    final IKalypsoTheme[] themes = mapModell.getAllThemes();
    final List<IKalypsoTheme> themesForCombo = new ArrayList<IKalypsoTheme>();
    for( IKalypsoTheme theme : themes )
    {
      if( HYDROGRAPH_PREDICATE.decide( theme ) )
        themesForCombo.add( theme );
      else if( theme instanceof IMapModell )
      {
        final IKalypsoTheme[] allThemes = ((IMapModell) theme).getAllThemes();
        for( final IKalypsoTheme kalypsoTheme : allThemes )
        {
          if( HYDROGRAPH_PREDICATE.decide( kalypsoTheme ) )
            themesForCombo.add( kalypsoTheme );
        }
      }
    }

    if( themesForCombo.size() > 0 )
    {
      final Control control = m_themeCombo.getControl();
      final ComboViewer themeCombo = m_themeCombo;
      control.getDisplay().asyncExec( new Runnable()
      {
        public void run( )
        {
          if( control.isDisposed() )
            return; // may be disposed meanwhile

          themeCombo.setInput( themesForCombo );

          if( themesForCombo.size() > 0 )
            themeCombo.setSelection( new StructuredSelection( themesForCombo.get( 0 ) ) );
        }
      } );
    }
  }

  protected void updateHydrographProperties( )
  {
    if( m_theme == null )
      m_infoWidget.setThemes( null );
    else
      m_infoWidget.setThemes( new IKalypsoTheme[] { m_theme } );

  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#moved(java.awt.Point)
   */
  @Override
  public void moved( java.awt.Point p )
  {
    if( m_delegateWidget != null )
      m_delegateWidget.moved( p );
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#dragged(java.awt.Point)
   */
  @Override
  public void dragged( java.awt.Point p )
  {
    if( m_delegateWidget != null )
      m_delegateWidget.dragged( p );
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#leftClicked(java.awt.Point)
   */
  @Override
  public void leftClicked( java.awt.Point p )
  {
    if( m_delegateWidget != null )
      m_delegateWidget.leftClicked( p );
  }

  private void setDelegate( final IWidget delegateWidget )
  {
    if( m_delegateWidget != null )
      m_delegateWidget.finish();

    m_delegateWidget = delegateWidget;

    if( m_delegateWidget != null )
      m_delegateWidget.activate( getCommandTarget(), getMapPanel() );
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#paint(java.awt.Graphics)
   */
  @Override
  public void paint( Graphics g )
  {
    if( m_delegateWidget != null )
      m_delegateWidget.paint( g );
  }

  public void setHydrographViewerSelection( final StructuredSelection selection )
  {
    final Display display = m_hydrographViewer.getControl().getDisplay();
    display.asyncExec( new Runnable()
    {
      @SuppressWarnings("synthetic-access")
      public void run( )
      {
        if( selection != null && m_hydrographViewer != null )
          m_hydrographViewer.setSelection( selection, true );
      }
    } );
  }
}
