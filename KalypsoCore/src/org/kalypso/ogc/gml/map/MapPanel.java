/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and

 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de

 ---------------------------------------------------------------------------------------------------*/
package org.kalypso.ogc.gml.map;

import java.awt.Canvas;
import java.awt.Color;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.RenderingHints;
import java.awt.event.ComponentEvent;
import java.awt.event.ComponentListener;
import java.awt.event.MouseEvent;
import java.awt.image.BufferedImage;
import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.runtime.ISafeRunnable;
import org.eclipse.core.runtime.SafeRunner;
import org.eclipse.jface.util.SafeRunnable;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.ISelectionProvider;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.ogc.gml.IKalypsoCascadingTheme;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.KalypsoCascadingThemeSelection;
import org.kalypso.ogc.gml.KalypsoFeatureThemeSelection;
import org.kalypso.ogc.gml.command.JMSelector;
import org.kalypso.ogc.gml.map.listeners.IMapPanelListener;
import org.kalypso.ogc.gml.map.listeners.IMapPanelPaintListener;
import org.kalypso.ogc.gml.mapmodel.IKalypsoThemeVisitor;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.ogc.gml.mapmodel.IMapModellListener;
import org.kalypso.ogc.gml.mapmodel.MapModellAdapter;
import org.kalypso.ogc.gml.mapmodel.MapModellHelper;
import org.kalypso.ogc.gml.mapmodel.visitor.KalypsoThemeChangeExtentVisitor;
import org.kalypso.ogc.gml.selection.EasyFeatureWrapper;
import org.kalypso.ogc.gml.selection.IFeatureSelection;
import org.kalypso.ogc.gml.selection.IFeatureSelectionListener;
import org.kalypso.ogc.gml.selection.IFeatureSelectionManager;
import org.kalypso.ogc.gml.widgets.WidgetManager;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.graphics.transformation.WorldToScreenTransform;
import org.kalypsodeegree_impl.model.geometry.GM_Envelope_Impl;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * @author vdoemming
 */
public class MapPanel extends Canvas implements ComponentListener, ISelectionProvider
{
  private static interface IListenerRunnable
  {
    public void visit( final IMapPanelListener l );
  }

  public static final int MODE_SELECT = 0;

  public static final int MODE_TOGGLE = 1;

  public static final int MODE_UNSELECT = 2;

  private static final long serialVersionUID = 1L;

  public final static String WIDGET_ZOOM_IN = "ZOOM_IN";

  public final static String WIDGET_ZOOM_IN_RECT = "ZOOM_IN_RECT";

  public final static String WIDGET_PAN = "PAN";

  public final static String WIDGET_EDIT_FEATURE = "EDIT_FEATURE_WITH_GEOMETRY";

  public final static String WIDGET_SELECT = "SELECT";

  public final static String WIDGET_EDIT_GEOMETRY = "EDIT_GEOMETRY";

  public final static String WIDGET_UNSELECT = "UNSELECT";

  public final static String WIDGET_TOGGLE_SELECT = "TOGGLE_SELECT";

  public final static String WIDGET_CREATE_FEATURE = "CREATE_FEATURE";

  public final static String WIDGET_CREATE_FEATURE_WITH_GEOMETRY = "CREATE_FEATURE_WITH_GEOMETRY";

  public final static String WIDGET_CREATE_FEATURE_WITH_POINT = "CREATE_FEATURE_WITH_POINT";

  public final static String WIDGET_EDIT_FEATURE_GEOMETRY = "WIDGET_EDIT_FEATURE_GEOMETRY";

  public final static String WIDGET_CREATE_FEATURE_WITH_LINESTRING = "CREATE_FEATURE_WITH_LINESTRING";

  public final static String WIDGET_CREATE_FEATURE_WITH_POLYGON = "CREATE_FEATURE_WITH_POLYGON";

  public static final String WIDGET_SINGLE_SELECT = "SINGLE_SELECT";

  static
  {
    System.setProperty( "sun.awt.noerasebackground", "true" );
  }

  private final IFeatureSelectionManager m_selectionManager;

  private final List<ISelectionChangedListener> m_selectionListeners = new ArrayList<ISelectionChangedListener>( 5 );

  private final IFeatureSelectionListener m_globalSelectionListener = new IFeatureSelectionListener()
  {
    public void selectionChanged( final IFeatureSelection selection )
    {
      globalSelectionChanged( selection );
    }
  };

  protected int m_width = 0;

  protected int m_height = 0;

  private int xOffset = 0;

  private int yOffset = 0;

  private IMapModell m_model = null;

  private final WidgetManager m_widgetManager;

  private final GeoTransform m_projection = new WorldToScreenTransform();

  protected GM_Envelope m_boundingBox = new GM_Envelope_Impl();

  private GM_Envelope m_wishBBox;

  private final List<IMapPanelListener> m_mapPanelListeners = new ArrayList<IMapPanelListener>();

  private final List<IMapPanelPaintListener> m_paintListeners = new ArrayList<IMapPanelPaintListener>();

  private Boolean m_shouldPaint = true;

  private String m_message = "";

  private final IMapModellListener m_modellListener = new MapModellAdapter()
  {
    /**
     * @see org.kalypso.ogc.gml.mapmodel.MapModellAdapter#repaintRequested(org.kalypso.ogc.gml.mapmodel.IMapModell,
     *      org.kalypsodeegree.model.geometry.GM_Envelope)
     */
    @Override
    public void repaintRequested( final IMapModell source, final GM_Envelope bbox )
    {
      forceRepaint();
    }

    /**
     * @see org.kalypso.ogc.gml.mapmodel.MapModellAdapter#themeAdded(org.kalypso.ogc.gml.mapmodel.IMapModell,
     *      org.kalypso.ogc.gml.IKalypsoTheme)
     */
    @Override
    public void themeAdded( final IMapModell source, final IKalypsoTheme theme )
    {
      if( theme.isVisible() )
      {
        /* The theme can do something with it (e.g. the WMS theme will start reloading the map). */
        theme.setExtent( m_width, m_height, m_boundingBox );

        forceRepaint();
      }
    }

    /**
     * @see org.kalypso.ogc.gml.mapmodel.MapModellAdapter#themeOrderChanged(org.kalypso.ogc.gml.mapmodel.IMapModell)
     */
    @Override
    public void themeOrderChanged( final IMapModell source )
    {
      forceRepaint();
    }

    /**
     * @see org.kalypso.ogc.gml.mapmodel.MapModellAdapter#themeRemoved(org.kalypso.ogc.gml.mapmodel.IMapModell,
     *      org.kalypso.ogc.gml.IKalypsoTheme)
     */
    @Override
    public void themeRemoved( final IMapModell source, final IKalypsoTheme theme, boolean lastVisibility )
    {
      if( lastVisibility )
        forceRepaint();
    }

    /**
     * @see org.kalypso.ogc.gml.mapmodel.MapModellAdapter#themeVisibilityChanged(org.kalypso.ogc.gml.mapmodel.IMapModell,
     *      org.kalypso.ogc.gml.IKalypsoTheme, boolean)
     */
    @Override
    public void themeVisibilityChanged( final IMapModell source, final IKalypsoTheme theme, boolean visibility )
    {
      forceRepaint();
    }

  };

  private final MapModellPainter m_mapModellPainter;

  private IPainter m_modellPainter = null;

  public MapPanel( final ICommandTarget viewCommandTarget, final IFeatureSelectionManager manager )
  {
    m_selectionManager = manager;
    m_selectionManager.addSelectionListener( m_globalSelectionListener );
    m_mapModellPainter = new MapModellPainter( this );

    // set empty Modell:
    setMapModell( null );

    m_widgetManager = new WidgetManager( viewCommandTarget, this );
    addMouseListener( m_widgetManager );
    addMouseMotionListener( m_widgetManager );
    addKeyListener( m_widgetManager );
    addComponentListener( this );

    // really needed?
    setVisible( true );
  }

  /**
   * Runns the given runnable on every listener in a safe way.
   */
  private void acceptListenersRunnable( final IListenerRunnable r )
  {
    final IMapPanelListener[] listeners = m_mapPanelListeners.toArray( new IMapPanelListener[m_mapPanelListeners.size()] );
    for( final IMapPanelListener l : listeners )
    {
      final ISafeRunnable code = new SafeRunnable()
      {
        public void run( ) throws Exception
        {
          r.visit( l );
        }
      };

      SafeRunner.run( code );
    }
  }

  /**
   * Add a listener in the mapPanel that will be notified in specific changes. <br/> At the moment there is only the
   * message changed event.
   */
  public void addMapPanelListener( final IMapPanelListener l )
  {
    m_mapPanelListeners.add( l );
  }

  public void addPaintListener( final IMapPanelPaintListener pl )
  {
    m_paintListeners.add( pl );
  }

  public void addSelectionChangedListener( final ISelectionChangedListener listener )
  {
    m_selectionListeners.add( listener );
  }

  /**
   * @deprecated Does not belong into the MapPanel. Use {@link IFeatureSelectionChanger} instead.
   */
  @Deprecated
  private void changeSelection( final List features, final IKalypsoFeatureTheme theme, final IFeatureSelectionManager selectionManager2, final int selectionMode )
  {
    // nothing was chosen by the user, clear selection
    if( features.isEmpty() )
      selectionManager2.clear();
    // TODO: this should do the widget-manager?

    // remove all selected features from this theme
    // TODO: maybe only visible??
    final FeatureList featureList = theme.getFeatureList();
    if( featureList == null )
      return;

    final Feature parentFeature = featureList.getParentFeature();
    final IRelationType parentProperty = featureList.getParentFeatureTypeProperty();

    // add all selected features
    final EasyFeatureWrapper[] selectedWrapped = new EasyFeatureWrapper[features.size()];
    for( int i = 0; i < features.size(); i++ )
    {
      final Feature f = (Feature) features.get( i );
      selectedWrapped[i] = new EasyFeatureWrapper( theme.getWorkspace(), f, parentFeature, parentProperty );
    }

    final Feature[] toRemove;
    final EasyFeatureWrapper[] toAdd;

    switch( selectionMode )
    {
      case MODE_TOGGLE: // dreht die selection der auswahl um
        // BUG: past nicht mehr zur beschreibung!
        toRemove = new Feature[0];
        toAdd = selectedWrapped;
        break;

      case MODE_SELECT: // selectert genau das, was ausgewählt wurde
        // toRemove = featureList.toFeatures();
        final EasyFeatureWrapper[] allFeatures = selectionManager2.getAllFeatures();
        toRemove = new Feature[allFeatures.length];
        for( int i = 0; i < allFeatures.length; i++ )
          toRemove[i] = allFeatures[i].getFeature();
        toAdd = selectedWrapped;
        break;

      case MODE_UNSELECT: // löscht alles augewählte aus der selection
        toRemove = featureList.toFeatures();
        toAdd = new EasyFeatureWrapper[0];

      default:
        throw new UnsupportedOperationException( "Unknown selection mode: " + selectionMode );
    }

    selectionManager2.changeSelection( toRemove, toAdd );
  }

  /**
   * @see java.awt.event.ComponentListener#componentHidden(java.awt.event.ComponentEvent)
   */
  public void componentHidden( final ComponentEvent e )
  {
    //
  }

  /**
   * @see java.awt.event.ComponentListener#componentMoved(java.awt.event.ComponentEvent)
   */
  public void componentMoved( final ComponentEvent e )
  {
    //
  }

  /**
   * @see java.awt.event.ComponentListener#componentResized(java.awt.event.ComponentEvent)
   */
  public void componentResized( final ComponentEvent e )
  {
    if( m_wishBBox != null )
      setBoundingBox( m_wishBBox );
    else
      setBoundingBox( getBoundingBox() );
  }

  /**
   * @see java.awt.event.ComponentListener#componentShown(java.awt.event.ComponentEvent)
   */
  public void componentShown( final ComponentEvent e )
  {
    if( m_wishBBox != null )
      setBoundingBox( m_wishBBox );
    else
      setBoundingBox( getBoundingBox() );
  }

  public void dispose( )
  {
    removeMouseListener( m_widgetManager );
    removeMouseMotionListener( m_widgetManager );
    removeKeyListener( m_widgetManager );
    removeComponentListener( this );

    m_selectionManager.removeSelectionListener( m_globalSelectionListener );

    m_widgetManager.dispose();

    setMapModell( null );

    // REMARK: this should not be necessary, but fixes the memory leak problem when opening/closing a .gmt file.
    // TODO: where is this map panel still referenced from?
    m_selectionListeners.clear();
    m_mapPanelListeners.clear();
    m_paintListeners.clear();

    m_modellPainter.dispose();
    m_mapModellPainter.dispose();
  }

  protected void fireExtentChanged( final GM_Envelope oldExtent, final GM_Envelope newExtent )
  {
    acceptListenersRunnable( new IListenerRunnable()
    {
      public void visit( final IMapPanelListener l )
      {
        l.onExtentChanged( MapPanel.this, oldExtent, newExtent );
      }
    } );
  }

  protected void fireMapModelChanged( final IMapModell oldModel, final IMapModell newModel )
  {
    acceptListenersRunnable( new IListenerRunnable()
    {
      public void visit( final IMapPanelListener l )
      {
        l.onMapModelChanged( MapPanel.this, oldModel, newModel );
      }
    } );
  }

  /**
   * Must be invoked, if the message of the mapPanel has changed.
   */
  private void fireMessageChanged( final String message )
  {
    acceptListenersRunnable( new IListenerRunnable()
    {
      public void visit( final IMapPanelListener l )
      {
        l.onMessageChanged( MapPanel.this, message );
      }
    } );
  }

  public final void fireSelectionChanged( )
  {
    final ISelectionChangedListener[] listenersArray = m_selectionListeners.toArray( new ISelectionChangedListener[m_selectionListeners.size()] );

    final SelectionChangedEvent e = new SelectionChangedEvent( this, getSelection() );
    for( final ISelectionChangedListener l : listenersArray )
    {
      final SafeRunnable safeRunnable = new SafeRunnable()
      {
        /**
         * Overwritten because opening the message dialog here results in a NPE
         * 
         * @see org.eclipse.jface.util.SafeRunnable#handleException(java.lang.Throwable)
         */
        @Override
        public void handleException( final Throwable t )
        {
          t.printStackTrace();
        }

        public void run( )
        {
          // TODO: fire in SWT display thread!
          // FIXE: for the moment: just commented out, it does not work
          l.selectionChanged( e );
        }
      };

      SafeRunnable.run( safeRunnable );
    }
  }

  /**
   * @see org.kalypsodeegree.model.feature.event.ModellEventListener#onModellChange(org.kalypsodeegree.model.feature.event.ModellEvent)
   */
  protected final void forceRepaint( )
  {
    xOffset = 0;
    yOffset = 0;

    invalidateMap();
  }

  public synchronized GM_Envelope getBoundingBox( )
  {
    return m_boundingBox;
  }

  /**
   * calculates the current map scale (denominator) as defined in the OGC SLD 1.0.0 specification
   * 
   * @return scale of the map
   */
  public double getCurrentScale( )
  {
    return MapModellHelper.calcScale( m_model, getBoundingBox(), getWidth(), getHeight() );
  }

  /**
   * @see org.kalypso.ogc.gml.mapmodel.IMapModellView#getMapModell()
   */
  public IMapModell getMapModell( )
  {
    return m_model;
  }

  public String getMessage( )
  {
    return m_message;
  }

  public GM_Envelope getPanToLocationBoundingBox( final double gisMX, final double gisMY )
  {
    final double ratio = m_height / m_width;

    final GeoTransform transform = getProjection();

    final double gisDX = transform.getSourceX( m_width / 2 ) - transform.getSourceX( 0 );
    final double gisDY = gisDX * ratio;
    final double gisX1 = gisMX - gisDX;
    final double gisX2 = gisMX + gisDX;
    final double gisY1 = gisMY - gisDY;
    final double gisY2 = gisMY + gisDY;

    return GeometryFactory.createGM_Envelope( gisX1, gisY1, gisX2, gisY2 );
  }

  public GM_Envelope getPanToPixelBoundingBox( final double mx, final double my )
  {
    final GeoTransform transform = getProjection();

    final double gisMX = transform.getSourceX( mx );
    final double gisMY = transform.getSourceY( my );
    return getPanToLocationBoundingBox( gisMX, gisMY );
  }

  public GeoTransform getProjection( )
  {
    return m_projection;
  }

  private double getRatio( )
  {
    return (double) getHeight() / (double) getWidth();
  }

  /**
   * @see org.eclipse.jface.viewers.ISelectionProvider#getSelection()
   */
  public ISelection getSelection( )
  {
    final IMapModell mapModell = getMapModell();
    if( mapModell == null )
      return StructuredSelection.EMPTY;

    final IKalypsoTheme activeTheme = mapModell.getActiveTheme();
    if( activeTheme instanceof IKalypsoFeatureTheme )
      return new KalypsoFeatureThemeSelection( m_selectionManager.toList(), (IKalypsoFeatureTheme) activeTheme, m_selectionManager, null, null );
    else if( activeTheme instanceof IKalypsoCascadingTheme )
      return new KalypsoCascadingThemeSelection( m_selectionManager.toList(), (IKalypsoCascadingTheme) activeTheme, m_selectionManager, null, null );

    return StructuredSelection.EMPTY;
  }

  public IFeatureSelectionManager getSelectionManager( )
  {
    return m_selectionManager;
  }

  public WidgetManager getWidgetManager( )
  {
    return m_widgetManager;
  }

  public GM_Envelope getZoomOutBoundingBox( )
  {
    final GeoTransform transform = getProjection();
    final double ratio = getRatio();
    final double gisMX = transform.getSourceX( getWidth() / 2d );
    final double gisMY = transform.getSourceY( getHeight() / 2d );

    final double gisDX = 2 * (gisMX - transform.getSourceX( 0 ));
    final double gisDY = gisDX * ratio;
    final double gisX1 = gisMX - gisDX;
    final double gisX2 = gisMX + gisDX;
    final double gisY1 = gisMY - gisDY;
    final double gisY2 = gisMY + gisDY;

    return GeometryFactory.createGM_Envelope( gisX1, gisY1, gisX2, gisY2 );
  }

  protected void globalSelectionChanged( final IFeatureSelection selection )
  {
    if( selection != null )
    {
      // TODO: only repaint, if selection contains features contained in my themes changes
    }

    invalidateMap();

    // TODO: should be fired in the SWT thread, because the global selection listeners
    // need this
    fireSelectionChanged();
  }

  /**
   * Invalidates the whole map, all data is redrawn freshly.
   * <p>
   * Calls {@link #repaint()}.
   */
  public void invalidateMap( )
  {
    final int x = 0;
    final int y = 0;
    final int w = getWidth();
    final int h = getHeight();

    synchronized( this )
    {
      m_shouldPaint = false;

      /* Cancel old job if still running. */
      if( m_mapModellPainter != null && m_modellPainter == m_mapModellPainter )
        m_mapModellPainter.cancel();

      // do not set to null, else we may get NPE in the unsynchronized 'paint'-call

      /* Determine painter depending on state of model. */
      if( m_model == null )
      {
        if( m_modellPainter == null )
          m_modellPainter = new TextPainter( "Kartenvorlage wird geladen...", w, h );
        else
          m_modellPainter = new TextPainter( "Keine Daten vorhanden...", w, h );
      }
      else if( (m_model != null) && (m_model.getThemeSize() == 0) )
        m_modellPainter = new TextPainter( "Keine Kartenthemen vorhanden", w, h );
      else
      {
        // Why -2 ?
        m_projection.setDestRect( x - 2, y - 2, w + x, h + y );

        m_modellPainter = m_mapModellPainter;

        // delay the Schedule, so if another invalidate comes within that time-span, no repaint happens at all
        m_mapModellPainter.schedule( 250 );
      }

      m_shouldPaint = true;
    }

    // do not repaint here, as the painter trigger repaint events himself. Repainting here causes
    // ugly side effects for pan
    // repaint();
  }

  /**
   * @see java.awt.Component#isDoubleBuffered()
   */
  @Override
  public boolean isDoubleBuffered( )
  {
    return true;
  }

  /**
   * <p>
   * This method was synchronized in order to fix bugs caused by threading issues concerning the setBoundBox method.
   * </p>
   * <p>
   * The bug was fixed by this, an so far no dead locks are encountered. see also {@link #getBoundingBox()}and
   * {@link #setBoundingBox(GM_Envelope)}
   * </p>
   * <p>
   * Make sure, that no call to one of the 'fire...' methods is made in the synchronized code.
   * </p>
   * 
   * @see java.awt.Component#paint(java.awt.Graphics)
   */
  @Override
  public void paint( final Graphics outerG )
  {
    final Graphics2D outerG2 = (Graphics2D) outerG;
    outerG2.setRenderingHint( RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON );
    outerG2.setRenderingHint( RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_ON );

    final int height = getHeight();
    final int width = getWidth();

    if( (height == 0) || (width == 0) )
      return;

    // update dimension
    if( (height != m_height) || (width != m_width) )
    {
      m_height = height;
      m_width = width;
    }

    final BufferedImage image = paintBuffer( height, width );

    // If offset is set, fill the rest with the background color
    if( xOffset != 0 || yOffset != 0 ) // to clear background ...
    {
      final int left = Math.max( 0, xOffset );
      final int right = Math.min( width, xOffset + width );
      final int top = Math.max( 0, yOffset );
      final int bottom = Math.min( height, yOffset + height );

      outerG2.setColor( getBackground() );
      outerG2.fillRect( 0, 0, left, height ); // left
      outerG2.fillRect( left, 0, right - left, top ); // top
      outerG2.fillRect( left, bottom, right - left, height - bottom ); // bottom
      outerG2.fillRect( right, 0, width - right, height ); // right
    }

    outerG2.drawImage( image, xOffset, yOffset, null );
  }

  private BufferedImage paintBuffer( final int height, final int width )
  {
    final BufferedImage image = new BufferedImage( width, height, BufferedImage.TYPE_INT_ARGB );

    Graphics2D g = null;

    try
    {
      g = (Graphics2D) image.getGraphics();

      /* Clear background */
      g.setColor( Color.white );
      g.fillRect( 0, 0, width, height );

      g.setRenderingHint( RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON );
      g.setRenderingHint( RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_ON );

      m_modellPainter.paint( g );

      /* avoid concurrent thread access - race condition! */
      if( m_shouldPaint )
      {
        final IMapPanelPaintListener[] pls = m_paintListeners.toArray( new IMapPanelPaintListener[] {} );
        for( final IMapPanelPaintListener pl : pls )
        {
          if( m_shouldPaint )
            pl.paint( g );
        }
      }

      paintWidget( g );
      return image;
    }
    finally
    {
      if( g != null )
        g.dispose();
    }
  }

  /**
   * Lets the active widget paint itself.
   */
  private void paintWidget( final Graphics g )
  {
    final Color color = new Color( Color.red.getRed(), Color.red.getGreen(), Color.red.getBlue(), 150 );
    // why not just use Color.red?
    g.setColor( color );

    m_widgetManager.paintWidget( g );
  }

  /**
   * Removes this listener from the mapPanel.
   */
  public void removeMapPanelListener( final IMapPanelListener l )
  {
    m_mapPanelListeners.remove( l );
  }

  public void removePaintListener( final IMapPanelPaintListener pl )
  {
    m_paintListeners.remove( pl );
  }

  public void removeSelectionChangedListener( final ISelectionChangedListener listener )
  {
    m_selectionListeners.remove( listener );
  }

  /**
   * @deprecated
   */
  @Deprecated
  public void select( final Point startPoint, final Point endPoint, final int radius, final int selectionMode, final boolean useOnlyFirstChoosen )
  {
    final GeoTransform transform = getProjection();

    final IKalypsoTheme activeTheme = m_model.getActiveTheme();
    if( (activeTheme == null) || !(activeTheme instanceof IKalypsoFeatureTheme) )
      return;

    if( startPoint != null )
    {
      final double g1x = transform.getSourceX( startPoint.getX() );
      final double g1y = transform.getSourceY( startPoint.getY() );

      if( endPoint == null ) // not dragged
      {
        // TODO depend on featuretype
        // line and point with radius
        // polygon without radius
        final double gisRadius = Math.abs( transform.getSourceX( startPoint.getX() + radius ) - g1x );
        final JMSelector selector = new JMSelector();
        final GM_Point pointSelect = GeometryFactory.createGM_Point( g1x, g1y, getMapModell().getCoordinatesSystem() );

        final Feature fe = (Feature) JMSelector.selectNearest( pointSelect, gisRadius, ((IKalypsoFeatureTheme) activeTheme).getFeatureListVisible( null ), false );

        final List<Feature> listFe = new ArrayList<Feature>();
        if( fe != null )
          listFe.add( fe );

        changeSelection( listFe, (IKalypsoFeatureTheme) activeTheme, m_selectionManager, selectionMode );
      }
      else
      // dragged
      {
        final double g2x = transform.getSourceX( endPoint.getX() );
        final double g2y = transform.getSourceY( endPoint.getY() );
        boolean withinStatus = false;

        if( (endPoint.getX() > startPoint.getX()) && (endPoint.getY() > startPoint.getY()) )
          withinStatus = true;

        final double minX = g1x < g2x ? g1x : g2x;
        final double maxX = g1x > g2x ? g1x : g2x;
        final double minY = g1y < g2y ? g1y : g2y;
        final double maxY = g1y > g2y ? g1y : g2y;

        if( (minX != maxX) && (minY != maxY) )
        {
          final GM_Envelope envSelect = GeometryFactory.createGM_Envelope( minX, minY, maxX, maxY );
          final List<Object> features = JMSelector.select( envSelect, ((IKalypsoFeatureTheme) activeTheme).getFeatureListVisible( envSelect ), withinStatus );

          if( useOnlyFirstChoosen && !features.isEmpty() )
          {
            // delete all but first if we shall only the first selected
            final Feature object = (Feature) features.get( 0 );
            features.clear();
            features.add( object );
          }

          changeSelection( features, (IKalypsoFeatureTheme) activeTheme, m_selectionManager, selectionMode );
        }
      }
    }
  }

  public synchronized void setBoundingBox( final GM_Envelope wishBBox )
  {
    // TODO: if the map is still about to be painted (e.g. from the last pan-drag); we get an ugly effect here (map
    // flashes on the old position)

    m_wishBBox = wishBBox;

    final GM_Envelope oldExtent = m_boundingBox;
    m_boundingBox = MapModellHelper.adjustBoundingBox( m_model, m_wishBBox, getRatio() );

    if( m_boundingBox != null )
    {
      // TODO: introduce a trace option for this
      // final StringBuffer dump = new StringBuffer();
      // dump.append( "MinX:" + m_boundingBox.getMin().getX() );
      // dump.append( "\nMinY:" + m_boundingBox.getMin().getY() );
      // dump.append( "\nMaxX:" + m_boundingBox.getMax().getX() );
      // dump.append( "\nMaxY:" + m_boundingBox.getMax().getY() );
      // dump.append( "\n" );
      // System.out.println( dump.toString() );

      m_projection.setSourceRect( m_boundingBox );

      // don't call onModellChange and inform the listeners
      // this is dangerous (dead lock!) inside a synchronized method
      // onModellChange( null );

      // instead invalidate the map yourself
      xOffset = 0;
      yOffset = 0;
      invalidateMap();
    }

    /* Tell the themes , that the extent has changed. */
    if( m_model != null )
    {
      final int height = getHeight();
      final int width = getWidth();

      /* Update dimension. */
      if( (height != m_height) || (width != m_width) )
      {
        m_height = height;
        m_width = width;
      }

      m_model.accept( new KalypsoThemeChangeExtentVisitor( m_width, m_height, m_boundingBox ), IKalypsoThemeVisitor.DEPTH_INFINITE );
    }

    fireExtentChanged( oldExtent, m_boundingBox );
  }

  /**
   * @see org.kalypso.ogc.gml.mapmodel.IMapModellView#setMapModell(org.kalypso.ogc.gml.mapmodel.IMapModell)
   */
  public void setMapModell( final IMapModell modell )
  {
    final IMapModell oldModel = m_model;

    if( m_model != null )
      m_model.removeMapModelListener( m_modellListener );

    m_model = modell;
    if( m_model != null )
      m_model.addMapModelListener( m_modellListener );

    invalidateMap();

    fireMapModelChanged( oldModel, m_model );
  }

  /**
   * Sets the message of this mapPanel. Some widgets update it, so that the MapView could update the status-bar text.
   */
  public void setMessage( final String message )
  {
    m_message = message;

    fireMessageChanged( message );
  }

  public void setOffset( final int dx, final int dy ) // used by pan method
  {
    xOffset = dx;
    yOffset = dy;

    repaint();
  }

  public void setSelection( final ISelection selection )
  {
    // should not be called!
    throw new UnsupportedOperationException();
  }

  @Override
  public void update( final Graphics g )
  {
    // do not clear background, it flicker even if we double buffer
    paint( g );
  }

  public void fireMouseMouveEvent( final MouseEvent e )
  {
    final IMapModell mapModell = getMapModell();
    if( mapModell == null )
      return;

    final GeoTransform transform = getProjection();

    final Point mousePoint = e.getPoint();
    final double gx = transform.getSourceX( mousePoint.getX() );
    final double gy = transform.getSourceY( mousePoint.getY() );

    final CS_CoordinateSystem cs = mapModell.getCoordinatesSystem();
    final GM_Point gmPoint = GeometryFactory.createGM_Point( gx, gy, cs );

    final IMapPanelListener[] listeners = m_mapPanelListeners.toArray( new IMapPanelListener[] {} );
    for( final IMapPanelListener mpl : listeners )
      mpl.onMouseMoveEvent( this, gmPoint, mousePoint );
  }
}