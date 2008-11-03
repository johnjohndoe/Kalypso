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
import java.awt.Rectangle;
import java.awt.RenderingHints;
import java.awt.event.ComponentEvent;
import java.awt.event.ComponentListener;
import java.awt.image.BufferedImage;
import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.runtime.ISafeRunnable;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.SafeRunner;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.util.SafeRunnable;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.PlatformUI;
import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.core.KalypsoCoreDebug;
import org.kalypso.core.i18n.Messages;
import org.kalypso.ogc.gml.IKalypsoCascadingTheme;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.KalypsoCascadingThemeSelection;
import org.kalypso.ogc.gml.KalypsoFeatureThemeSelection;
import org.kalypso.ogc.gml.map.listeners.IMapPanelListener;
import org.kalypso.ogc.gml.map.listeners.IMapPanelPaintListener;
import org.kalypso.ogc.gml.mapmodel.IKalypsoThemeVisitor;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.ogc.gml.mapmodel.IMapModellListener;
import org.kalypso.ogc.gml.mapmodel.MapModellAdapter;
import org.kalypso.ogc.gml.mapmodel.MapModellHelper;
import org.kalypso.ogc.gml.mapmodel.visitor.KalypsoThemeChangeExtentVisitor;
import org.kalypso.ogc.gml.selection.IFeatureSelection;
import org.kalypso.ogc.gml.selection.IFeatureSelectionListener;
import org.kalypso.ogc.gml.selection.IFeatureSelectionManager;
import org.kalypso.ogc.gml.widgets.IWidgetManager;
import org.kalypso.ogc.gml.widgets.WidgetManager;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.graphics.transformation.WorldToScreenTransform;
import org.kalypsodeegree_impl.model.geometry.GM_Envelope_Impl;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

/**
 * @author Andreas von Dömming
 */
public class MapPanel extends Canvas implements ComponentListener, IMapPanel
{
  private static interface IListenerRunnable
  {
    public void visit( final IMapPanelListener l );
  }

  private static final long serialVersionUID = 1L;

  static
  {
    System.setProperty( "sun.awt.noerasebackground", "true" ); //$NON-NLS-1$ //$NON-NLS-2$
  }

  private final IFeatureSelectionManager m_selectionManager;

  private final List<ISelectionChangedListener> m_selectionListeners = new ArrayList<ISelectionChangedListener>( 5 );

  private final IFeatureSelectionListener m_globalSelectionListener = new IFeatureSelectionListener()
  {
    public void selectionChanged( final IFeatureSelection selection )
    {
      globalSelectionChanged();
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

  private final ExtentHistory m_extentHistory = new ExtentHistory( 200 );

  private Boolean m_shouldPaint = true;

  private String m_message = ""; //$NON-NLS-1$

  private final IMapModellListener m_modellListener = new MapModellAdapter()
  {
    /**
     * @see org.kalypso.ogc.gml.mapmodel.MapModellAdapter#repaintRequested(org.kalypso.ogc.gml.mapmodel.IMapModell,
     *      org.kalypsodeegree.model.geometry.GM_Envelope)
     */
    @Override
    public void repaintRequested( final IMapModell source, final GM_Envelope bbox )
    {
      invalidateMap();
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

        invalidateMap();
      }
    }

    /**
     * @see org.kalypso.ogc.gml.mapmodel.MapModellAdapter#themeOrderChanged(org.kalypso.ogc.gml.mapmodel.IMapModell)
     */
    @Override
    public void themeOrderChanged( final IMapModell source )
    {
      invalidateMap();
    }

    /**
     * @see org.kalypso.ogc.gml.mapmodel.MapModellAdapter#themeRemoved(org.kalypso.ogc.gml.mapmodel.IMapModell,
     *      org.kalypso.ogc.gml.IKalypsoTheme)
     */
    @Override
    public void themeRemoved( final IMapModell source, final IKalypsoTheme theme, final boolean lastVisibility )
    {
      if( lastVisibility )
        invalidateMap();
    }

    /**
     * @see org.kalypso.ogc.gml.mapmodel.MapModellAdapter#themeVisibilityChanged(org.kalypso.ogc.gml.mapmodel.IMapModell,
     *      org.kalypso.ogc.gml.IKalypsoTheme, boolean)
     */
    @Override
    public void themeVisibilityChanged( final IMapModell source, final IKalypsoTheme theme, final boolean visibility )
    {
      invalidateMap();
    }

  };

  private MapPanelPainter m_painter = new MapPanelPainter( this );

  private IStatus m_status = Status.OK_STATUS;

  public MapPanel( final ICommandTarget viewCommandTarget, final IFeatureSelectionManager manager )
  {
    m_selectionManager = manager;
    m_selectionManager.addSelectionListener( m_globalSelectionListener );

    m_widgetManager = new WidgetManager( viewCommandTarget, this );
    addMouseListener( m_widgetManager );
    addMouseMotionListener( m_widgetManager );
    addKeyListener( m_widgetManager );
    addComponentListener( this );
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
   * Add a listener in the mapPanel that will be notified in specific changes. <br/>
   * At the moment there is only the message changed event.
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
    final GM_Envelope bbox = m_wishBBox != null ? m_wishBBox : m_boundingBox;
    setBoundingBox( bbox, false );
  }

  /**
   * @see java.awt.event.ComponentListener#componentShown(java.awt.event.ComponentEvent)
   */
  public void componentShown( final ComponentEvent e )
  {
    final GM_Envelope bbox = m_wishBBox != null ? m_wishBBox : m_boundingBox;
    setBoundingBox( bbox, false );
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

    m_painter.dispose();

    // Also release references, as the ref to the MapPanel is never released (do to ICommand stuff and so on)
    m_painter = null;
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

  /**
   * Must be invoked, if the status of the mapPanel has changed.
   */
  private void fireStatusChanged( )
  {
    acceptListenersRunnable( new IListenerRunnable()
    {
      public void visit( final IMapPanelListener l )
      {
        l.onStatusChanged( MapPanel.this );
      }
    } );
  }

  public final void fireSelectionChanged( )
  {
    final ISelectionChangedListener[] listenersArray = m_selectionListeners.toArray( new ISelectionChangedListener[m_selectionListeners.size()] );

    final SelectionChangedEvent e = new SelectionChangedEvent( this, getSelection() );
    for( final ISelectionChangedListener l : listenersArray )
    {
      final Display display = PlatformUI.getWorkbench().getDisplay();
      display.asyncExec( new Runnable()
      {

        public void run( )
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
      } );
    }
  }

  public synchronized GM_Envelope getBoundingBox( )
  {
    return m_boundingBox;
  }

  /**
   * @see org.kalypso.ogc.gml.map.IMapPanel#getScreenBounds()
   */
  @Override
  public Rectangle getScreenBounds( )
  {
    return getBounds();
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

  public GeoTransform getProjection( )
  {
    return m_projection;
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

  public IWidgetManager getWidgetManager( )
  {
    return m_widgetManager;
  }

  protected void globalSelectionChanged( )
  {
    if( m_painter != null )
      m_painter.invalidate( true );

    // TODO: should be fired in the SWT thread, because the global selection listeners
    // need this
    fireSelectionChanged();
  }

  /**
   * Invalidates the whole map, all data is redrawn freshly.
   */
  public void invalidateMap( )
  {
    xOffset = 0;
    yOffset = 0;

    synchronized( this )
    {
      m_shouldPaint = false;

      if( m_model != null )
      {
        m_projection.setDestRect( 0, 0, getWidth(), getHeight(), getMapModell().getCoordinatesSystem() );

        // We should instead get a status from the model itself
        if( m_model.getThemeSize() == 0 )
          setStatus( StatusUtilities.createStatus( IStatus.INFO, Messages.getString( "org.kalypso.ogc.gml.map.MapPanel.21" ), null ) ); //$NON-NLS-1$
        else
          setStatus( Status.OK_STATUS );
      }

      if( m_painter != null )
        m_painter.invalidate( false );

      m_shouldPaint = true;
    }

    // we do not repaint here, as the painter trigger repaint events himself. Repainting here causes
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
   * This method was synchronised in order to fix bugs caused by threading issues concerning the setBoundBox method.
   * </p>
   * <p>
   * The bug was fixed by this, an so far no dead locks are encountered. see also {@link #getBoundingBox()}and
   * {@link #setBoundingBox(GM_Envelope)}
   * </p>
   * <p>
   * Make sure, that no call to one of the 'fire...' methods is made in the synchronised code.
   * </p>
   * 
   * @see java.awt.Component#paint(java.awt.Graphics)
   */
  @Override
  public void paint( final Graphics outerG )
  {
    if( !m_shouldPaint )
      return;

    final Graphics2D outerG2 = (Graphics2D) outerG;
    outerG2.setRenderingHint( RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON );
    outerG2.setRenderingHint( RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_ON );

    final int height = getHeight();
    final int width = getWidth();

    if( height == 0 || width == 0 )
      return;

    // update dimension
    if( height != m_height || width != m_width )
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

      if( m_painter != null )
        m_painter.paint( g );
      // TODO: at the moment, we paint the status just on top of the map, if we change this component to SWT, we should
      // show the statusComposite in a title bar, if the status is non-OK (with details button for a stack trace)
      paintStatus( g );

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
   * If a message is present, paint it and return true
   */
  private void paintStatus( final Graphics2D g )
  {
    if( m_status.isOK() )
      return;

    final String message = m_status.getMessage();

    final int stringWidth = g.getFontMetrics().stringWidth( message );

    final int width = getWidth();
    final int height = getHeight();

    g.setColor( Color.white );
    g.fillRect( 0, 0, width, height );
    g.setColor( Color.black );

    g.drawString( message, (width - stringWidth) / 2, height / 2 );
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
   * This function sets the bounding box to this map panel and all its themes.
   * 
   * @param wishBBox
   *          The new extent, will be adapted so it fits into the current size of the panel.
   */
  public synchronized void setBoundingBox( final GM_Envelope wishBBox )
  {
    setBoundingBox( wishBBox, true );
  }

  /**
   * This function sets the bounding box to this map panel and all its themes.
   * 
   * @param wishBBox
   *          The new extent, will be adapted so it fits into the current size of the panel.
   * @param useHistory
   *          If <code>true</code>, the last extend is put into the extend history.
   */
  public void setBoundingBox( final GM_Envelope wishBBox, final boolean useHistory )
  {
    /* The wished bounding box. */
    m_wishBBox = wishBBox;

    /* We do remember the wish-box here, this behaves more nicely if the size of the view changed meanwhile. */
    if( useHistory )
      m_extentHistory.push( m_wishBBox );

    /* Store the old extent */
    final GM_Envelope oldExtent = m_boundingBox;

    /* Adjust the new extent (using the wish bounding box). */
    final double ratio = MapPanelUtilities.getRatio( this );
    m_boundingBox = MapModellHelper.adjustBoundingBox( m_model, m_wishBBox, ratio );

    if( m_boundingBox != null )
    {
      /* Debug-Information. */
      if( KalypsoCoreDebug.MAP_PANEL.isEnabled() )
      {
        final StringBuffer dump = new StringBuffer();
        dump.append( "MinX:" + m_boundingBox.getMin().getX() );
        dump.append( "\nMinY:" + m_boundingBox.getMin().getY() );
        dump.append( "\nMaxX:" + m_boundingBox.getMax().getX() );
        dump.append( "\nMaxY:" + m_boundingBox.getMax().getY() );
        dump.append( "\n" );

        System.out.println( dump.toString() );
      }

      /* Alter the source rect of the projection. */
      m_projection.setSourceRect( m_boundingBox );

      /* Instead invalidate the map yourself. */
      m_shouldPaint = false;
      invalidateMap();
    }

    /* Tell the themes, that the extent has changed. */
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

      /* Change the extent for all themes. */
      m_model.accept( new KalypsoThemeChangeExtentVisitor( m_width, m_height, m_boundingBox ), IKalypsoThemeVisitor.DEPTH_INFINITE );
    }

    /* Tell everyone, that the extent has changed. */
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

    if( m_model == null )
      setStatus( StatusUtilities.createStatus( IStatus.INFO, Messages.getString( "org.kalypso.ogc.gml.map.MapPanel.20" ), null ) ); //$NON-NLS-1$
    else
    {
      m_model.addMapModelListener( m_modellListener );
      // Status will immediately set by the call to invalidateMao
    }

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

    repaintMap();
  }

  public void setSelection( final ISelection selection )
  {
    // should not be called!
    throw new UnsupportedOperationException();
  }

  @Override
  public void update( final Graphics g )
  {
    // do not clear background, it flickers even if we double buffer
    paint( g );
  }

  public void fireMouseMouveEvent( final int mousex, final int mousey )
  {
    final IMapModell mapModell = getMapModell();
    if( mapModell == null )
      return;

    final GeoTransform transform = getProjection();

    final double gx = transform.getSourceX( mousex );
    final double gy = transform.getSourceY( mousey );

    final String cs = mapModell.getCoordinatesSystem();
    final GM_Point gmPoint = GeometryFactory.createGM_Point( gx, gy, cs );

    final IMapPanelListener[] listeners = m_mapPanelListeners.toArray( new IMapPanelListener[] {} );
    for( final IMapPanelListener mpl : listeners )
      mpl.onMouseMoveEvent( this, gmPoint, mousex, mousey );
  }

  /**
   * Causes any pending paint to be stopped, and nothing will be painted until the next call to invalidateMap (called if
   * setBoundingBox is called).<br>
   * Fixes the ugly pan flicker bug (map gets drawn on old position after pan has been released).
   */
  public void stopPaint( )
  {
    m_shouldPaint = false;

    if( m_painter != null )
      m_painter.cancel();
  }

  public ExtentHistory getExtentHistory( )
  {
    return m_extentHistory;
  }

  public void setStatus( final IStatus status )
  {
    m_status = status;
    fireStatusChanged();
  }

  public IStatus getStatus( )
  {
    return m_status;
  }

  /**
   * @see java.awt.Component#repaint()
   */
  @Override
  public void repaintMap( )
  {
    super.repaint();
  }

  /**
   * @see org.kalypso.ogc.gml.map.IMapPanel#getMapImage()
   */
  @Override
  public BufferedImage getMapImage( )
  {
    if( m_painter == null )
      return null;

    return m_painter.getNormalImage();
  }

}