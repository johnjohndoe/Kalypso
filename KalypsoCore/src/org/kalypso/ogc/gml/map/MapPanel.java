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
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.core.runtime.ISafeRunnable;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.SafeRunner;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.ISchedulingRule;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.util.SafeRunnable;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.PlatformUI;
import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.core.runtime.jobs.MutexRule;
import org.kalypso.contribs.eclipse.jobs.BufferPaintJob;
import org.kalypso.contribs.eclipse.jobs.JobObserverJob;
import org.kalypso.contribs.eclipse.jobs.BufferPaintJob.IPaintable;
import org.kalypso.core.KalypsoCoreDebug;
import org.kalypso.core.i18n.Messages;
import org.kalypso.ogc.gml.IKalypsoCascadingTheme;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.KalypsoCascadingThemeSelection;
import org.kalypso.ogc.gml.KalypsoFeatureThemeSelection;
import org.kalypso.ogc.gml.map.layer.BufferedRescaleMapLayer;
import org.kalypso.ogc.gml.map.layer.NullMapLayer;
import org.kalypso.ogc.gml.map.layer.SelectionMapLayer;
import org.kalypso.ogc.gml.map.listeners.IMapPanelListener;
import org.kalypso.ogc.gml.map.listeners.IMapPanelPaintListener;
import org.kalypso.ogc.gml.mapmodel.IKalypsoThemeVisitor;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.ogc.gml.mapmodel.IMapModellListener;
import org.kalypso.ogc.gml.mapmodel.MapModellAdapter;
import org.kalypso.ogc.gml.mapmodel.MapModellHelper;
import org.kalypso.ogc.gml.selection.IFeatureSelection;
import org.kalypso.ogc.gml.selection.IFeatureSelectionListener;
import org.kalypso.ogc.gml.selection.IFeatureSelectionManager;
import org.kalypso.ogc.gml.widgets.IWidgetManager;
import org.kalypso.ogc.gml.widgets.WidgetManager;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.graphics.transformation.WorldToScreenTransform;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

/**
 * AWT canvas that displays a {@link org.kalypso.ogc.gml.mapmodel.MapModell}.
 *
 * @author Andreas von Dömming
 * @author Gernot Belger
 */
public class MapPanel extends Canvas implements ComponentListener, IMapPanel
{
// /**
// * Maximum delay by which repaints to the map are produced.
// *
// * @see java.awt.Component#repaint(long)
// */
// private static final long MAP_REPAINT_MILLIS = 500;

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

  private IMapModell m_model = null;

  private final WidgetManager m_widgetManager;

  protected GM_Envelope m_boundingBox = null;

  private GM_Envelope m_wishBBox;

  private final List<IMapPanelListener> m_mapPanelListeners = new ArrayList<IMapPanelListener>();

  private final List<IMapPanelPaintListener> m_paintListeners = new ArrayList<IMapPanelPaintListener>();

  private final Map<IKalypsoTheme, IMapLayer> m_layers = new HashMap<IKalypsoTheme, IMapLayer>();

  private final ExtentHistory m_extentHistory = new ExtentHistory( 200 );

  private String m_message = ""; //$NON-NLS-1$

  // TODO: fetch from map-modell (aka from .gmt file)
  private final Color m_backgroundColor = new Color( 255, 255, 255 );

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
        invalidateMap();
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
      handleThemeRemoved( theme, lastVisibility );
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

    /**
     * @see org.kalypso.ogc.gml.mapmodel.MapModellAdapter#themeStatusChanged(org.kalypso.ogc.gml.mapmodel.IMapModell,
     *      org.kalypso.ogc.gml.IKalypsoTheme)
     */
    @Override
    public void themeStatusChanged( final IMapModell source, final IKalypsoTheme theme )
    {
      invalidateMap();
    }
  };

  private BufferPaintJob m_bufferPaintJob = null;

  /** One mutex-rule per panel, so painting jobs for one panel run one after another. */
  private final ISchedulingRule m_painterMutex = new MutexRule();

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
    // Anwer: From the map-commands!
    m_selectionListeners.clear();
    m_mapPanelListeners.clear();
    m_paintListeners.clear();

    if( m_bufferPaintJob != null )
    {
      m_bufferPaintJob.dispose();
      m_bufferPaintJob = null;
    }
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
              l.selectionChanged( e );
            }
          };

          SafeRunnable.run( safeRunnable );
        }
      } );
    }
  }

  public GM_Envelope getBoundingBox( )
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
    final GeoTransform projection = getProjection();
    if( projection == null )
      return Double.NaN;

    return projection.getScale();
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
    invalidateMap();

    // TODO: should be fired in the SWT thread, because the global selection listeners
    // need this
    fireSelectionChanged();
  }

  /**
   * Invalidates the whole map, all data is redrawn freshly.<br>
   * Should not be invoked from outside; normally every theme invalidates itself, if its data is changed; if not check,
   * if all events are correctly sent.<br>
   * Important: does not invalidate the theme's buffers, so this will in most cases not do what you want.. please always
   * invalidate the theme by correctly firing gml-events
   */
  public void invalidateMap( )
  {
    if( m_model != null )
    {
      // We should instead get a status from the model itself
      if( m_model.getThemeSize() == 0 )
        setStatus( StatusUtilities.createStatus( IStatus.INFO, Messages.getString( "org.kalypso.ogc.gml.map.MapPanel.21" ), null ) ); //$NON-NLS-1$
      else
        setStatus( Status.OK_STATUS );
    }

    final IMapModell mapModell = getMapModell();
    if( mapModell == null )
      return;

    final IMapLayer[] layers = getLayersForRendering();
    final IPaintable paintable = new MapPanelPainter( layers, mapModell, getProjection(), m_backgroundColor );

    final BufferPaintJob bufferPaintJob = new BufferPaintJob( paintable );
    bufferPaintJob.setRule( m_painterMutex );
    bufferPaintJob.setPriority( Job.SHORT );
    bufferPaintJob.setUser( false );

    final JobObserverJob repaintJob = new JobObserverJob( "Repaint map observer", bufferPaintJob, 5000 )
    {
      @Override
      protected void jobRunning( )
      {
        MapPanel.this.repaintMap();
      }
    };
    repaintJob.setSystem( true );
    repaintJob.schedule();

    /* Cancel old job if still running. */
    synchronized( this )
    {
      if( m_bufferPaintJob != null )
      {
        m_bufferPaintJob.dispose();
        m_bufferPaintJob = null;
      }

      m_bufferPaintJob = bufferPaintJob;
      // delay the Schedule, so if another invalidate comes within that time-span, no repaint happens at all
      m_bufferPaintJob.schedule( 100 );
    }

    repaintMap();
  }

  /**
   * Paints contents of the map ni the following order:
   * <ul>
   * <li>the buffered image containing the layers</li>
   * <li>the status, if not OK</li>
   * <li>all 'paint-listeners'</li>
   * <li>the current widget</li>
   * </ul>
   *
   * @see java.awt.Component#paint(java.awt.Graphics)
   */
  @Override
  public void paint( final Graphics g )
  {
    final int width = getWidth();
    final int height = getHeight();
    if( height == 0 || width == 0 )
      return;

    final BufferedImage buffer = new BufferedImage( width, height, BufferedImage.TYPE_INT_ARGB );
    Graphics2D bufferGraphics = null;
    try
    {
      bufferGraphics = buffer.createGraphics();
      bufferGraphics.setRenderingHint( RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON );
      bufferGraphics.setRenderingHint( RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_ON );

      final BufferPaintJob bufferPaintJob = m_bufferPaintJob; // get copy (for thread safety)
      final BufferedImage image = bufferPaintJob == null ? null : bufferPaintJob.getImage();
      if( image != null )
      {
        final MapPanelPainter paintable = (MapPanelPainter) bufferPaintJob.getPaintable();
        final GeoTransform world2screen = paintable.getWorld2screen();
        final GM_Envelope imageBounds = world2screen.getSourceRect();
        if( imageBounds.equals( m_boundingBox ) )
          bufferGraphics.drawImage( image, 0, 0, null );
        else
        {
          // If current buffer only shows part of the map, paint it into the right screen-rect
          final GeoTransform currentProjection = getProjection();
          if( currentProjection != null )
            MapPanelUtilities.paintIntoExtent( bufferGraphics, currentProjection, image, imageBounds, m_backgroundColor );
        }
      }

      // TODO: at the moment, we paint the status just on top of the map, if we change this component to SWT, we should
      // show the statusComposite in a title bar, if the status is non-OK (with details button for a stack trace)
      paintStatus( bufferGraphics );

      final IMapPanelPaintListener[] pls = m_paintListeners.toArray( new IMapPanelPaintListener[] {} );
      for( final IMapPanelPaintListener pl : pls )
        pl.paint( bufferGraphics );

      paintWidget( bufferGraphics );
    }
    finally
    {
      if( bufferGraphics != null )
        bufferGraphics.dispose();
    }

    g.drawImage( buffer, 0, 0, null );
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

    g.setColor( m_backgroundColor );
    g.fillRect( 0, 0, width, height );
    g.setColor( Color.black );

    g.drawString( message, (width - stringWidth) / 2, height / 2 );
  }

  /**
   * Lets the active widget paint itself.
   */
  private void paintWidget( final Graphics g )
  {
    // TODO: either reset the GC completely or do not set anything at all
    g.setColor( Color.RED );
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
   * @see IMapPanel#setBoundingBox(GM_Envelope, boolean, boolean)
   */
  public void setBoundingBox( final GM_Envelope wishBBox, final boolean useHistory )
  {
    setBoundingBox( wishBBox, true, true );
  }

  /**
   * @see IMapPanel#setBoundingBox(GM_Envelope, boolean, boolean)
   */
  public void setBoundingBox( final GM_Envelope wishBBox, final boolean useHistory, final boolean invalidateMap )
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
    final GM_Envelope boundingBox = MapModellHelper.adjustBoundingBox( m_model, m_wishBBox, ratio );
    m_boundingBox = boundingBox;

    if( boundingBox != null )
    {
      KalypsoCoreDebug.MAP_PANEL.printf( "MinX: %d%n", boundingBox.getMin().getX() );
      KalypsoCoreDebug.MAP_PANEL.printf( "MinY: %d%n", boundingBox.getMin().getY() );
      KalypsoCoreDebug.MAP_PANEL.printf( "MaxX: %d%n", boundingBox.getMax().getX() );
      KalypsoCoreDebug.MAP_PANEL.printf( "MaxY: %d%n", boundingBox.getMax().getY() );

      if( invalidateMap )
        invalidateMap();
      else
        repaintMap();
    }

    /* Tell everyone, that the extent has changed. */
    if( invalidateMap )
      fireExtentChanged( oldExtent, boundingBox );
  }

  /**
   * @see org.kalypso.ogc.gml.mapmodel.IMapModellView#setMapModell(org.kalypso.ogc.gml.mapmodel.IMapModell)
   */
  public void setMapModell( final IMapModell modell )
  {
    final IMapModell oldModel = m_model;

    if( m_model != null )
    {
      m_model.removeMapModelListener( m_modellListener );

      for( final IMapLayer layer : m_layers.values() )
        layer.dispose();
    }

    m_model = modell;

    if( m_model == null )
      setStatus( StatusUtilities.createStatus( IStatus.INFO, Messages.getString( "org.kalypso.ogc.gml.map.MapPanel.20" ), null ) ); //$NON-NLS-1$
    else
    {
      m_model.addMapModelListener( m_modellListener );
      invalidateMap();
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
    if( transform == null )
      return;

    final double gx = transform.getSourceX( mousex );
    final double gy = transform.getSourceY( mousey );

    final String cs = mapModell.getCoordinatesSystem();
    final GM_Point gmPoint = GeometryFactory.createGM_Point( gx, gy, cs );

    final IMapPanelListener[] listeners = m_mapPanelListeners.toArray( new IMapPanelListener[] {} );
    for( final IMapPanelListener mpl : listeners )
      mpl.onMouseMoveEvent( this, gmPoint, mousex, mousey );
  }

  public GeoTransform getProjection( )
  {
    final GM_Envelope boundingBox = m_boundingBox;
    if( boundingBox == null )
      return null;

    final GeoTransform projection = new WorldToScreenTransform();
    projection.setSourceRect( boundingBox );
    final int width = getWidth();
    final int height = getHeight();
    projection.setDestRect( 0, 0, width, height, null );

    return projection;
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
    repaint( 50 );
  }

  /**
   * @see org.kalypso.ogc.gml.map.IMapPanel#getMapImage()
   */
  @Override
  public BufferedImage getMapImage( )
  {
    final BufferPaintJob bufferPaintJob = m_bufferPaintJob; // get copy for thread safety
    if( bufferPaintJob == null )
      return null;

    return bufferPaintJob.getImage();
  }

  /**
   * Create the list of layers in the order it should be rendered.
   */
  protected IMapLayer[] getLayersForRendering( )
  {
    final List<IMapLayer> result = new ArrayList<IMapLayer>( 20 );
    final List<IKalypsoFeatureTheme> visibleFestureThemes = new ArrayList<IKalypsoFeatureTheme>( 10 );

    final IKalypsoThemeVisitor createLayerVisitor = new IKalypsoThemeVisitor()
    {
      @Override
      public boolean visit( final IKalypsoTheme theme )
      {
        if( theme.isVisible() )
        {
          final IMapLayer layer = getLayer( theme );
          result.add( layer );

          if( theme instanceof IKalypsoFeatureTheme )
            visibleFestureThemes.add( (IKalypsoFeatureTheme) theme );

          return true;
        }

        // No sense in descending into invisible cascading-themes
        return false;
      }
    };

    m_model.accept( createLayerVisitor, IKalypsoThemeVisitor.DEPTH_INFINITE );

    // Reverse list, last should be rendered first.
    Collections.reverse( result );

    final IKalypsoFeatureTheme[] selectionThemes = visibleFestureThemes.toArray( new IKalypsoFeatureTheme[visibleFestureThemes.size()] );
    // TODO: care for disposal of SelectionMapLayer
    result.add( new SelectionMapLayer( this, selectionThemes ) );

    return result.toArray( new IMapLayer[result.size()] );
  }

  protected IMapLayer getLayer( final IKalypsoTheme theme )
  {
    final IMapLayer existingLayer = m_layers.get( theme );
    if( existingLayer == null )
    {
      // TODO: move into factory method; there should be an extension-point...
      final IMapLayer newLayer;
      if( theme instanceof IKalypsoCascadingTheme )
        newLayer = new NullMapLayer( this, theme );
      else if( theme.getClass().getName().endsWith( "KalypsoWMSTheme" ) )
      {
        // REMARK: uncomment to change to different rendering strategy
        newLayer = new BufferedRescaleMapLayer( this, theme, false );
      }
      else
      {
        // REMARK: uncomment to change to different rendering strategy. I like
        // 'BufferedRescale' best...
        // newLayer = new DirectMapLayer( this, theme );
        // newLayer = new BufferedMapLayer( this, theme );
        newLayer = new BufferedRescaleMapLayer( this, theme, true );
      }

      m_layers.put( theme, newLayer );
      return newLayer;
    }

    return existingLayer;
  }

  protected void handleThemeRemoved( final IKalypsoTheme theme, final boolean lastVisibility )
  {
    final IMapLayer layer = m_layers.remove( theme );
    if( layer != null )
      layer.dispose();

    if( lastVisibility )
      invalidateMap();
  }
}