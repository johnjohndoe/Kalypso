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
import java.awt.Image;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.event.ComponentEvent;
import java.awt.event.ComponentListener;
import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.runtime.Platform;
import org.eclipse.jface.util.SafeRunnable;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.ISelectionProvider;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.KalypsoFeatureThemeSelection;
import org.kalypso.ogc.gml.command.JMSelector;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.ogc.gml.mapmodel.IMapModellView;
import org.kalypso.ogc.gml.mapmodel.MapModell;
import org.kalypso.ogc.gml.mapmodel.MapModellHelper;
import org.kalypso.ogc.gml.selection.EasyFeatureWrapper;
import org.kalypso.ogc.gml.selection.IFeatureSelection;
import org.kalypso.ogc.gml.selection.IFeatureSelectionListener;
import org.kalypso.ogc.gml.selection.IFeatureSelectionManager;
import org.kalypso.ogc.gml.widgets.WidgetManager;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.event.ModellEvent;
import org.kalypsodeegree.model.feature.event.ModellEventListener;
import org.kalypsodeegree.model.feature.event.ModellEventProvider;
import org.kalypsodeegree.model.feature.event.ModellEventProviderAdapter;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.graphics.transformation.WorldToScreenTransform;
import org.kalypsodeegree_impl.model.geometry.GM_Envelope_Impl;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * @author vdoemming
 */
public class MapPanel extends Canvas implements IMapModellView, ComponentListener, ModellEventProvider,
    ISelectionProvider
{
  public static final int MODE_SELECT = 0;

  public static final int MODE_TOGGLE = 1;

  public static final int MODE_UNSELECT = 2;

  private final IFeatureSelectionManager m_selectionManager;

  private final List m_selectionListeners = new ArrayList( 5 );

  private final IFeatureSelectionListener m_globalSelectionListener = new IFeatureSelectionListener()
  {
    public void selectionChanged( final IFeatureSelection selection )
    {
      globalSelectionChanged( selection );
    }
  };

  private final ModellEventProvider m_modellEventProvider = new ModellEventProviderAdapter();

  private static final long serialVersionUID = 1L;

  public final static String WIDGET_ZOOM_IN = "ZOOM_IN";

  public final static String WIDGET_ZOOM_IN_RECT = "ZOOM_IN_RECT";

  public final static String WIDGET_PAN = "PAN";

  public final static String WIDGET_EDIT_FEATURE = "EDIT_FEATURE";

  public final static String WIDGET_SELECT = "SELECT";

  public final static String WIDGET_UNSELECT = "UNSELECT";

  public final static String WIDGET_TOGGLE_SELECT = "TOGGLE_SELECT";

  public final static String WIDGET_CREATE_FEATURE = "CREATE_FEATURE";

  public static final String WIDGET_SINGLE_SELECT = "SINGLE_SELECT";

  private Image m_mapImage = null;

  private int xOffset = 0;

  private int yOffset = 0;

  private int m_width = 0;

  private int m_height = 0;

  private boolean validMap = false;

  private IMapModell m_model = null;

  private final WidgetManager m_widgetManager;

  private final GeoTransform m_projection = new WorldToScreenTransform();

  private GM_Envelope m_boundingBox = new GM_Envelope_Impl();

  private GM_Envelope m_wishBBox;

  public MapPanel( final ICommandTarget viewCommandTarget, final CS_CoordinateSystem crs,
      final IFeatureSelectionManager manager )
  {
    m_selectionManager = manager;
    m_selectionManager.addSelectionListener( m_globalSelectionListener );

    // set empty Modell:
    setMapModell( new MapModell( crs, null ) );
    m_widgetManager = new WidgetManager( viewCommandTarget, this );
    addMouseListener( m_widgetManager );
    addMouseMotionListener( m_widgetManager );
    addComponentListener( this );
    setVisible( true );
  }

  public void dispose()
  {
    removeMouseListener( m_widgetManager );
    removeMouseMotionListener( m_widgetManager );

    m_selectionManager.removeSelectionListener( m_globalSelectionListener );

    if( m_model != null )
      m_model.removeModellListener( this );
  }

  public void setOffset( int dx, int dy ) // used by pan method
  {
    xOffset = dx;
    yOffset = dy;

    repaint();
  }

  public void clearOffset() // used by pan method
  {
    xOffset = 0;
    yOffset = 0;
    repaint();
  }

  /**
   * @see java.awt.Component#paint(java.awt.Graphics)
   */
  public void paint( Graphics g )
  {
    paintMap( g );
    paintWidget( g );
  }

  private void paintMap( final Graphics g )
  {
    // to avoid threading issues, get reference once
    final IMapModell model = m_model;

    if( model == null || model.getThemeSize() == 0 ) // no maps ...
    {
      g.setColor( Color.white );
      g.fillRect( 0, 0, getWidth(), getHeight() );
      g.setColor( Color.black );
      g.drawString( "Kalypso", getWidth() / 2, getHeight() / 2 );
      return;
    }

    if( getHeight() == 0 || getWidth() == 0 )
      return;

    if( getHeight() != m_height || getWidth() != m_width )
    { // update dimension
      m_height = getHeight();
      m_width = getWidth();
      // setBoundingBox( getBoundingBox() );
      // setValidAll( false );
    }

    if( !hasValidMap() || m_mapImage == null )
    {
      final Rectangle clipBounds = g.getClipBounds();
      if( clipBounds != null )
      {
        m_mapImage = MapModellHelper.createImageFromModell( getProjection(), getBoundingBox(), clipBounds, getWidth(),
            getHeight(), model );
        setValidMap( m_mapImage != null );
      }
    }

    // paint selection ?
    /*
     * if( !hasValidSelection() ) { selectionImage = new BufferedImage( getWidth(), getHeight(),
     * BufferedImage.TYPE_INT_ARGB ); Graphics gr = selectionImage.getGraphics(); gr.setClip( 0, 0, getWidth(),
     * getHeight() ); try { setValidSelection( true ); //myMapView.paintSelected( gr ); } catch( Exception e ) {
     * e.printStackTrace(); } gr.dispose(); } // paint highlights ? if( !hasValidHighlight() ) { highlightImage = new
     * BufferedImage( getWidth(), getHeight(), BufferedImage.TYPE_INT_ARGB ); Graphics gr =
     * highlightImage.getGraphics(); gr.setClip( 0, 0, getWidth(), getHeight() ); try { setValidHighlight( true );
     * //myMapView.paintHighlighted( gr ); } catch( Exception e ) { e.printStackTrace(); } gr.dispose(); }
     */
    if( xOffset != 0 && yOffset != 0 ) // to clear backround ...
    {
      final int left = Math.max( 0, xOffset );
      final int right = Math.min( getWidth(), xOffset + getWidth() );
      final int top = Math.max( 0, yOffset );
      final int bottom = Math.min( getHeight(), yOffset + getHeight() );

      g.setColor( getBackground() );
      // g.setColor( Color.black );

      g.fillRect( 0, 0, left, getHeight() ); // left
      g.fillRect( left, 0, right - left, top ); // top
      g.fillRect( left, bottom, right - left, getHeight() - bottom ); // bottom
      g.fillRect( right, 0, getWidth() - right, getHeight() ); // right
    }

    // draw map:
    g.drawImage( m_mapImage, xOffset, yOffset, null );
    // draw selection:
    //     g.setXORMode( Color.red );
    //     g.drawImage( selectionImage, xOffset, yOffset, null );
    // draw highlights:
    // g.setXORMode( Color.green );
    // g.drawImage( highlightImage, xOffset, yOffset, null );
    g.setPaintMode();
  }

  public void update( Graphics g )
  {
    paint( g );
  }

  private void paintWidget( Graphics g )
  {
    g.setColor( Color.red );
    g.setClip( 0, 0, getWidth(), getHeight() );
    m_widgetManager.paintWidget( g );
  }

  public void setValidMap( boolean status )
  {
    validMap = status;
  }

  private boolean hasValidMap()
  {
    return validMap;
  }

  private void setValidAll( boolean status )
  {
    setValidMap( status );
  }

  /**
   * @see org.kalypso.ogc.gml.mapmodel.IMapModellView#getMapModell()
   */
  public IMapModell getMapModell()
  {
    return m_model;
  }

  /**
   * @see org.kalypso.ogc.gml.mapmodel.IMapModellView#setMapModell(org.kalypso.ogc.gml.mapmodel.IMapModell)
   */
  public void setMapModell( final IMapModell modell )
  {
    if( m_model != null )
      m_model.removeModellListener( this );

    m_model = modell;

    if( m_model != null )
      m_model.addModellListener( this );
  }

  /**
   * @see org.kalypsodeegree.model.feature.event.ModellEventListener#onModellChange(org.kalypsodeegree.model.feature.event.ModellEvent)
   */
  public void onModellChange( final ModellEvent modellEvent )
  {
    setValidAll( false );
    clearOffset();
    // inform my listeners
    fireModellEvent( modellEvent );
  }

  public GM_Envelope getPanToPixelBoundingBox( double mx, double my )
  {
    double ratio = m_height / m_width;

    final GeoTransform transform = getProjection();

    double gisMX = transform.getSourceX( mx );
    double gisMY = transform.getSourceY( my );

    double gisDX = ( transform.getSourceX( m_width / 2 ) - transform.getSourceX( 0 ) );
    double gisDY = gisDX * ratio;
    double gisX1 = gisMX - gisDX;
    double gisX2 = gisMX + gisDX;
    double gisY1 = gisMY - gisDY;
    double gisY2 = gisMY + gisDY;

    return GeometryFactory.createGM_Envelope( gisX1, gisY1, gisX2, gisY2 );
  }

  /**
   * calculates the current map scale (denominator) as defined in the OGC SLD 1.0.0 specification
   * 
   * @return scale of the map
   */
  public double getCurrentScale()
  {
    return MapModellHelper.calcScale( m_model, getBoundingBox(), getWidth(), getHeight() );
  }

  public GeoTransform getProjection()
  {
    return m_projection;
  }

  public GM_Envelope getBoundingBox()
  {
    return m_boundingBox;
  }

  public void setBoundingBox( final GM_Envelope wishBBox )
  {
    m_wishBBox = wishBBox;
    m_boundingBox = adjustBoundingBox( m_wishBBox );
    if( m_boundingBox == null )
      return;
    
    m_projection.setSourceRect( m_boundingBox );
    /*
     * if( m_model != null ) { if( m_model.getCoordinatesSystem() != null ) m_projection.setSourceCS(
     * m_model.getCoordinatesSystem() ); }
     */
    // redraw
    onModellChange( null );
    
    repaint();
  }

  private GM_Envelope adjustBoundingBox( GM_Envelope env )
  {
    if( env == null )
      env = m_model.getFullExtentBoundingBox();
    if( env == null )
      return null;

    double ratio = getRatio();
    // todo besser loesen
    if( Double.isNaN( ratio ) )
      return env;

    double minX = env.getMin().getX();
    double minY = env.getMin().getY();

    double maxX = env.getMax().getX();
    double maxY = env.getMax().getY();

    double dx = ( maxX - minX ) / 2d;
    double dy = ( maxY - minY ) / 2d;

    if( dx * ratio > dy )
      dy = dx * ratio;
    else
      dx = dy / ratio;

    double mx = ( maxX + minX ) / 2d;
    double my = ( maxY + minY ) / 2d;

    return GeometryFactory.createGM_Envelope( mx - dx, my - dy, mx + dx, my + dy );
  }

  private double getRatio()
  {
    return ( (double)getHeight() ) / ( (double)getWidth() );
  }

  public GM_Envelope getZoomOutBoundingBox()
  {
    GeoTransform transform = getProjection();
    double ratio = getRatio();
    double gisMX = transform.getSourceX( getWidth() / 2d );
    double gisMY = transform.getSourceY( getHeight() / 2d );

    double gisDX = 2 * ( gisMX - transform.getSourceX( 0 ) );
    double gisDY = gisDX * ratio;
    double gisX1 = gisMX - gisDX;
    double gisX2 = gisMX + gisDX;
    double gisY1 = gisMY - gisDY;
    double gisY2 = gisMY + gisDY;

    return GeometryFactory.createGM_Envelope( gisX1, gisY1, gisX2, gisY2 );
  }

  public WidgetManager getWidgetManager()
  {
    return m_widgetManager;
  }

  /**
   * @see java.awt.event.ComponentListener#componentHidden(java.awt.event.ComponentEvent)
   */
  public void componentHidden( ComponentEvent e )
  {
  //  
  }

  /**
   * @see java.awt.event.ComponentListener#componentMoved(java.awt.event.ComponentEvent)
   */
  public void componentMoved( ComponentEvent e )
  {
  //  
  }

  /**
   * @see java.awt.event.ComponentListener#componentResized(java.awt.event.ComponentEvent)
   */
  public void componentResized( ComponentEvent e )
  {
    if( m_wishBBox != null )
      setBoundingBox( m_wishBBox );
    else
      setBoundingBox( getBoundingBox() );
  }

  /**
   * @see java.awt.event.ComponentListener#componentShown(java.awt.event.ComponentEvent)
   */
  public void componentShown( ComponentEvent e )
  {
    if( m_wishBBox != null )
      setBoundingBox( m_wishBBox );
    else
      setBoundingBox( getBoundingBox() );
  }

  public void addModellListener( ModellEventListener listener )
  {
    m_modellEventProvider.addModellListener( listener );
  }

  public void fireModellEvent( ModellEvent event )
  {
    m_modellEventProvider.fireModellEvent( event );
  }

  public void removeModellListener( ModellEventListener listener )
  {
    m_modellEventProvider.removeModellListener( listener );
  }

  public void addSelectionChangedListener( final ISelectionChangedListener listener )
  {
    m_selectionListeners.add( listener );
  }

  public void removeSelectionChangedListener( final ISelectionChangedListener listener )
  {
    m_selectionListeners.remove( listener );
  }

  public void setSelection( final ISelection selection )
  {
    // should not be called!
    throw new UnsupportedOperationException();
  }

  /**
   * @see org.eclipse.jface.viewers.ISelectionProvider#getSelection()
   */
  public ISelection getSelection()
  {
    final IMapModell mapModell = getMapModell();
    if( mapModell == null )
      return StructuredSelection.EMPTY;

    final IKalypsoTheme activeTheme = mapModell.getActiveTheme();
    if( !( activeTheme instanceof IKalypsoFeatureTheme ) )
      return StructuredSelection.EMPTY;

    return new KalypsoFeatureThemeSelection( m_selectionManager.toList(), (IKalypsoFeatureTheme)activeTheme,
        m_selectionManager, null, null );
  }

  protected void globalSelectionChanged( final IFeatureSelection selection )
  {
    if( selection != null )
    {
      // TODO: only repaint, if selection contains features contained in my themes changes
    }

    setValidAll( false );
    repaint();
    
    fireSelectionChanged();
  }

  public void select( final Point startPoint, final Point endPoint, final int radius, final int selectionMode,
      final boolean useOnlyFirstChoosen )
  {
    final GeoTransform transform = getProjection();

    final IKalypsoTheme activeTheme = m_model.getActiveTheme();
    if( activeTheme == null || !( activeTheme instanceof IKalypsoFeatureTheme ) )
      return;

    if( startPoint != null )
    {
      double g1x = transform.getSourceX( startPoint.getX() );
      double g1y = transform.getSourceY( startPoint.getY() );

      if( endPoint == null ) // not dragged
      {
        // TODO depend on featuretype
        // line and point with radius
        // polygon without radius
        double gisRadius = Math.abs( transform.getSourceX( startPoint.getX() + radius ) - g1x );

        final JMSelector selector = new JMSelector();

        final GM_Point pointSelect = GeometryFactory.createGM_Point( g1x, g1y, getMapModell().getCoordinatesSystem() );

        final Feature fe = selector.selectNearest( pointSelect, gisRadius, ( (IKalypsoFeatureTheme)activeTheme )
            .getFeatureListVisible( null ), false );

        final List listFe = new ArrayList();
        if( fe != null )
          listFe.add( fe );

        changeSelection( listFe, (IKalypsoFeatureTheme)activeTheme, m_selectionManager, selectionMode );
      }
      else
      // dragged
      {
        final double g2x = transform.getSourceX( endPoint.getX() );
        final double g2y = transform.getSourceY( endPoint.getY() );
        boolean withinStatus = false;

        if( endPoint.getX() > startPoint.getX() && endPoint.getY() > startPoint.getY() )
          withinStatus = true;

        double minX = g1x < g2x ? g1x : g2x;
        double maxX = g1x > g2x ? g1x : g2x;
        double minY = g1y < g2y ? g1y : g2y;
        double maxY = g1y > g2y ? g1y : g2y;

        if( minX != maxX && minY != maxY )
        {
          final JMSelector selector = new JMSelector();
          final GM_Envelope envSelect = GeometryFactory.createGM_Envelope( minX, minY, maxX, maxY );
          final List features = selector.select( envSelect, ( (IKalypsoFeatureTheme)activeTheme )
              .getFeatureListVisible( null ), withinStatus );

          if( useOnlyFirstChoosen && !features.isEmpty() )
          {
            // delete all but first if we shall only the first selected
            final Object object = features.get( 0 );
            features.clear();
            features.add( object );
          }

          changeSelection( features, (IKalypsoFeatureTheme)activeTheme, m_selectionManager, selectionMode );
        }
      }
    }
  }

  private void changeSelection( final List features, final IKalypsoFeatureTheme theme,
      final IFeatureSelectionManager selectionManager2, final int selectionMode )
  {
    // nothing was choosen by the user, dont do anything
    // TODO: maybe clear selection?
    if( features.isEmpty() )
    {
      // TODO: this should to the widget-manager?
      repaint();
      return;
    }

    // remove all selected features from this theme
    // TODO: maybe only visible??
    final FeatureList featureList = theme.getFeatureList();
    final Feature parentFeature = featureList.getParentFeature();
    final String parentProperty = featureList.getParentFeatureTypeProperty().getName();

    // add all selectied features
    final EasyFeatureWrapper[] selectedWrapped = new EasyFeatureWrapper[features.size()];
    for( int i = 0; i < features.size(); i++ )
    {
      final Feature f = (Feature)features.get( i );
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

    case MODE_SELECT: // selectert genau das, was ausgew�hlt wurde
      toRemove = featureList.toFeatures();
      toAdd = selectedWrapped;
      break;

    case MODE_UNSELECT: // l�scht alles augew�hlte aus der selection
      toRemove = featureList.toFeatures();
      toAdd = new EasyFeatureWrapper[0];

    default:
      throw new UnsupportedOperationException( "Unknown selection mode: " + selectionMode );
    }

    selectionManager2.changeSelection( toRemove, toAdd );
  }

  private final void fireSelectionChanged()
  {
    final ISelectionChangedListener[] listenersArray = (ISelectionChangedListener[])m_selectionListeners
        .toArray( new ISelectionChangedListener[m_selectionListeners.size()] );

    final SelectionChangedEvent e = new SelectionChangedEvent( this, getSelection() );
    for( int i = 0; i < listenersArray.length; i++ )
    {
      final ISelectionChangedListener l = listenersArray[i];
      final SafeRunnable safeRunnable = new SafeRunnable()
      {
        public void run()
        {
          l.selectionChanged( e );
        }
      };

      Platform.run( safeRunnable );
    }
  }

}