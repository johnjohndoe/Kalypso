/*----------------    FILE HEADER  ------------------------------------------
 
 This file is part of deegree.
 Copyright (C) 2001 by:
 EXSE, Department of Geography, University of Bonn
 http://www.giub.uni-bonn.de/exse/
 lat/lon Fitzke/Fretter/Poth GbR
 http://www.lat-lon.de
 
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
 
 Andreas Poth
 lat/lon Fitzke/Fretter/Poth GbR
 Meckenheimer Allee 176
 53115 Bonn
 Germany
 E-Mail: poth@lat-lon.de
 
 Jens Fitzke
 Department of Geography
 University of Bonn
 Meckenheimer Allee 166
 53115 Bonn
 Germany
 E-Mail: jens.fitzke@uni-bonn.de
 
 
 ---------------------------------------------------------------------------*/
package org.deegree_impl.graphics;

import java.awt.Graphics;
import java.awt.Graphics2D;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;

import org.deegree.graphics.Highlighter;
import org.deegree.graphics.Layer;
import org.deegree.graphics.MapEventController;
import org.deegree.graphics.MapView;
import org.deegree.graphics.RenderException;
import org.deegree.graphics.Selector;
import org.deegree.graphics.Theme;
import org.deegree.graphics.optimizers.Optimizer;
import org.deegree.graphics.transformation.GeoTransform;
import org.deegree.model.geometry.GM_Envelope;
import org.deegree.model.geometry.GM_Position;
import org.deegree_impl.graphics.optimizers.AbstractOptimizer;
import org.deegree_impl.graphics.transformation.WorldToScreenTransform;
import org.deegree_impl.model.cs.ConvenienceCSFactory;
import org.deegree_impl.model.cs.CoordinateSystem;
import org.deegree_impl.model.ct.GeoTransformer;
import org.deegree_impl.model.geometry.GeometryFactory;
import org.deegree_impl.tools.Debug;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * This interface describes the data modell of the map it self. It is build from
 * themes containing DisplayElements to be rendered. Themes can be added and
 * removed. Existing themes can be re-arragned by changing their order.
 * 
 * <p>
 * ------------------------------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @author <a href="mailto:mschneider@lat-lon.de">Markus Schneider </a>
 * @version $Revision$ $Date$
 */
class MapView_Impl implements MapView
{

  private String name = null;

  private HashMap themes = null;

  private HashMap enabled = null;

  private ArrayList themesL = null;

  private Theme activatedTh = null;

  private GM_Envelope boundingbox = null;

  private CS_CoordinateSystem crs = null;

  private List eventCntr = Collections.synchronizedList( new ArrayList() );

  private double scale;

  private GeoTransform projection = new WorldToScreenTransform();

  // list of Optimizers that are processed at the beginning of the paint ()-call
  private ArrayList optimizers = new ArrayList();

  MapView_Impl( String name, GM_Envelope boundingbox )
  {
    this.name = name;
    themes = new HashMap();
    themesL = new ArrayList();
    enabled = new HashMap();
    setBoundingBox( boundingbox );
    CoordinateSystem cs = ConvenienceCSFactory.getInstance().getCSByName( "EPSG:4326" );
    crs = org.deegree_impl.model.cs.Adapters.getDefault().export( cs );
  }

  MapView_Impl( String name, GM_Envelope boundingbox, CS_CoordinateSystem crs )
  {
    this.name = name;
    themes = new HashMap();
    themesL = new ArrayList();
    enabled = new HashMap();
    setBoundingBox( boundingbox );
    this.crs = crs;
  }

  /**
   * returns the name of the map
   */
  public String getName()
  {
    return name;
  }

  /**
   * returns the Theme that matches the submitted name
   */
  public Theme getTheme( String name )
  {
    return (Theme)themes.get( name );
  }

  /**
   * returns the Theme that matches the submitted index
   */
  public Theme getTheme( int index )
  {
    return (Theme)themesL.get( index );
  }

  /**
   * returns the Themes in correct order. The first Theme (index == 0) shall be
   * rendered at first (bottom most).
   */
  public Theme[] getAllThemes()
  {
    return (Theme[])themesL.toArray( new Theme[themesL.size()] );
  }

  /**
   * Returns the current scale of the MapView.
   */
  public double getScale()
  {
    return scale;
  }

  /**
   * Returns the current scale of the MapView.
   */
  public double getScale( Graphics g )
  {
    scale = calcScale( g.getClipBounds().width, g.getClipBounds().height );
    return scale;
  }

  /**
   * adds a theme to the MapView
   */
  public void addTheme( Theme theme ) throws Exception
  {
    themes.put( theme.getName(), theme );
    themesL.add( theme );
    enabled.put( theme.getName(), Boolean.TRUE );
    activatedTh = theme;
    theme.setParent( this );
    theme.getLayer().setCoordinatesSystem( crs );
  }

  /**
   * removes a theme from the MapView
   */
  public void removeTheme( Theme theme )
  {
    enabled.remove( theme.getName() );
    themesL.remove( themesL.indexOf( theme ) );
    themes.remove( theme.getName() );
  }

  /**
   * removes the theme that matches the submitted name from the MapView
   */
  public void removeTheme( String name )
  {
    removeTheme( getTheme( name ) );
  }

  /**
   * removes the theme that matches the submitted index from the MapView
   */
  public void removeTheme( int index )
  {
    removeTheme( (Theme)themesL.get( index ) );
  }

  /**
   * removes all themes from the MapView.
   */
  public void clear()
  {
    themes.clear();
    themesL.clear();
    enabled.clear();
    activatedTh = null;
  }

  /**
   * swaps the positions of the submitted themes
   */
  public void swapThemes( Theme first, Theme second )
  {
    Debug.debugMethodBegin();

    if( themesL.contains( first ) && themesL.contains( second ) )
    {
      int i1 = themesL.indexOf( first );
      int i2 = themesL.indexOf( second );
      themesL.set( i1, second );
      themesL.set( i2, first );
    }

    Debug.debugMethodEnd();
  }

  /**
   * move a theme up for one index position (index = oldindex + 1)
   */
  public void moveUp( Theme theme )
  {
    Debug.debugMethodBegin();

    int idx = themesL.indexOf( theme );
    if( idx < themesL.size() - 1 )
    {
      Theme th = (Theme)themesL.get( idx + 1 );
      swapThemes( theme, th );
    }

    Debug.debugMethodEnd();
  }

  /**
   * move a theme down for one index position (index = oldindex - 1)
   */
  public void moveDown( Theme theme )
  {
    Debug.debugMethodBegin();

    int idx = themesL.indexOf( theme );
    if( idx > 0 )
    {
      Theme th = (Theme)themesL.get( idx - 1 );
      swapThemes( theme, th );
    }

    Debug.debugMethodEnd();
  }

  /**
   * enables or disables a theme that is part of the MapView. A theme that has
   * been disabled won't be rendered and usually doesn't react to events
   * targeted to the MapView, but still is part of the MapView.
   */
  public void enableTheme( Theme theme, boolean enable )
  {
    enabled.put( theme.getName(), enable ? Boolean.TRUE : Boolean.FALSE );
  }

  /**
   * returns true if the passed theme is set to be enabled
   */
  public boolean isThemeEnabled( Theme theme )
  {
    return ( (Boolean)enabled.get( theme.getName() ) ).booleanValue();
    //return enabled.get( theme.getName() ) != null;
  }

  /**
   * activates a theme. Usually the activated theme is perferred to react to
   * events (this doesn't mean that other themes are not allowed to react to
   * events).
   */
  public void activateTheme( Theme theme )
  {
    activatedTh = theme;
  }

  /**
   * returns true if the passed theme is the one that is set to be activated
   */
  public boolean isThemeActivated( Theme theme )
  {
    return activatedTh.getName().equals( theme.getName() );
  }

  /**
   * returns the amount of themes within the MapView.
   */
  public int getThemeSize()
  {
    return themes.size();
  }

  /**
   * adds an eventcontroller to the MapView that's reponsible for handling
   * events that targets the map. E.g.: zooming, panning, selecting a feature
   * etc.
   */
  public void addEventController( MapEventController obj )
  {
    eventCntr.add( obj );
    obj.addMapView( this );
  }

  /**
   * @see org.deegree_impl.graphics.MapView_Impl#addEventController(MapEventController)
   */
  public void removeEventController( MapEventController obj )
  {
    eventCntr.remove( obj );
    obj.removeMapView( this );
  }

  /**
   * A selector is a class that offers methods for selecting and deselecting
   * single DisplayElements or groups of DisplayElements. A selector may offers
   * methods like 'select all DisplayElements within a specified bounding box'
   * or 'select all DisplayElements thats area is larger than 120 km?' etc.
   */
  public void addSelector( Selector obj )
  {
    for( int i = 0; i < themesL.size(); i++ )
    {
      getTheme( i ).addSelector( obj );
    }
  }

  /**
   * @see org.deegree_impl.graphics.MapView_Impl#addSelector(Selector)
   */
  public void removeSelector( Selector obj )
  {
    for( int i = 0; i < themesL.size(); i++ )
    {
      getTheme( i ).removeSelector( obj );
    }
  }

  /**
   * returns the BoundingBox (Envelope) of the MapView. This isn't nessecary the
   * BoundingBox of the data that will be rendered. It's the boundingBox of the
   * the visible area of the map measured in its coordinate reference system.
   */
  public GM_Envelope getBoundingBox()
  {
    return boundingbox;
  }

  /**
   * @see org.deegree_impl.graphics.MapView_Impl#getBoundingBox() this method
   *      may be used for zooming and panning the map
   */
  public void setBoundingBox( GM_Envelope boundingbox )
  {
    this.boundingbox = boundingbox;
    projection.setSourceRect( boundingbox );
  }

  /**
   * returns the coordinate reference system of the MapView
   */
  public CS_CoordinateSystem getCoordinatesSystem()
  {
    return crs;
  }

  /**
   * sets the coordinate reference system of the map;
   */
  public void setCoordinateSystem( CS_CoordinateSystem crs ) throws Exception
  {
    this.crs = crs;
    for( int i = 0; i < themesL.size(); i++ )
    {
      Layer lay = getTheme( i ).getLayer();
      lay.setCoordinatesSystem( crs );
    }
  }

  /**
   * renders the map to the passed graphic context
   * 
   * @param g
   * @throws RenderException
   *           thrown if the passed <tt>Graphic<tt> haven't
   *                         clipbounds. use g.setClip( .. );
   */
  public void paint( Graphics g ) throws RenderException
  {
    Debug.debugMethodBegin();
    if( getThemeSize() == 0 )
      return;
    if( g.getClipBounds() == null )
    {
      throw new RenderException( "no clip bounds defined for graphic context" );
    }

    int x = g.getClipBounds().x;
    int y = g.getClipBounds().y;
    int w = g.getClipBounds().width;
    int h = g.getClipBounds().height;
    projection.setDestRect( x - 2, y - 2, w + x, h + y );

    scale = calcScale( g.getClipBounds().width, g.getClipBounds().height );

    // call all Optimizers
    optimize( g );

    // paint all Themes
    for( int i = 0; i < themesL.size(); i++ )
    {
      if( isThemeEnabled( getTheme( i ) ) )
      {
        getTheme( i ).paint( g );
      }
    }

    Debug.debugMethodEnd();
  }

  /**
   * renders the features marked as selected of all themes contained within the
   * MapView
   * 
   * @param g
   *          graphic context to render the map too
   * @throws RenderException
   *           thrown if the passed <tt>Graphic<tt> haven't
   *                         clipbounds. use g.setClip( .. );
   */
  public void paintSelected( Graphics g ) throws RenderException
  {
    Debug.debugMethodBegin( this, "paintSelected" );
    if( getThemeSize() == 0 )
      return;
    if( g.getClipBounds() == null )
    {
      throw new RenderException( "no clip bounds defined for graphic context" );
    }

    int x = g.getClipBounds().x;
    int y = g.getClipBounds().y;
    int width = g.getClipBounds().width;
    int height = g.getClipBounds().height;
    projection.setDestRect( x - 2, y - 2, width + x, height + y );

    scale = calcScale( g.getClipBounds().width, g.getClipBounds().height );

    // call all Optimizers
    optimize( g );

    // paint all Themes
    for( int i = 0; i < themesL.size(); i++ )
    {
      if( isThemeEnabled( getTheme( i ) ) )
      {
        getTheme( i ).paintSelected( g );
      }
    }

    Debug.debugMethodEnd();
  }

  /**
   * renders the features marked as highlighted of all themes contained within
   * the MapView
   * 
   * @param g
   *          graphic context to render the map too
   * @throws RenderException
   *           thrown if the passed <tt>Graphic<tt> haven't
   *                         clipbounds. use g.setClip( .. );
   */
  public void paintHighlighted( Graphics g ) throws RenderException
  {
    Debug.debugMethodBegin( this, "paintHighlighted" );
    if( getThemeSize() == 0 )
      return;
    if( g.getClipBounds() == null )
    {
      throw new RenderException( "no clip bounds defined for graphic context" );
    }

    int x = g.getClipBounds().x;
    int y = g.getClipBounds().y;
    int width = g.getClipBounds().width;
    int height = g.getClipBounds().height;
    projection.setDestRect( x - 2, y - 2, width + x, height + y );

    scale = calcScale( g.getClipBounds().width, g.getClipBounds().height );

    // call all Optimizers
    optimize( g );

    // paint all Themes
    for( int i = 0; i < themesL.size(); i++ )
    {
      if( isThemeEnabled( getTheme( i ) ) )
      {
        getTheme( i ).paintHighlighted( g );
      }
    }

    Debug.debugMethodEnd();
  }

  /**
   * A Highlighter is a class that is responsible for managing the highlight
   * capabilities for one or more Themes.
   */
  public void addHighlighter( Highlighter highlighter )
  {
    for( int i = 0; i < themesL.size(); i++ )
    {
      getTheme( i ).addHighlighter( highlighter );
    }
  }

  /**
   * @see org.deegree_impl.graphics.MapView_Impl#addHighlighter(Highlighter)
   */
  public void removeHighlighter( Highlighter highlighter )
  {
    for( int i = 0; i < themesL.size(); i++ )
    {
      getTheme( i ).removeHighlighter( highlighter );
    }
  }

  /**
   * calculates the map scale (denominator) as defined in the OGC SLD 1.0.0
   * specification
   * 
   * @return scale of the map
   */
  private double calcScale( int mapWidth, int mapHeight )
  {

    try
    {
      CS_CoordinateSystem epsg4326crs = getCoordinatesSystem();
      GM_Envelope bbox = getBoundingBox();

      if( !getCoordinatesSystem().getName().equalsIgnoreCase( "EPSG:4326" ) )
      {
        // transform the bounding box of the request to EPSG:4326
        GeoTransformer transformer = new GeoTransformer( "EPSG:4326" );
        bbox = transformer.transformEnvelope( bbox, epsg4326crs );
      }

      double dx = bbox.getWidth() / mapWidth;
      double dy = bbox.getHeight() / mapHeight;

      // create a box on the central map pixel to determine its size in meters
      GM_Position min = GeometryFactory.createGM_Position( bbox.getMin().getX() + dx
          * ( mapWidth / 2d - 1 ), bbox.getMin().getY() + dy * ( mapHeight / 2d - 1 ) );
      GM_Position max = GeometryFactory.createGM_Position( bbox.getMin().getX() + dx
          * ( mapWidth / 2d ), bbox.getMin().getY() + dy * ( mapHeight / 2d ) );
      double distance = calcDistance( min.getY(), min.getX(), max.getY(), max.getX() );

      // default pixel size defined in SLD specs is 28mm
      double scale = distance / 0.00028;

      return scale;
    }
    catch( Exception e )
    {
      Debug.debugException( e, "Exception occured when calculating scale!" );
    }
    return 0.0;
  }

  /**
   * calculates the distance in meters between two points in EPSG:4326
   * coodinates .
   */
  private double calcDistance( double lon1, double lat1, double lon2, double lat2 )
  {
    double r = 6378.137;
    double rad = Math.PI / 180d;
    double cose = 0;

    cose = Math.sin( rad * lon1 ) * Math.sin( rad * lon2 ) + Math.cos( rad * lon1 )
        * Math.cos( rad * lon2 ) * Math.cos( rad * ( lat1 - lat2 ) );
    double dist = r * Math.acos( cose );

    return dist * 1000;
  }

  /**
   * Returns the <tt>GeoTransform</tt> that is associated to this MapView.
   * <p>
   * 
   * @return the associated <tt>GeoTransform</tt> -instance
   */
  public GeoTransform getProjection()
  {
    return projection;
  }

  /**
   * Calls all registered <tt>Optimizer</tt> subsequently.
   * 
   * @param g
   */
  private void optimize( Graphics g )
  {
    Graphics2D g2 = (Graphics2D)g;
    Iterator it = optimizers.iterator();
    while( it.hasNext() )
    {
      AbstractOptimizer optimizer = (AbstractOptimizer)it.next();
      optimizer.optimize( g2 );
    }
  }

  /**
   * Adds an <tt>Optimizer</tt>.
   * 
   * @param optimizer
   */
  public void addOptimizer( Optimizer optimizer )
  {
    optimizers.add( optimizer );
    optimizer.setMapView( this );
  }

  /**
   * Returns the <tt>Optimizer</tt>s.
   * 
   * @return
   */
  public Optimizer[] getOptimizers()
  {
    return (Optimizer[])optimizers.toArray( new Optimizer[0] );
  }

  /**
   * Sets the <tt>Optimizer<tt>s.
   * @param optimizers
   */
  public void setOptimizers( Optimizer[] optimizers )
  {
    this.optimizers.clear();
    for( int i = 0; i < optimizers.length; i++ )
    {
      addOptimizer( optimizers[i] );
    }
  }

}