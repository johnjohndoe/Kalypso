package org.kalypso.ogc.gml;

import java.awt.Graphics;
import java.util.ArrayList;

import org.deegree.graphics.Highlighter;
import org.deegree.graphics.Layer;
import org.deegree.graphics.MapView;
import org.deegree.graphics.Selector;
import org.deegree.graphics.Theme;
import org.deegree.graphics.ThemeEventController;
import org.deegree.graphics.sld.UserStyle;
import org.deegree.graphics.transformation.GeoTransform;
import org.deegree.model.geometry.GM_Envelope;
import org.deegree.model.sort.JMSpatialIndex;
import org.kalypso.ogc.event.ModellEvent;
import org.kalypso.ogc.event.ModellEventListener;
import org.kalypso.ogc.event.ModellEventProvider;
import org.kalypso.ogc.event.ModellEventProviderAdapter;

/**
 * @author vdoemming
 */
public class KalypsoTheme implements Theme, ModellEventProvider, ModellEventListener
{
  private ModellEventProviderAdapter myEventProvider = new ModellEventProviderAdapter();

  private KalypsoFeatureLayer myLayer = null;

  private KalypsoUserStyle[] myStyles = null;

  public boolean DEBUG_ENV = false;

  private MapView myParent = null;

  private String myName;

  public KalypsoTheme( final KalypsoFeatureLayer layer, final String name )
  {
    myLayer = layer;
    myStyles = new KalypsoUserStyle[] {};
    myName = name;

    myLayer.addModellListener( this );
  }

  public void dispose()
  {
    myLayer.removeModellListener( this );
  }

  /**
   * returns the name of the layer
   */
  public String getName()
  {
    return myName;
  }
  
  public void setName( final String name )
  {
    myName = name;
    
    fireModellEvent(null);
  }

  /**
   * renders the layer to the submitted graphic context
   */
  public void paint( Graphics g )
  {
    double scale = myParent.getScale( g );
    GeoTransform p = myParent.getProjection();
    GM_Envelope bbox = myParent.getBoundingBox();
    for( int i = 0; i < myStyles.length; i++ )
      myLayer.getSort().paint( g, p, myStyles[i], scale, bbox );

    //    if( DEBUG_ENV )
    //      myIndexDE.paint( g, myParent.getProjection() );
  }

  /**
   * stes the styles used for this <tt>Theme</tt>. If this method will be
   * called all <tt>DisplayElement</tt> s will be recreated to consider the
   * new style definitions.
   */
  public void setStyles( final UserStyle[] styles )
  {
    for( int i = 0; i < myStyles.length; i++ )
    {
      myLayer.getSort().removeStyle( myStyles[i] );
      myStyles[i].removeModellListener( this );
    }

    myStyles = (KalypsoUserStyle[])styles;
    
    for( int i = 0; i < myStyles.length; i++ )
    {
      final KalypsoUserStyle kus = (KalypsoUserStyle)styles[i];
      kus.addModellListener(this );
      myLayer.getSort().addStyle( kus );
    }
  }

  /*
   * 
   * 
   * public void setStyles( UserStyle[] styles ) { System.out.println( "set
   * Styles of Theme \"" + getName() + "\"" ); myStyles = styles; // TODO das
   * muss noch anders werden... // myIndexDE =
   * JMSpatialIndexFactory.createSpatialIndex( // myLayer.getBoundingBox() );
   * 
   * DisplayElementFactory fac = new DisplayElementFactory(); if( myLayer
   * instanceof FeatureLayer ) { try { for( int i = 0; i < (
   * (FeatureLayer)myLayer ).getSize(); i++ ) { Feature feature = (
   * (FeatureLayer)myLayer ).getFeature( i ); myIndexDE.add( new DisplayContext(
   * feature, styles ) ); } } catch( Exception e ) { e.printStackTrace();
   * System.out.println( e ); } }
   */
  /*
   * else { try { //instance of RasterLayer RasterLayer rl =
   * (RasterLayer)myLayer; DisplayElement[] de =
   * DisplayElementFactory.createDisplayElement( rl.getRaster(), styles ); //
   * GM_Envelope // env=rl.getBoundingBox(); for( int k = 0; k < de.length; k++ ) { //
   * displayElementsIndex.add( env, de[k] ); // displayElements.add( de[k] ); } }
   * catch( Exception e ) { e.printStackTrace();
   * 
   * System.out.println( e ); } } }
   */

  /**
   * returns the styles used for this <tt>Theme</tt>.
   */
  public UserStyle[] getStyles()
  {
    return myStyles;
  }

  /**
   * returns the layer that holds the data of the theme
   */
  public Layer getLayer()
  {
    return myLayer;
  }

  public void addEventController( ThemeEventController arg0 )
  {
  // nothing
  }

  public void addHighlighter( Highlighter arg0 )
  {
  // nothing
  }

  public void addSelector( Selector arg0 )
  {
  // nothing
  }

  public ArrayList getDisplayElements()
  {
    return null;
  }

  public void paint( Graphics arg0, String[] arg1 )
  {
  // nothing
  }

  public void paintHighlighted( Graphics arg0 )
  {
  // nothing
  }

  public void paintSelected( Graphics arg0 )
  {
  // nothing
  }

  public void removeEventController( ThemeEventController arg0 )
  {
  // nothing
  }

  public void removeHighlighter( Highlighter arg0 )
  {
  // nothing
  }

  public void removeSelector( Selector arg0 )
  {
  // nothing
  }

  public void setDisplayElements( ArrayList arg0 )
  {
  // nothing
  }

  public void setParent( MapView parent )
  {
    myParent = parent;
  }

  /**
   * @see org.deegree.graphics.Theme#getSpatialIndex()
   */
  public JMSpatialIndex getSpatialIndex()
  {
    // nothing, wir nicht gebraucht
    return null;
  }

  /**
   * 
   * @see org.kalypso.ogc.event.ModellEventListener#onModellChange(org.kalypso.ogc.event.ModellEvent)
   */
  public void onModellChange( ModellEvent modellEvent )
  {
    fireModellEvent(modellEvent);
  }

  public void addModellListener( ModellEventListener listener )
  {
    myEventProvider.addModellListener( listener );
  }

  public void fireModellEvent( ModellEvent event )
  {
    myEventProvider.fireModellEvent( event );
  }

  public void removeModellListener( ModellEventListener listener )
  {
    myEventProvider.removeModellListener( listener );
  }
}