package org.kalypso.ogc.gml;

import java.awt.Graphics;

import org.deegree.graphics.sld.UserStyle;
import org.deegree.graphics.transformation.GeoTransform;
import org.deegree.model.geometry.GM_Envelope;
import org.kalypso.ogc.MapModell;

/**
 * @author vdoemming
 */
public class KalypsoWMSTheme extends AbstractKalypsoTheme
{
  // TODO: ist das richtig? ein layer im WMSTheme????
  private KalypsoFeatureLayer myLayer = null;

  //  private KalypsoUserStyle[] myStyles = null;

  private MapModell myParent = null;

  public KalypsoWMSTheme( final KalypsoFeatureLayer layer, final String name )
  {
    super( name );

    myLayer = layer;
    //    myStyles = new KalypsoUserStyle[] {};

    myLayer.addModellListener( this );
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoTheme#dispose()
   */
  public void dispose()
  {
    myLayer.removeModellListener(this);
  }

  /**
   * renders the layer to the submitted graphic context
   */
  public void paint( Graphics g )
  {
    double scale = myParent.getScale( g );
    GeoTransform p = myParent.getProjection();
    GM_Envelope bbox = myParent.getBoundingBox();
    //    for( int i = 0; i < myStyles.length; i++ )
    myLayer.getSort().paint( g, p, scale, bbox );

    //    if( DEBUG_ENV )
    //      myIndexDE.paint( g, myParent.getProjection() );
  }

  public void paintSelected( Graphics g, int selectionId )
  {
    double scale = myParent.getScale( g );
    GeoTransform p = myParent.getProjection();
    GM_Envelope bbox = myParent.getBoundingBox();
    //    for( int i = 0; i < myStyles.length; i++ )
    myLayer.getSort().paintSelected( g, p, scale, bbox, selectionId );

    //    if( DEBUG_ENV )
    //      myIndexDE.paint( g, myParent.getProjection() );
  }

  //  /**
  //   * stes the styles used for this <tt>Theme</tt>. If this method will be
  //   * called all <tt>DisplayElement</tt> s will be recreated to consider the
  //   * new style definitions.
  //   */
  //  public void setStyles( final UserStyle[] styles )
  //  {
  //    for( int i = 0; i < myStyles.length; i++ )
  //      myLayer.getSort().removeStyle( myStyles[i] );
  //
  //    myStyles = (KalypsoUserStyle[])styles;
  //    
  //    for( int i = 0; i < myStyles.length; i++ )
  //    {
  //      final KalypsoUserStyle kus = (KalypsoUserStyle)styles[i];
  //      myLayer.getSort().addStyle( kus );
  //    }
  //  }
  //
  
  /**
   * returns the styles used for this <tt>Theme</tt>.
   */
  public UserStyle[] getStyles()
  {
    //return myStyles;
    return myLayer.getSort().getStyles();
  }

  public void addStyle( final KalypsoUserStyle style )
  {
    myLayer.getSort().addStyle( style );

    fireModellEvent( null );
  }

  public void removeStyle( final KalypsoUserStyle style )
  {
    myLayer.getSort().removeStyle( style );

    fireModellEvent( null );
  }

  /**
   * returns the layer that holds the data of the theme
   */
  public KalypsoFeatureLayer getLayer()
  {
    return myLayer;
  }

  public void setParent( MapModell parent )
  {
    myParent = parent;
  }

}