package org.kalypso.ogc.gml;

import java.awt.Graphics;

import org.deegree.graphics.sld.UserStyle;
import org.deegree.graphics.transformation.GeoTransform;
import org.deegree.model.geometry.GM_Envelope;

/**
 * @author vdoemming
 */
public class KalypsoFeatureTheme extends AbstractKalypsoTheme
{
  private IKalypsoLayer myLayer = null;

  //  private KalypsoUserStyle[] myStyles = null;

  public boolean DEBUG_ENV = false;

  public KalypsoFeatureTheme( final IKalypsoLayer layer, final String name )
  {
    super( name );

    myLayer = layer;
    //    myStyles = new KalypsoUserStyle[] {};

    myLayer.addModellListener( this );
  }

  public void dispose()
  {
    myLayer.removeModellListener( this );
  }

  //  /**
  //   * renders the layer to the submitted graphic context
  //   */
  //  public void paint( final Graphics g, final GeoTransform p, final double
  // scale, final GM_Envelope bbox )
  //  {
  //// final double scale = myParent.getScale( g );
  //// final GeoTransform p = myParent.getProjection();
  //// final GM_Envelope bbox = myParent.getBoundingBox();
  //    // for( int i = 0; i < myStyles.length; i++ )
  //    myLayer.getSort().paint( g, p, scale, bbox );
  //
  //    // if( DEBUG_ENV )
  //    // myIndexDE.paint( g, myParent.getProjection() );
  //  }

  public void paintSelected( final Graphics g, final GeoTransform p, final double scale,
      final GM_Envelope bbox, final int selectionId )
  {
    if( myLayer instanceof KalypsoFeatureLayer )
    {
      //    double scale = myParent.getScale( g );
      //    GeoTransform p = myParent.getProjection();
      //    GM_Envelope bbox = myParent.getBoundingBox();
      //    for( int i = 0; i < myStyles.length; i++ )
      ( (KalypsoFeatureLayer)myLayer ).getSort().paintSelected( g, p, scale, bbox, selectionId );

      //    if( DEBUG_ENV )
      //      myIndexDE.paint( g, myParent.getProjection() );
    }
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
    // TODO
    if( myLayer instanceof KalypsoFeatureLayer )
      return ( (KalypsoFeatureLayer)myLayer ).getSort().getStyles();
    return null;
  }

  public void addStyle( final KalypsoUserStyle style )
  {
    if( myLayer instanceof KalypsoFeatureLayer )
      ( (KalypsoFeatureLayer)myLayer ).getSort().addStyle( style );

    fireModellEvent( null );
  }

  public void removeStyle( final KalypsoUserStyle style )
  {
    if( myLayer instanceof KalypsoFeatureLayer )
      ( (KalypsoFeatureLayer)myLayer ).getSort().removeStyle( style );

    fireModellEvent( null );
  }

  /**
   * returns the layer that holds the data of the theme
   */
  public IKalypsoLayer getLayer()
  {
    return myLayer;
  }
}