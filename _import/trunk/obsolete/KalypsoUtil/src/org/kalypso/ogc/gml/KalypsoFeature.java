package org.kalypso.ogc.gml;

import java.awt.Graphics;

import org.deegree.graphics.displayelements.DisplayElement;
import org.deegree.graphics.sld.UserStyle;
import org.deegree.graphics.transformation.GeoTransform;
import org.deegree.model.feature.Feature;
import org.deegree.model.feature.FeatureProperty;
import org.deegree.model.feature.FeatureTypeProperty;
import org.deegree_impl.graphics.displayelements.DisplayElementFactory;
import org.deegree_impl.model.feature.FeatureFactory;
import org.deegree_impl.model.feature.Feature_Impl;

/**
 * @author doemming
 */
public class KalypsoFeature extends Feature_Impl
{
  private int mySelection=0;
  private DisplayElement myDE[][] = null;

  
  public KalypsoFeature(Feature feature)
  {
    super(feature.getId(),feature.getFeatureType(),getFeatureProperties(feature));
  }
  
  public KalypsoFeature(Feature feature,UserStyle[] styles)
  {
    super(feature.getId(),feature.getFeatureType(),getFeatureProperties(feature));
    setDisplayElements(styles);
  }
  
  private static FeatureProperty[] getFeatureProperties(Feature fe)
  {
    FeatureTypeProperty[] ftp=fe.getFeatureType().getProperties();
    FeatureProperty[] fp=new FeatureProperty[ftp.length];
    for(int i=0;i<ftp.length;i++)
      fp[i]=FeatureFactory.createFeatureProperty(ftp[i].getName(), fe.getProperty(ftp[i].getName()));
     return fp;
     
  }
  
  public boolean select(int selectID)
  {
    if(isSelected(selectID))
     return false;
    
      mySelection|=selectID;
      return true;
     
     }
  
  public boolean unselect(int selectID)
  {
    if(!isSelected(selectID))
      return false;
    mySelection&=~selectID;
    return true;
  }
  
  public boolean toggle(int selectID)
  {
  select(selectID);// mySelection^=selectID;  
   return true;
  }
  
  public boolean isSelected(int selectID)
  {
    return selectID==(mySelection&selectID);
  }
  
  public void setDisplayElements( UserStyle styles[] )
  {
    myDE = new DisplayElement[styles.length][];
    for( int i = 0; i < styles.length; i++ )
    {
      try
      {

        myDE[i] = DisplayElementFactory.createDisplayElement( this, new UserStyle[]
        { styles[i] } );
      }
      catch( Exception e )
      {
        e.printStackTrace();
        myDE[i] = null;
      }
    }
  }

  public void updateDisplayElements( int styleNo, UserStyle styles[] )
  {
    if( styleNo >= myDE.length )
      myDE = new DisplayElement[styles.length][];
    try
    {
      myDE[styleNo] = DisplayElementFactory.createDisplayElement( this, new UserStyle[]
      { styles[styleNo] } );
    }
    catch( Exception e )
    {
      e.printStackTrace();
      myDE[styleNo] = null;
    }
  }
  
  public void paint( Graphics g, GeoTransform projection, int styleNo )
  {
    if( myDE[styleNo] != null )
      for( int i = 0; i < myDE[styleNo].length; i++ )
        myDE[styleNo][i].paint( g, projection );
  }

  public void paint( Graphics g, GeoTransform projection, int styleNo, double scale )
  {
    if( myDE[styleNo] != null )
      for( int i = 0; i < myDE[styleNo].length; i++ )
        if( myDE[styleNo][i].doesScaleConstraintApply( scale ) )
          myDE[styleNo][i].paint( g, projection );
  }
}
