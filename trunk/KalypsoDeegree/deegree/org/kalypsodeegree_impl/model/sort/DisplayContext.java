package org.deegree_impl.model.sort;

import java.awt.Graphics;
import org.deegree.graphics.displayelements.DisplayElement;
import org.deegree.graphics.sld.UserStyle;
import org.deegree.graphics.transformation.GeoTransform;
import org.deegree.model.feature.Feature;
import org.deegree.model.geometry.GM_Object;
import org.deegree_impl.graphics.displayelements.DisplayElementFactory;

public class DisplayContext
{

  private boolean isSelected = false;

  private Feature myFE = null;

  private DisplayElement myDE[][] = null;

  public DisplayContext( Feature feature )
  {
    myFE = feature;
  }

  public DisplayContext( Feature feature, UserStyle styles[] )
  {
    myFE = feature;
    updateDisplayElements( styles );
  }

  public boolean isSelected()
  {
    return isSelected;
  }

  public boolean select()
  {
    if( isSelected )
      return false;
    else
    {
      isSelected = true;
      return true;
    }
  }

  public boolean unselect()
  {
    if( !isSelected )
      return false;
    else
    {
      isSelected = false;
      return true;
    }
  }

  public void toggle()
  {
    isSelected = !isSelected;
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

  public void updateDisplayElements( UserStyle styles[] )
  {
    myDE = new DisplayElement[styles.length][];
    for( int i = 0; i < styles.length; i++ )
    {
      try
      {

        myDE[i] = DisplayElementFactory.createDisplayElement( myFE, new UserStyle[]
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
      myDE[styleNo] = DisplayElementFactory.createDisplayElement( myFE, new UserStyle[]
      { styles[styleNo] } );
    }
    catch( Exception e )
    {
      e.printStackTrace();
      myDE[styleNo] = null;
    }
  }

  public Feature getFeature()
  {
    return myFE;
  }

  public GM_Object getDefaultGeometry()
  {
    return myFE.getDefaultGeometryProperty();
  }

  /**
   * @see java.lang.Object#equals(java.lang.Object)
   */
  public boolean equals( Object other )
  {
    if( !( other instanceof DisplayContext ) )
      return false;
    if( ( (DisplayContext)other ).getFeature() != myFE )
      return false;
    return true;

  }

  /**
   * @see java.lang.Object#hashCode()
   */
  public int hashCode()
  {
    return myFE.hashCode();
  }
}