package org.deegree.model.feature;

import java.awt.Graphics;

import org.deegree.graphics.sld.UserStyle;
import org.deegree.graphics.transformation.GeoTransform;


/**
 * @author doemming
 * 
 * this class extends the deegree feature interface and implements
 * methods to handle properties that have maxOccurs > 1
 */
public interface Feature extends DeegreeFeature
{
  public void addProperty( FeatureProperty prop );
  public void paint( Graphics g, GeoTransform projection, int styleNo, double scale );
  public void paint( Graphics g, GeoTransform projection, int styleNo );
  public void updateDisplayElements( int styleNo, UserStyle styles[] );
  public void setDisplayElements( UserStyle styles[] );
  public boolean isSelected(int selectID);
  public boolean toggle(int selectID);
  public boolean unselect(int selectID);
  public boolean select(int selectID);
  
}