package org.deegree.model.feature;

import org.deegree.model.geometry.GM_Object;

/**
 * @author doemming
 * 
 * this class extends the deegree feature interface and implements methods to
 * handle properties that have maxOccurs > 1
 */
public interface Feature extends DeegreeFeature
{
  public void addProperty( FeatureProperty prop );

  public boolean isSelected( int selectID );

  public boolean toggle( int selectID );

  public boolean unselect( int selectID );

  public boolean select( int selectID );

  /** Gibt die gesamte Selektion zur�ck */
  public int getSelection();

  /** setzt die Selektion komplett */
  public void setSelection( final int selection );

  public Object getVirtuelProperty( String propertyName,GMLWorkspace workspace );
}