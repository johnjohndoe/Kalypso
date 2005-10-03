package org.kalypsodeegree.model.feature;

/**
 * @author doemming
 * 
 * this class extends the deegree feature interface and implements methods to handle properties that have maxOccurs > 1
 */
public interface Feature extends DeegreeFeature
{
  public void addProperty( FeatureProperty prop );

//  public boolean isSelected( int selectID );
//
//  public boolean toggle( int selectID );
//
//  public boolean unselect( int selectID );
//
//  public boolean select( int selectID );

//  /** Gibt die gesamte Selektion zur�ck */
//  public int getSelection();
//
//  /** setzt die Selektion komplett */
//  public void setSelection( final int selection );

  public Object getVirtuelProperty( final String propertyName, final GMLWorkspace workspace );

  public void setProperty( final String propertyName,final Object value);
  /** Uses the visitor on each property of this feature. */
  public void accept( final IFeaturePropertyVisitor visitor );
}