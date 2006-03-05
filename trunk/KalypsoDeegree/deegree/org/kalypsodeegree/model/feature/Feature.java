package org.kalypsodeegree.model.feature;

import javax.xml.namespace.QName;

import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypsodeegree_impl.gml.schema.virtual.VirtualFeatureTypeProperty;

/**
 * @author doemming this class extends the deegree feature interface and implements methods to handle properties that
 *         have maxOccurs > 1
 */
public interface Feature extends DeegreeFeature
{
  public void addProperty( FeatureProperty prop );

  // public boolean isSelected( int selectID );
  //
  // public boolean toggle( int selectID );
  //
  // public boolean unselect( int selectID );
  //
  // public boolean select( int selectID );

  // /** Gibt die gesamte Selektion zurück */
  // public int getSelection();
  //
  // /** setzt die Selektion komplett */
  // public void setSelection( final int selection );

  public Object getVirtuelProperty( final VirtualFeatureTypeProperty virtualPropertyType, final GMLWorkspace workspace );

  public void setProperty( final IPropertyType propertyType, final Object value );

  /** Uses the visitor on each property of this feature. 
   * 
   * @TODO: REMARK Gernot: this is never used (i used it once, but no more) so maybe we delete it for simplicity? -> if we keep it, we should move ti to a utility class
   * */
  public void accept( final IFeaturePropertyVisitor visitor );

  /**
   * @deprecated use getPropery(PropertyType)
   */
  @Deprecated
  public Object getProperty( String propLocalName );

  /**
   * @deprecated
   */
  @Deprecated
  public void setProperty( final String propLocalName, final Object value );

  public Object getProperty( QName propQName );
}