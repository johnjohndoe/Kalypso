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
  public GMLWorkspace getWorkspace( );

  public Feature getParent( );

  public Object getVirtuelProperty( final VirtualFeatureTypeProperty virtualPropertyType, final GMLWorkspace workspace );

  public void setProperty( final IPropertyType propertyType, final Object value );

  public void setProperty( final QName propQName, final Object value );

  /**
   * @deprecated use getPropery(PropertyType)
   */
  @Deprecated
  public Object getProperty( final String propLocalName );

  /**
   * @deprecated
   */
  @Deprecated
  public void setProperty( final String propLocalName, final Object value );

  public Object getProperty( final QName propQName );

  /**
   * intended to be called from GMLWorkspace when root feature is set.
   */
  public void setWorkspace( final GMLWorkspace workspace );
}