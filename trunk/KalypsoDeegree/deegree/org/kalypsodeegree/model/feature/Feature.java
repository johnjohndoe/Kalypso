package org.kalypsodeegree.model.feature;

import javax.xml.namespace.QName;

import org.eclipse.core.runtime.IAdaptable;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypsodeegree_impl.gml.schema.virtual.VirtualFeatureTypeProperty;

/**
 * A GML Feature represents a general object.
 * <p>
 * A Feature is adaptable, thus allowing Adapter Factories and/or Subclasses to provide another "view" over a feature
 * object. For instance, an observation-feature can be directly represented as an observation.
 * 
 * @author doemming this class extends the deegree feature interface and implements methods to handle properties that
 *         have maxOccurs > 1
 */
public interface Feature extends DeegreeFeature, IAdaptable
{
  public GMLWorkspace getWorkspace( );

  /**
   * Return the parent of this feature, that is, the feature wich contains this feature as inline feature.
   * @see #getParentRelation()
   */
  public Feature getParent( );

  /**
   * Returns the {@link IRelationType} where this feature resides inside its parent feature.
   * @see #getParent()
   */
  public IRelationType getParentRelation();
  
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
  
  public void invalidEnvelope( );
}