package org.kalypsodeegree.model.feature;

import java.util.List;

import org.kalypso.gmlschema.property.IPropertyType;

/**
 * Implements the visitor pattern to iterate over alle properties of a feature.
 * 
 * <p>
 * This class is the visitor part. The visitor distinguishes between normal properties (objects), features, linked
 * features and lists of feature-associations.
 * </p>
 * 
 * @author belger
 */
public interface IFeaturePropertyVisitor
{
  /** Called if the current property is a feature.
   * 
   * @param position The (internal) position of the property inside the feature. -1 if not known.
   * @param feature May be null, if the visitor is used on the type alone.
   */
  public boolean visit( final int position, final IPropertyType name, final Feature feature );

  /** Called if the current property is a link to another feature.
   * @param position The (internal) position of the property inside the feature. -1 if not known.
   * @param linkedFeatureId May be null, if the visitor is used on the type alone.
   */
  public boolean visit( final int position, final IPropertyType name, final String linkedFeatureId );

  /** Called if the current property is a list of (links to) features.
   * @param position The (internal) position of the property inside the feature. -1 if not known.
   * @param list May be null, if the visitor is used on the type alone.
    */
  public boolean visit( final int position, final IPropertyType name, final List list );

  /** Called if the current property anything else. 
   * @param position The (internal) position of the property inside the feature. -1 if not known.
   * @param data May be null, if the visitor is used on the type alone.
   */
  public boolean visit( final int position, final IPropertyType name, final Object data );
}
