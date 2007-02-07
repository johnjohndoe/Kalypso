package org.kalypsodeegree.model.feature.binding;

import org.kalypsodeegree.model.feature.Feature;

/**
 * Classes implementing this interface signals their feature wrapper nature and provide an implementation of
 * {@link #getWrappedFeature()} to get the feature they are wrapping
 * 
 * @deprecated Use {@link IFeatureWrapper2} instead. 
 * 
 * @author Patrice Congo
 */
@Deprecated
public interface IFeatureWrapper
{
  public Feature getWrappedFeature( );

  public String getGmlID( );
}
