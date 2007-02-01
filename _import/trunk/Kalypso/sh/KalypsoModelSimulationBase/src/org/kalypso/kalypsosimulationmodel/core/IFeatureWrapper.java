package org.kalypso.kalypsosimulationmodel.core;

import org.kalypsodeegree.model.feature.Feature;

/**
 * Classes implementing this interface signals their feature wrapper nature and provide an implementation of
 * {@link #getWrappedFeature()} to get the feature they are wrapping
 * 
 * @author Patrice Congo
 */
public interface IFeatureWrapper
{
  public Feature getWrappedFeature( );
}
