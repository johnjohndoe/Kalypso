/*
 * Created on Oct 6, 2004
 *
 * TODO To change the template for this generated file go to
 * Window - Preferences - Java - Code Style - Code Templates
 */
package org.deegree.model.feature;

import org.deegree_impl.gml.schema.GMLSchema;

/**
 * @author doemming
 */
public interface GMLWorkspace 
{
	 public Feature getRootFeature();
	 
	  public GMLSchema getSchema();

	/**
	 * 
	 */
	public FeatureType[] getFeatureTypes();

	/**
	 * @param ft
	 */
	public Feature[] getFeatures(FeatureType ft);

    /**
     * @param fe
     * @param string
     * @return
     */
    public Feature resolveLink(Feature srcFeature, String linkPropertyName);
}
