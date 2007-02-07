package org.kalypso.kalypsosimulationmodel.core.terrainmodel;

import javax.xml.namespace.QName;

import org.kalypso.kalypsosimulationmodel.schema.UrlCatalogModelSimulationBase;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper2;

/**
 * Interface to be implemented by classes which represents
 * a simBase:_RiverProfile element
 * 
 * @author Patrice Congo
 *
 */
public interface IRiverProfile extends IFeatureWrapper2
{
  public static final QName QNAME = new QName( UrlCatalogModelSimulationBase.SIM_MODEL_NS, "_RiverProfile" );
	
}
