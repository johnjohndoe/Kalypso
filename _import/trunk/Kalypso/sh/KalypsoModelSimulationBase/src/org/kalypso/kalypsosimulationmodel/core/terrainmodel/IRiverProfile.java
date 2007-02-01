package org.kalypso.kalypsosimulationmodel.core.terrainmodel;

import javax.xml.namespace.QName;

import org.kalypso.kalypsosimulationmodel.core.IFeatureWrapper;
import org.kalypso.kalypsosimulationmodel.schema.UrlCatalogModelSimulationBase;

/**
 * Interface to be implemented by classes which represents
 * a simBase:_RiverProfile element
 * 
 * @author Patrice Congo
 *
 */
public interface IRiverProfile extends IFeatureWrapper
{
  public static final QName QNAME = new QName( UrlCatalogModelSimulationBase.SIM_MODEL_NS, "_RiverProfile" );
	
}
