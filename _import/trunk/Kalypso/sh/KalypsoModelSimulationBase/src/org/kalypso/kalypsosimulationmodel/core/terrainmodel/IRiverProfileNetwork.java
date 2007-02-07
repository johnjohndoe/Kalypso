package org.kalypso.kalypsosimulationmodel.core.terrainmodel;

import javax.xml.namespace.QName;

import org.kalypso.kalypsosimulationmodel.core.IFeatureWrapperCollection;
import org.kalypso.kalypsosimulationmodel.schema.UrlCatalogModelSimulationBase;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper2;

/**
 * Interface to be implemented by classes that represent a simBase:RiverProfileNetwork
 * 
 * @author Patrice Congo
 */
public interface IRiverProfileNetwork extends IFeatureWrapperCollection<IRiverProfile>, IFeatureWrapper2
{
  public static QName QNAME = new QName( UrlCatalogModelSimulationBase.SIM_MODEL_NS, "RiverProfileNetwork" );

  public static QName QNAME_PROP_RIVER_PROFILE = new QName( UrlCatalogModelSimulationBase.SIM_MODEL_NS, "riverProfile" );

  /**
   * To get the river profile which is located before the given river
   * 
   * @param riverProfile
   *          the reference profile
   * @return the river profile situated before the specified one
   */
  public IRiverProfile getPrevious( final IRiverProfile riverProfile );

  /**
   * To get the river profile which is located after the given river
   * 
   * @param riverProfile
   *          the reference profile
   * @return the river profile situated after the specified one
   */
  public IRiverProfile getNext( final IRiverProfile riverProfile );

}
