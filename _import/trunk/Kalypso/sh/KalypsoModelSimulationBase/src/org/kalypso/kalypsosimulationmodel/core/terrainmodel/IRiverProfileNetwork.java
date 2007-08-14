package org.kalypso.kalypsosimulationmodel.core.terrainmodel;

import javax.xml.namespace.QName;

import org.kalypso.kalypsosimulationmodel.schema.UrlCatalogModelSimulationBase;
import org.kalypso.model.wspm.core.gml.WspmProfile;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper2;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;

/**
 * Interface to be implemented by classes that represent a simBase:RiverProfileNetwork
 * 
 * @author Patrice Congo
 */
public interface IRiverProfileNetwork extends IFeatureWrapperCollection<WspmProfile>, IFeatureWrapper2
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
  public WspmProfile getPrevious( final WspmProfile riverProfile );

  /**
   * To get the river profile which is located after the given river
   * 
   * @param riverProfile
   *          the reference profile
   * @return the river profile situated after the specified one
   */
  public WspmProfile getNext( final WspmProfile riverProfile );

}
