package org.kalypso.kalypsosimulationmodel.core.terrainmodel;

import java.util.List;

import javax.xml.namespace.QName;

import org.kalypso.kalypsosimulationmodel.core.IFeatureWrapperCollection;
import org.kalypso.kalypsosimulationmodel.schema.UrlCatalogModelSimulationBase;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper2;

/**
 * Interface to be implemented by class which represents the simBase:RiverProfileNetworkCollection
 * 
 * @author Patrice Congo
 */
public interface IRiverProfileNetworkCollection extends IFeatureWrapperCollection<IRiverProfileNetwork>, IFeatureWrapper2
{
  public static QName QNAME = new QName( UrlCatalogModelSimulationBase.SIM_MODEL_NS, "RiverProfileNetworkCollection" );

  public static QName QNAME_PROP_PROFILE_NETWORK = new QName( UrlCatalogModelSimulationBase.SIM_MODEL_NS, "riverProfileNetwork" );;

  /**
   * Select river profile network base which names matches the given regular expression
   * 
   * @param regExp
   *          the regular expressen to match with
   * @return a {@link List} of river profile networks that matches the the given regular expression
   */
  public List<IRiverProfileNetwork> selectRiverProfileNetwork( String regExp );
}
