package org.kalypso.kalypsosimulationmodel.core.terrainmodel;

import java.util.List;

import javax.xml.namespace.QName;

import org.kalypso.kalypsosimulationmodel.schema.UrlCatalogModelSimulationBase;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper2;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;

/**
 * Interface to be implemented by class which represents the simBase:RiverProfileNetworkCollection
 * 
 * @author Patrice Congo
 */
public interface IRiverProfileNetworkCollection extends IFeatureWrapperCollection<IRiverProfileNetwork>, IFeatureWrapper2
{
  public static QName QNAME = new QName( UrlCatalogModelSimulationBase.SIM_MODEL_NS, "RiverProfileNetworkCollection" ); //$NON-NLS-1$

  public static QName QNAME_PROP_PROFILE_NETWORK = new QName( UrlCatalogModelSimulationBase.SIM_MODEL_NS, "riverProfileNetwork" ); //$NON-NLS-1$

  /**
   * Select river profile network base which names matches the given regular expression
   * 
   * @param regExp
   *          the regular expressen to match with
   * @return a {@link List} of river profile networks that matches the the given regular expression
   */
  public List<IRiverProfileNetwork> selectRiverProfileNetwork( String regExp );
}
