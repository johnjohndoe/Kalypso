package org.kalypso.kalypsosimulationmodel.core.terrainmodel;

import javax.xml.namespace.QName;

import org.kalypso.kalypsosimulationmodel.schema.UrlCatalogModelSimulationBase;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper2;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;

/*
 * Interface to be implemented by classes that represent a simBase:RiverProfileNetwork
 *
 * @author Patrice Congo
 */
public interface IRiverProfileNetwork extends IFeatureWrapperCollection<IProfileFeature>, IFeatureWrapper2
{
  public static QName QNAME = new QName( UrlCatalogModelSimulationBase.SIM_MODEL_NS, "RiverProfileNetwork" ); //$NON-NLS-1$

  public static QName QNAME_PROP_RIVER_PROFILE = new QName( UrlCatalogModelSimulationBase.SIM_MODEL_NS, "riverProfile" ); //$NON-NLS-1$

  /**
   * To get the river profile which is located before the given river
   * 
   * @param riverProfile
   *          the reference profile
   * @return the river profile situated before the specified one
   */
  public IProfileFeature getPrevious( final IProfileFeature riverProfile );

  /**
   * To get the river profile which is located after the given river
   * 
   * @param riverProfile
   *          the reference profile
   * @return the river profile situated after the specified one
   */
  public IProfileFeature getNext( final IProfileFeature riverProfile );

}
