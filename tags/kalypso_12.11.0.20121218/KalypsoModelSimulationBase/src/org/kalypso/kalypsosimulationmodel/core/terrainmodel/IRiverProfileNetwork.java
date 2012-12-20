package org.kalypso.kalypsosimulationmodel.core.terrainmodel;

import javax.xml.namespace.QName;

import org.kalypso.kalypsosimulationmodel.schema.UrlCatalogModelSimulationBase;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.core.gml.IProfileSelectionProvider;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;

/**
 * Interface to be implemented by classes that represent a simBase:RiverProfileNetwork
 *
 * @author Patrice Congo
 */
public interface IRiverProfileNetwork extends Feature, IProfileSelectionProvider
{
  public static QName QNAME = new QName( UrlCatalogModelSimulationBase.SIM_MODEL_NS, "RiverProfileNetwork" ); //$NON-NLS-1$

  public static QName QNAME_PROP_RIVER_PROFILE = new QName( UrlCatalogModelSimulationBase.SIM_MODEL_NS, "riverProfile" ); //$NON-NLS-1$

  IFeatureBindingCollection<IProfileFeature> getProfiles( );
}