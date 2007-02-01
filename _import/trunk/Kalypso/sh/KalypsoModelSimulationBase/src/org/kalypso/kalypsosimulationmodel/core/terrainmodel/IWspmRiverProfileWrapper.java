/**
 * 
 */
package org.kalypso.kalypsosimulationmodel.core.terrainmodel;

import javax.xml.namespace.QName;

import org.kalypso.kalypsosimulationmodel.schema.UrlCatalogModelSimulationBase;
import org.kalypso.model.wspm.core.gml.WspmProfile;

/**
 * @author congo
 */
public interface IWspmRiverProfileWrapper extends IRiverProfile
{
  public static QName QNAME = new QName( UrlCatalogModelSimulationBase.SIM_MODEL_NS, "WspmRiverProfileWrapper" );
  
  public static QName QNAME_PROP_WSPM_RIVER_PROFILE = new QName( UrlCatalogModelSimulationBase.SIM_MODEL_NS, "wspmRiverProfile" );

  public WspmProfile getWspmRiverProfile( );

  public void setWspmRiverProfile( final WspmProfile wspmProfile );
}
