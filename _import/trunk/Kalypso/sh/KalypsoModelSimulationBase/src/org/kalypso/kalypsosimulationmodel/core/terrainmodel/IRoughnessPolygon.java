package org.kalypso.kalypsosimulationmodel.core.terrainmodel;

import javax.xml.namespace.QName;

import org.kalypso.kalypsosimulationmodel.core.roughness.IRoughnessCls;
import org.kalypso.kalypsosimulationmodel.schema.UrlCatalogModelSimulationBase;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper2;
import org.kalypsodeegree.model.geometry.GM_MultiSurface;
import org.kalypsodeegree.model.geometry.GM_Object;

/**
 * The interface to be implemented by classes representing a simBase:RoughnessPolygon element
 * 
 * @author Dejan Antanaskovic, Patrice Congo
 */
public interface IRoughnessPolygon extends IFeatureWrapper2
{
  public static final QName QNAME = new QName( UrlCatalogModelSimulationBase.SIM_MODEL_NS, "RoughnessPolygon" );

  public static final QName SUBSTITUTION_GROUP = new QName( "http://www.opengis.net/gml", "_Feature" );

  public static final QName PROP_POSITION = new QName( UrlCatalogModelSimulationBase.SIM_MODEL_NS, "polygonProperty" );

  public static final QName PROP_ROUGHNESS_STYLE = new QName( UrlCatalogModelSimulationBase.SIM_MODEL_NS, "roughnessStyle" );

  public static final QName PROP_CORRECTION_KS = new QName( UrlCatalogModelSimulationBase.SIM_MODEL_NS, "correction_ks" );

  public static final QName PROP_CORRECTION_AXAY = new QName( UrlCatalogModelSimulationBase.SIM_MODEL_NS, "correction_axay" );

  public static final QName PROP_CORRECTION_DP = new QName( UrlCatalogModelSimulationBase.SIM_MODEL_NS, "correction_dp" );

  /**
   * Returns the roughness ID for this polygon
   * 
   * @return the id of the roughness associated to the polygon
   */
  public String getRoughnessStyle( );

  /**
   * Answer the roughness class associated with this roughness polynom
   * 
   * @return the roughness class of this roughness polynom
   */
  public IRoughnessCls getRoughnessCls();
  
  public void setRoughnessClassMember( Feature linkedFeature ) throws IllegalArgumentException;

  /**
   * Returns the roughness MultiPolygon
   * 
   * @return the surface
   */
  public GM_MultiSurface getSurface( );

  /**
   * Sets the geometry of the roughness polygon
   * 
   * @param polygon
   *          the polygon to set
   */
  public void setSurface( GM_MultiSurface polygon );

  /**
   * Sets the geometry of the roughness polygon
   * 
   * @param polygon
   *          the polygon to set
   * @throws IllegalArgumentException if parameter cannot be converted to GM_MultiSurface 
   */
  public void setSurface( GM_Object object ) throws IllegalArgumentException;
  
  public void setCorrectionParameterKS(double value);

  public void setCorrectionParameterAxAy(double value);

  public void setCorrectionParameterDP(double value);

  public Double getCorrectionParameterKS();

  public Double getCorrectionParameterAxAy();

  public Double getCorrectionParameterDP();

}
