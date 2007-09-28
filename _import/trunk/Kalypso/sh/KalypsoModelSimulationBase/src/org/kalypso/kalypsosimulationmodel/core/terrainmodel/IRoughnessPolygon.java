package org.kalypso.kalypsosimulationmodel.core.terrainmodel;

import javax.xml.namespace.QName;

import org.kalypso.commons.xml.NS;
import org.kalypso.kalypsosimulationmodel.core.roughness.IRoughnessCls;
import org.kalypso.kalypsosimulationmodel.schema.UrlCatalogModelSimulationBase;
import org.kalypso.ogc.gml.command.FeatureChange;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper2;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Surface;

/**
 * The interface to be implemented by classes representing a simBase:RoughnessPolygon element
 * 
 * @author Dejan Antanaskovic, Patrice Congo
 */
public interface IRoughnessPolygon extends IFeatureWrapper2
{
  public static final QName QNAME = new QName( UrlCatalogModelSimulationBase.SIM_MODEL_NS, "RoughnessPolygon" );

  public static final QName SUBSTITUTION_GROUP = new QName( NS.GML3, "_Feature" );

  public static final QName PROP_ROUGHNESS_CLASS_MEMBER = new QName( UrlCatalogModelSimulationBase.SIM_MODEL_NS, "roughnessClassMember" );

  public static final QName PROP_ROUGHNESS_STYLE = new QName( UrlCatalogModelSimulationBase.SIM_MODEL_NS, "roughnessStyle" );

  public static final QName PROP_GEOMETRY = new QName( UrlCatalogModelSimulationBase.SIM_MODEL_NS, "polygonGeometry" );

  public static final QName PROP_CORRECTION_KS = new QName( UrlCatalogModelSimulationBase.SIM_MODEL_NS, "correction_ks" );

  public static final QName PROP_CORRECTION_AXAY = new QName( UrlCatalogModelSimulationBase.SIM_MODEL_NS, "correction_axay" );

  public static final QName PROP_CORRECTION_DP = new QName( UrlCatalogModelSimulationBase.SIM_MODEL_NS, "correction_dp" );

  public static final String NO_ROUGHNESS = Messages.getString( "RoughnessPolygon.NoRoughness" );

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
  public IRoughnessCls getRoughnessCls( );

  public void setRoughnessClassMember( Feature linkedFeature ) throws IllegalArgumentException;

  public FeatureChange[] resetRoughnessClassMemberXLink( );

  public GM_Surface getSurface( );

  public void setSurface( GM_Object object );

  public void setCorrectionParameterKS( double value );

  public void setCorrectionParameterAxAy( double value );

  public void setCorrectionParameterDP( double value );

  public Double getCorrectionParameterKS( );

  public Double getCorrectionParameterAxAy( );

  public Double getCorrectionParameterDP( );

}
