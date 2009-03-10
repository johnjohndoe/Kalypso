package org.kalypsodeegree.model.feature;

import javax.xml.namespace.QName;

import org.deegree.model.spatialschema.GeometryException;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Object;

/**
 * Some Interface Methods from Deegree 2 Feature Implementation. We don't extend the Deegree2 Feature Interface
 * directly. At the moment we only want to get nearer to the Deegree2 Feature API
 * 
 * @author Dirk Kuch
 */

public interface Deegree2Feature
{
  /**
   * @return changed to GM_Envelope
   * @throws GeometryException
   */
  GM_Envelope getBoundedBy( ) throws GeometryException;

  /**
   * @return changed to GM_Object
   */
  GM_Object getDefaultGeometryPropertyValue( );

  String getDescription( );

  // changed to IFeatureType
  IFeatureType getFeatureType( );

  /**
   * @return changed to GM_Object[]
   */
  GM_Object[] getGeometryPropertyValues( );

  String getId( );

  /**
   * Name of Method changed to getQualifiedName
   * 
   * @return changed to QName
   */
  QName getQualifiedName( );

  /**
   * @return changed to Feature
   */
  Feature getOwner( );

// /**
// * @return changed to IFeatureType[]
// */
// IFeatureType[] getProperties( );

  public void setEnvelopesUpdated( );

  /**
   * @param ft
   *          changed to IFeatureType
   */
  void setFeatureType( IFeatureType ft );

  void setId( String fid );

}
