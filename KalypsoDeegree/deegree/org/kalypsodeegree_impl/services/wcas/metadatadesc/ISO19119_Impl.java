/*
 * ---------------- FILE HEADER ------------------------------------------
 * 
 * This file is part of deegree. Copyright (C) 2001 by: EXSE, Department of
 * Geography, University of Bonn http://www.giub.uni-bonn.de/exse/ lat/lon
 * Fitzke/Fretter/Poth GbR http://www.lat-lon.de
 * 
 * This library is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Lesser General Public License as published by the Free
 * Software Foundation; either version 2.1 of the License, or (at your option)
 * any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation, Inc.,
 * 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 * 
 * Contact:
 * 
 * Andreas Poth lat/lon Fitzke/Fretter/Poth GbR Meckenheimer Allee 176 53115
 * Bonn Germany E-Mail: poth@lat-lon.de
 * 
 * Jens Fitzke Department of Geography University of Bonn Meckenheimer Allee 166
 * 53115 Bonn Germany E-Mail: jens.fitzke@uni-bonn.de
 * 
 * 
 * ---------------------------------------------------------------------------
 */

package org.deegree_impl.services.wcas.metadatadesc;

import java.util.ArrayList;

import org.deegree.services.wcas.metadatadesc.AccessProperties;
import org.deegree.services.wcas.metadatadesc.Citation;
import org.deegree.services.wcas.metadatadesc.DataCoupling;
import org.deegree.services.wcas.metadatadesc.ISO19115;
import org.deegree.services.wcas.metadatadesc.ISO19119;
import org.deegree.services.wcas.metadatadesc.Keywords;
import org.deegree.services.wcas.metadatadesc.LegalConstraints;
import org.deegree.services.wcas.metadatadesc.OperationMetadata;
import org.deegree.services.wcas.metadatadesc.PointOfContact;
import org.deegree.services.wcas.metadatadesc.Quality;
import org.deegree.services.wcas.metadatadesc.ResourceSpecifiedUsage;
import org.deegree.services.wcas.metadatadesc.SecurityConstraints;
import org.deegree.services.wcas.metadatadesc.ServiceType;
import org.deegree.services.wcas.metadatadesc.StatusCode;
import org.deegree.services.wcas.metadatadesc.TypeProperty;

/**
 * ISO19119_Impl.java
 * 
 * Created on 16. September 2002, 10:08
 * <p>
 * ----------------------------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:schaefer@lat-lon.de">Axel Schaefer </a>
 * @version $Revision$ $Date$
 */
public class ISO19119_Impl implements ISO19119
{

  String abstract_ = null;

  AccessProperties accessproperties = null;

  Citation citation = null;

  String credit = null;

  DataCoupling datacoupling = null;

  ArrayList keywords = null;

  String latlonboundingbox = null;

  ArrayList legalconstraints = null;

  ArrayList md_metadata = null;

  ArrayList operationmetadata = null;

  ArrayList pointofcontact = null;

  String purpose = null;

  Quality quality = null;

  ArrayList resourcespecifiedusage = null;

  ArrayList securityconstraints = null;

  ServiceType servicetype = null;

  String servicetypeversion = null;

  ArrayList statuscode = null;

  ArrayList typeproperty = null;

  /** Creates a new instance of asfasdasdsa */
  public ISO19119_Impl( String abstract_, AccessProperties accessproperties, Citation citation,
      String credit, DataCoupling datacoupling, Keywords[] keywords, ISO19115[] md_metadata,
      OperationMetadata[] operationmetadata, PointOfContact[] pointofcontact, String purpose,
      Quality quality, ResourceSpecifiedUsage[] resourcespecifiedusage,
      SecurityConstraints[] securityconstraints, ServiceType servicetype,
      String servicetypeversion, StatusCode[] statuscode, TypeProperty[] typeproperty )
  {

    this.keywords = new ArrayList();
    this.md_metadata = new ArrayList();
    this.operationmetadata = new ArrayList();
    this.pointofcontact = new ArrayList();
    this.resourcespecifiedusage = new ArrayList();
    this.securityconstraints = new ArrayList();
    this.statuscode = new ArrayList();
    this.typeproperty = new ArrayList();

    setAbstract( abstract_ );
    setAccessProperties( accessproperties );
    setCitation( citation );
    setCredit( credit );
    setDataCoupling( datacoupling );
    setKeywords( keywords );
    setMD_Metadata( md_metadata );
    setOperationMetadata( operationmetadata );
    setPointOfContact( pointofcontact );
    setPurpose( purpose );
    setQuality( quality );
    setResourceSpecifiedUsage( resourcespecifiedusage );
    setSecurityConstraints( securityconstraints );
    setServiceType( servicetype );
    setServiceTypeVersion( servicetypeversion );
    setStatusCode( statuscode );
    setTypeProperty( typeproperty );
  }

  /**
   * @return
   */
  public String getAbstract()
  {
    return abstract_;
  }

  /**
   * @see getAbstract
   */
  public void setAbstract( String abstract_ )
  {
    this.abstract_ = abstract_;
  }

  /**
   * @return
   */
  public AccessProperties getAccessProperties()
  {
    return accessproperties;
  }

  /**
   * @see getAccessProperties
   */
  public void setAccessProperties( AccessProperties accessproperties )
  {
    this.accessproperties = accessproperties;
  }

  /**
   * @return
   */
  public Citation getCitation()
  {
    return citation;
  }

  /**
   * @see getCitation
   */
  public void setCitation( Citation citation )
  {
    this.citation = citation;
  }

  /**
   * @return
   */
  public String getCredit()
  {
    return credit;
  }

  /**
   * @see getCredit
   */
  public void setCredit( String credit )
  {
    this.credit = credit;
  }

  /**
   * @return
   */
  public DataCoupling getDataCoupling()
  {
    return datacoupling;
  }

  /**
   * @see getDataCoupling
   */
  public void setDataCoupling( DataCoupling datacoupling )
  {
    this.datacoupling = datacoupling;
  }

  /**
   * minOccurs="0" maxOccurs="unbounded"
   * 
   * @return
   */
  public Keywords[] getKeywords()
  {
    return (Keywords[])keywords.toArray( new Keywords[keywords.size()] );
  }

  /**
   * @see getKeywords
   */
  public void addKeyword( Keywords keyword )
  {
    this.keywords.add( keyword );
  }

  /**
   * @see getKeywords
   */
  public void setKeywords( Keywords[] keywords )
  {
    this.keywords.clear();
    for( int i = 0; i < keywords.length; i++ )
    {
      this.keywords.add( keywords[i] );
    }
  }

  /**
   * minOccurs="0"
   * 
   * @return
   */
  public String getLatLonBoundingBox()
  {
    return latlonboundingbox;
  }

  /**
   * @see getLatLonBoundingBox
   */
  public void setLatLonBoundingBox( String latlonboundingbox )
  {
    this.latlonboundingbox = latlonboundingbox;
  }

  /**
   * minOccurs="0" maxOccurs="unbounded"
   * 
   * @return
   */
  public LegalConstraints[] getLegalConstraints()
  {
    return (LegalConstraints[])legalconstraints.toArray( new LegalConstraints[legalconstraints
        .size()] );
  }

  /**
   * @see getLegalConstraints
   */
  public void addLegalConstraints( LegalConstraints legalconstraints )
  {
    this.legalconstraints.add( legalconstraints );
  }

  /**
   * @see getLegalConstraints
   */
  public void setLegalConstraints( LegalConstraints[] legalconstraints )
  {
    this.legalconstraints.clear();
    for( int i = 0; i < legalconstraints.length; i++ )
    {
      this.legalconstraints.add( legalconstraints[i] );
    }
  }

  /**
   * minOccurs="0" maxOccurs="unbounded"
   * 
   * @return
   */
  public ISO19115[] getMD_Metadata()
  {
    return (ISO19115[])md_metadata.toArray( new ISO19115[md_metadata.size()] );
  }

  /**
   * @see getMD_Metadata
   */
  public void addMD_Metadata( ISO19115 md_metadata )
  {
    this.md_metadata.add( md_metadata );
  }

  /**
   * @see getMD_Metadata
   */
  public void setMD_Metadata( ISO19115[] md_metadata )
  {
    this.md_metadata.clear();
    for( int i = 0; i < md_metadata.length; i++ )
    {
      this.md_metadata.add( md_metadata[i] );
    }
  }

  /**
   * minOccurs="0" maxOccurs="unbounded"
   * 
   * @return
   */
  public OperationMetadata[] getOperationMetadata()
  {
    return (OperationMetadata[])operationmetadata.toArray( new OperationMetadata[operationmetadata
        .size()] );
  }

  /**
   * @see getOperationMetadata
   */
  public void addOperationMetadata( OperationMetadata operationmetadata )
  {
    this.operationmetadata.add( operationmetadata );
  }

  /**
   * @see getOperationMetadata
   */
  public void setOperationMetadata( OperationMetadata[] operationmetadata )
  {
    this.operationmetadata.clear();
    for( int i = 0; i < operationmetadata.length; i++ )
    {
      this.operationmetadata.add( operationmetadata[i] );
    }
  }

  /**
   * minOccurs="0" maxOccurs="unbounded"
   * 
   * @return
   */
  public PointOfContact[] getPointOfContact()
  {
    return (PointOfContact[])pointofcontact.toArray( new PointOfContact[pointofcontact.size()] );
  }

  /**
   * @see getPointOfContact
   */
  public void addPointOfContact( PointOfContact pointofcontact )
  {
    this.pointofcontact.add( pointofcontact );
  }

  /**
   * @see getPointOfContact
   */
  public void setPointOfContact( PointOfContact[] pointofcontact )
  {
    this.pointofcontact.clear();
    for( int i = 0; i < pointofcontact.length; i++ )
    {
      this.pointofcontact.add( pointofcontact[i] );
    }
  }

  /**
   * @return
   */
  public String getPurpose()
  {
    return purpose;
  }

  /**
   * @see getPurpose
   */
  public void setPurpose( String purpose )
  {
    this.purpose = purpose;
  }

  /**
   * minOccurs="0"
   * 
   * @return
   */
  public Quality getQuality()
  {
    return quality;
  }

  /**
   * @see getQuality
   */
  public void setQuality( Quality quality )
  {
    this.quality = quality;
  }

  /**
   * minOccurs="0" maxOccurs="unbounded"
   * 
   * @return
   */
  public ResourceSpecifiedUsage[] getResourceSpecifiedUsage()
  {
    return (ResourceSpecifiedUsage[])resourcespecifiedusage
        .toArray( new ResourceSpecifiedUsage[resourcespecifiedusage.size()] );
  }

  /**
   * @see getResourceSpecifiedUsage
   */
  public void addResourceSpecifiedUsage( ResourceSpecifiedUsage resourcespecifiedusage )
  {
    this.resourcespecifiedusage.add( resourcespecifiedusage );
  }

  /**
   * @see getResourceSpecifiedUsage
   */
  public void setResourceSpecifiedUsage( ResourceSpecifiedUsage[] resourcespecifiedusage )
  {
    this.resourcespecifiedusage.clear();
    for( int i = 0; i < resourcespecifiedusage.length; i++ )
    {
      this.resourcespecifiedusage.add( resourcespecifiedusage[i] );
    }
  }

  /**
   * minOccurs="0" maxOccurs="unbounded"
   * 
   * @return
   */
  public SecurityConstraints[] getSecurityConstraints()
  {
    return (SecurityConstraints[])securityconstraints
        .toArray( new SecurityConstraints[securityconstraints.size()] );
  }

  /**
   * @see getSecurityConstraints
   */
  public void addSecurityConstraints( SecurityConstraints securityconstraints )
  {
    this.securityconstraints.add( securityconstraints );
  }

  /**
   * @see getSecurityConstraints
   */
  public void setSecurityConstraints( SecurityConstraints[] securityconstraints )
  {
    this.securityconstraints.clear();
    for( int i = 0; i < securityconstraints.length; i++ )
    {
      this.securityconstraints.add( securityconstraints[i] );
    }
  }

  /**
   * @return
   */
  public ServiceType getServiceType()
  {
    return servicetype;
  }

  /**
   * @see getServiceType
   */
  public void setServiceType( ServiceType servicetype )
  {
    this.servicetype = servicetype;
  }

  /**
   * @return String
   *  
   */
  public String getServiceTypeVersion()
  {
    return servicetypeversion;
  }

  /**
   * @see getServiceTypeVersion
   */
  public void setServiceTypeVersion( String servicetypeversion )
  {
    this.servicetypeversion = servicetypeversion;
  }

  /**
   * minOccurs="0" maxOccurs="unbounded"
   * 
   * @return
   */
  public StatusCode[] getStatusCode()
  {
    return (StatusCode[])statuscode.toArray( new StatusCode[statuscode.size()] );
  }

  /**
   * @see getStatusCode
   */
  public void addStatusCode( StatusCode statuscode )
  {
    this.statuscode.add( statuscode );
  }

  /**
   * @see getStatusCode
   */
  public void setStatusCode( StatusCode[] statuscode )
  {
    this.statuscode.clear();
    for( int i = 0; i < statuscode.length; i++ )
    {
      this.statuscode.add( statuscode[i] );
    }
  }

  /**
   * minOccurs="0" maxOccurs="unbounded"
   * 
   * @return
   */
  public TypeProperty[] getTypeProperty()
  {
    return (TypeProperty[])typeproperty.toArray( new TypeProperty[typeproperty.size()] );
  }

  /**
   * @see getTypeProperty
   */
  public void addTypeProperty( TypeProperty typeproperty )
  {
    this.typeproperty.add( typeproperty );
  }

  /**
   * @see getTypeProperty
   */
  public void setTypeProperty( TypeProperty[] typeproperty )
  {
    this.typeproperty.clear();
    for( int i = 0; i < typeproperty.length; i++ )
    {
      this.typeproperty.add( typeproperty[i] );
    }
  }

  /**
   * to String method
   */
  public String toString()
  {
    String ret = null;
    ret = "abstract_ = " + abstract_ + "\n";
    ret += "accessproperties = " + accessproperties + "\n";
    ret += "citation = " + citation + "\n";
    ret += "credit = " + credit + "\n";
    ret += "datacoupling = " + datacoupling + "\n";
    ret += "keywords = " + keywords + "\n";
    ret += "latlonboundingbox = " + latlonboundingbox + "\n";
    ret += "legalconstraints = " + legalconstraints + "\n";
    ret += "md_metadata = " + md_metadata + "\n";
    ret += "operationmetadata = " + operationmetadata + "\n";
    ret += "pointofcontact = " + pointofcontact + "\n";
    ret += "purpose = " + purpose + "\n";
    ret += "quality = " + quality + "\n";
    ret += "resourcespecifiedusage = " + resourcespecifiedusage + "\n";
    ret += "securityconstraints = " + securityconstraints + "\n";
    ret += "servicetype = " + servicetype + "\n";
    ret += "servicetypeversion = " + servicetypeversion + "\n";
    ret += "statuscode = " + statuscode + "\n";
    ret += "typeproperty = " + typeproperty + "\n";
    return ret;
  }

}