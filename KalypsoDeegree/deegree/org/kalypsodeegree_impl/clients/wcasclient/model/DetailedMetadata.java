/*----------------    FILE HEADER  ------------------------------------------

 This file is part of deegree.
 Copyright (C) 2001 by:
 EXSE, Department of Geography, University of Bonn
 http://www.giub.uni-bonn.de/exse/
 lat/lon Fitzke/Fretter/Poth GbR
 http://www.lat-lon.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 Andreas Poth
 lat/lon Fitzke/Fretter/Poth GbR
 Meckenheimer Allee 176
 53115 Bonn
 Germany
 E-Mail: poth@lat-lon.de

 Jens Fitzke
 Department of Geography
 University of Bonn
 Meckenheimer Allee 166
 53115 Bonn
 Germany
 E-Mail: jens.fitzke@uni-bonn.de

 
 ---------------------------------------------------------------------------*/
package org.deegree_impl.clients.wcasclient.model;

import java.util.Calendar;

/**
 * encapsulates a detailed metadata description for a geo-spatial dataset. the
 * class attributes are based on ISO 19115 but may be also filled with data from
 * other metadata formats.
 * 
 * @version $Revision$
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 */
public class DetailedMetadata extends BaseMetadata
{
  private Calendar endTime = null;

  private Calendar startTime = null;

  private String abstract_ = null;

  private String crs = null;

  private String lineageSrcDesc = null;

  private String lineageStatement = null;

  private String maintenanceAndUpdateFrequency = null;

  private String processStep = null;

  private String resourceConstraints = null;

  private String scale = null;

  private Contact[] contacts = null;

  private String[] keywords = null;

  private OnlineTransferOption[] onlineTransferOptions = null;

  private String metadataStandardName = null;

  /** Creates a new instance of DetailedMD_Metadata */
  public DetailedMetadata()
  {}

  /**
   * Creates a new DetailedMD_Metadata object.
   * 
   * @param title
   * @param fileIdentifier
   * @param dateStamp
   * @param metadataStandardName
   * @param metadataStandardVersion
   */
  public DetailedMetadata( String title, String fileIdentifier, Calendar dateStamp,
      String metadataStandardName, String metadataStandardVersion, Calendar startTime,
      Calendar endTime, String abstract_, String crs, String lineageSrcDesc,
      String lineageStatement, String maintenanceAndUpdateFrequency, String resourceConstraints,
      String processStep, String scale, String[] keywords, Contact[] contacts,
      OnlineTransferOption[] onlineTransferOptions )
  {
    super( title, fileIdentifier, dateStamp, metadataStandardVersion, metadataStandardVersion );
    setStartTime( startTime );
    setEndTime( endTime );
    setAbstract( abstract_ );
    setCrs( crs );
    setLineageSrcDesc( lineageSrcDesc );
    setLineageStatement( lineageStatement );
    setMaintenanceAndUpdateFrequency( maintenanceAndUpdateFrequency );
    setResourceConstraints( resourceConstraints );
    setProcessStep( processStep );
    setScale( scale );
    setKeywords( keywords );
    setContacts( contacts );
    setOnlineTransferOptions( onlineTransferOptions );
    setMetadataStandardName( metadataStandardName );
  }

  /**
   * 
   * 
   * @return
   */
  public String getAbstract()
  {
    return abstract_;
  }

  /**
   * 
   * 
   * @param abstract_
   */
  public void setAbstract( String abstract_ )
  {
    this.abstract_ = abstract_;
  }

  /**
   * 
   * 
   * @return
   */
  public String getCrs()
  {
    return crs;
  }

  /**
   * 
   * 
   * @param crs
   */
  public void setCrs( String crs )
  {
    this.crs = crs;
  }

  /**
   * 
   * 
   * @return
   */
  public String getScale()
  {
    return scale;
  }

  /**
   * 
   * 
   * @param scale
   */
  public void setScale( String scale )
  {
    this.scale = scale;
  }

  /**
   * 
   * 
   * @return
   */
  public Calendar getStartTime()
  {
    return startTime;
  }

  /**
   * 
   * 
   * @param startTime
   */
  public void setStartTime( Calendar startTime )
  {
    this.startTime = startTime;
  }

  /**
   * 
   * 
   * @return
   */
  public Calendar getEndTime()
  {
    return endTime;
  }

  /**
   * 
   * 
   * @param endTime
   */
  public void setEndTime( Calendar endTime )
  {
    this.endTime = endTime;
  }

  /**
   * 
   * 
   * @return
   */
  public String getLineageStatement()
  {
    return lineageStatement;
  }

  /**
   * 
   * 
   * @param lineageStatement
   */
  public void setLineageStatement( String lineageStatement )
  {
    this.lineageStatement = lineageStatement;
  }

  /**
   * 
   * 
   * @return
   */
  public String getLineageSrcDesc()
  {
    return lineageSrcDesc;
  }

  /**
   * 
   * 
   * @param lineageSrcDesc
   */
  public void setLineageSrcDesc( String lineageSrcDesc )
  {
    this.lineageSrcDesc = lineageSrcDesc;
  }

  /**
   * 
   * 
   * @return
   */
  public String getProcessStep()
  {
    return processStep;
  }

  /**
   * 
   * 
   * @param processStep
   */
  public void setProcessStep( String processStep )
  {
    this.processStep = processStep;
  }

  /**
   * 
   * 
   * @return
   */
  public String getResourceConstraints()
  {
    return resourceConstraints;
  }

  /**
   *  
   */
  public void setResourceConstraints( String resourceConstraints )
  {
    this.resourceConstraints = resourceConstraints;
  }

  /**
   * 
   * 
   * @return
   */
  public String getMaintenanceAndUpdateFrequency()
  {
    return maintenanceAndUpdateFrequency;
  }

  /**
   * 
   * 
   * @param maintenanceAndUpdateFrequency
   */
  public void setMaintenanceAndUpdateFrequency( String maintenanceAndUpdateFrequency )
  {
    this.maintenanceAndUpdateFrequency = maintenanceAndUpdateFrequency;
  }

  /**
   * 
   * 
   * @return
   */
  public String[] getKeywords()
  {
    return keywords;
  }

  /**
   * 
   * 
   * @param keywords
   */
  public void setKeywords( String[] keywords )
  {
    this.keywords = keywords;
  }

  /**
   * returns a list of contact informations associated to a metadataset
   */
  public Contact[] getContacts()
  {
    return contacts;
  }

  /**
   * sets a list of contact informations associated to a metadataset
   */
  public void setContacts( Contact[] contacts )
  {
    this.contacts = contacts;
  }

  /**
   * 
   * 
   * @return
   */
  public OnlineTransferOption[] getOnlineTransferOptions()
  {
    return onlineTransferOptions;
  }

  /**
   * 
   * 
   * @param onlineTransferOptions
   */
  public void setOnlineTransferOptions( OnlineTransferOption[] onlineTransferOptions )
  {
    this.onlineTransferOptions = onlineTransferOptions;
  }

  /**
   * @see org.deegree_impl.clients.wcasclient.model.BaseMetadata#getMetadataStandardName()
   */
  public String getMetadataStandardName()
  {
    return metadataStandardName;
  }

  /**
   * @see org.deegree_impl.clients.wcasclient.model.BaseMetadata#setMetadataStandardName(java.lang.String)
   */
  public void setMetadataStandardName( String metadataStandardName )
  {
    this.metadataStandardName = metadataStandardName;
  }
}