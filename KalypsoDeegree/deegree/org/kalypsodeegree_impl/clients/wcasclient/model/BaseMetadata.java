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
 * 
 * 
 * @version $Revision$
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 */
public class BaseMetadata
{
  private Calendar dateStamp = null;

  private String fileIdentifier = null;

  private String metadataStandardName = null;

  private String metadataStandardVersion = null;

  private String title = null;

  /** Creates a new instance of BriefEntry */
  public BaseMetadata()
  {}

  /**
   * Creates a new BriefEntry object.
   * 
   * @param title
   * @param fileIdentifier
   * @param dateStamp
   * @param metadataStandardName
   * @param metadataStandardVersion
   */
  public BaseMetadata( String title, String fileIdentifier, Calendar dateStamp,
      String metadataStandardName, String metadataStandardVersion )
  {
    setTitle( title );
    setFileIdentifier( fileIdentifier );
    setDateStamp( dateStamp );
    setMetadataStandardName( metadataStandardName );
    setMetadataStandardVersion( metadataStandardVersion );
  }

  /**
   * returns the title of the metadata set
   * 
   * @return title of the metadata set
   */
  public String getTitle()
  {
    return title;
  }

  /**
   * sets the title of the metadata set
   * 
   * @param title
   *          title of the metadata set
   */
  public void setTitle( String title )
  {
    this.title = title;
  }

  /**
   * returns the fileIdentifier(ID) of the metadata set
   * 
   * @return fileIdentifier(ID) of the metadata set
   */
  public String getFileIdentifier()
  {
    return fileIdentifier;
  }

  /**
   * sets the fileIdentifier(ID) of the metadata set
   * 
   * @param fileIdentifier
   *          fileIdentifier(ID) of the metadata set
   */
  public void setFileIdentifier( String fileIdentifier )
  {
    this.fileIdentifier = fileIdentifier;
  }

  /**
   * returns the date stamp of the metadata set
   * 
   * @return date stamp of the metadata set
   */
  public Calendar getDateStamp()
  {
    return dateStamp;
  }

  /**
   * sets the date stamp of the metadata set
   * 
   * @param dateStamp
   *          date stamp of the metadata set
   */
  public void setDateStamp( Calendar dateStamp )
  {
    this.dateStamp = dateStamp;
  }

  /**
   * returns the metadata standard name where the metadata are stored in.
   * 
   * @return metadata standard name
   */
  public String getMetadataStandardName()
  {
    return metadataStandardName;
  }

  /**
   * sets the metadata standard name where the metadata are stored in.
   * 
   * @param metadataStandardName
   *          metadata standard name
   */
  public void setMetadataStandardName( String metadataStandardName )
  {
    this.metadataStandardName = metadataStandardName;
  }

  /**
   * returns the version of the metadata standard where the metadata are stored
   * in.
   * 
   * @return version of the metadata standard
   */
  public String getMetadataStandardVersion()
  {
    return metadataStandardVersion;
  }

  /**
   * sets the version of the metadata standard where the metadata are stored in.
   * 
   * @param metadataStandardVersion
   */
  public void setMetadataStandardVersion( String metadataStandardVersion )
  {
    this.metadataStandardVersion = metadataStandardVersion;
  }

}