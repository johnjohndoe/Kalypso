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

import org.deegree.services.wcas.metadatadesc.ResourceSpecifiedUsage;
import org.deegree.services.wcas.metadatadesc.UserContactInfo;

/**
 * ResourceSpecificUsage_Impl.java
 * 
 * Created on 16. September 2002, 10:34
 */
public class ResourceSpecifiedUsage_Impl implements ResourceSpecifiedUsage
{

  String specifiedusage = null;

  String usagedatetime = null;

  ArrayList usercontactinfo = null;

  String userdetirminedlimitations = null;

  /** Creates a new instance of ResourceSpecifiedUsage_Impl */
  public ResourceSpecifiedUsage_Impl( String specifiedusage, String usagedatetime,
      UserContactInfo[] usercontactinfo, String userdetirminedlimitations )
  {

    this.usercontactinfo = new ArrayList();
    setSpecifiedUsage( specifiedusage );
    setUsageDateTime( usagedatetime );
    setUserContactInfo( usercontactinfo );
    setUserDetirminedLimitations( userdetirminedlimitations );

  }

  /**
   * @return String
   *  
   */
  public String getSpecifiedUsage()
  {
    return specifiedusage;
  }

  /**
   * @see getSpecifiedUsage
   */
  public void setSpecifiedUsage( String specifiedusage )
  {
    this.specifiedusage = specifiedusage;
  }

  /**
   * minOccurs="0"
   * 
   * @return String
   *  
   */
  public String getUsageDateTime()
  {
    return usagedatetime;
  }

  /**
   * @see getUsageDateTime
   */
  public void setUsageDateTime( String usagedatetime )
  {
    this.usagedatetime = usagedatetime;
  }

  /**
   * maxOccurs="unbounded"
   * 
   * @return UserContactInfo-array
   *  
   */
  public UserContactInfo[] getUserContactInfo()
  {
    return (UserContactInfo[])usercontactinfo.toArray( new UserContactInfo[usercontactinfo.size()] );
  }

  /**
   * @see getUserContactInfo
   */
  public void addUserContactInfo( UserContactInfo usercontactinfo )
  {
    this.usercontactinfo.add( usercontactinfo );
  }

  /**
   * @see getUserContactInfo
   */
  public void setUserContactInfo( UserContactInfo[] usercontactinfo )
  {
    this.usercontactinfo.clear();
    for( int i = 0; i < usercontactinfo.length; i++ )
    {
      this.usercontactinfo.add( usercontactinfo[i] );
    }
  }

  /**
   * minOccurs="0"
   * 
   * @return String
   *  
   */
  public String getUserDetirminedLimitations()
  {
    return userdetirminedlimitations;
  }

  /**
   * @see getUserDetirminedLimitations
   */
  public void setUserDetirminedLimitations( String userdetirminedlimitations )
  {
    this.userdetirminedlimitations = userdetirminedlimitations;
  }

  /**
   * to String method
   */
  public String toString()
  {
    String ret = null;
    ret = "specifiedusage = " + specifiedusage + "\n";
    ret += "usagedatetime = " + usagedatetime + "\n";
    ret += "usercontactinfo = " + usercontactinfo + "\n";
    ret += "userdetirminedlimitations = " + userdetirminedlimitations + "\n";
    return ret;
  }

}