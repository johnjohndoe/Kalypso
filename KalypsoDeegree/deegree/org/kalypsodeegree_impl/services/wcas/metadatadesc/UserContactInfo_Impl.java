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

/*
 * UserContactInfo_Impl.java
 * 
 * Created on 16. September 2002, 10:40
 */

package org.deegree_impl.services.wcas.metadatadesc;

import java.util.ArrayList;

import org.deegree.services.wcas.metadatadesc.ContactInfo;
import org.deegree.services.wcas.metadatadesc.RoleCode;
import org.deegree.services.wcas.metadatadesc.UserContactInfo;

/**
 * 
 * @author as
 */
public class UserContactInfo_Impl implements UserContactInfo
{

  private ArrayList contactinfo = null;

  private ArrayList individualname = null;

  private ArrayList organisationname = null;

  private ArrayList positionname = null;

  private ArrayList rolecode = null;

  /** Creates a new instance of UserContactInfo_Impl */
  public UserContactInfo_Impl( ContactInfo[] contactinfo, String[] individualname,
      String[] organisationname, String[] positionname, RoleCode[] rolecode )
  {

    this.contactinfo = new ArrayList();
    this.individualname = new ArrayList();
    this.organisationname = new ArrayList();
    this.positionname = new ArrayList();
    this.rolecode = new ArrayList();

    setContactInfo( contactinfo );
    setIndividualName( individualname );
    setOrganisationName( organisationname );
    setPositionName( positionname );
    setRoleCode( rolecode );
  }

  /**
   * minOccurs="0" maxOccurs="unbounded"
   * 
   * @return ContactInfo-Array
   *  
   */
  public ContactInfo[] getContactInfo()
  {
    return (ContactInfo[])contactinfo.toArray( new ContactInfo[contactinfo.size()] );
  }

  /**
   * @see getContactInfo
   */
  public void addContactInfo( ContactInfo contactinfo )
  {
    this.contactinfo.add( contactinfo );
  }

  /**
   * @see getContactInfo
   */
  public void setContactInfo( ContactInfo[] contactinfo )
  {
    this.contactinfo.clear();
    for( int i = 0; i < contactinfo.length; i++ )
    {
      this.contactinfo.add( contactinfo[i] );
    }
  }

  /**
   * minOccurs="0" maxOccurs="unbounded"
   * 
   * @return String-Array
   *  
   */
  public String[] getIndividualName()
  {
    return (String[])individualname.toArray( new String[individualname.size()] );
  }

  /**
   * @see getIndividualName
   */
  public void addIndividualName( String individualname )
  {
    this.individualname.add( individualname );
  }

  /**
   * @see getIndividualName
   */
  public void setIndividualName( String[] individualname )
  {
    this.individualname.clear();
    for( int i = 0; i < individualname.length; i++ )
    {
      this.individualname.add( individualname[i] );
    }
  }

  /**
   * minOccurs="0" maxOccurs="unbounded"
   * 
   * @return String-Array
   */
  public String[] getOrganisationName()
  {
    return (String[])organisationname.toArray( new String[organisationname.size()] );
  }

  /**
   * @see getOrganisationName
   */
  public void addOrganisationName( String organisationname )
  {
    this.organisationname.add( organisationname );
  }

  /**
   * @see getOrganisationName
   */
  public void setOrganisationName( String[] organisationname )
  {
    this.organisationname.clear();
    for( int i = 0; i < organisationname.length; i++ )
    {
      this.organisationname.add( organisationname[i] );
    }
  }

  /**
   * minOccurs="0" maxOccurs="unbounded"
   * 
   * @return String-Array
   *  
   */
  public String[] getPositionName()
  {
    return (String[])positionname.toArray( new String[positionname.size()] );
  }

  /**
   * @see getPositionName
   */
  public void addPositionName( String positionname )
  {
    this.positionname.add( positionname );
  }

  /**
   * @see getPositionName
   */
  public void setPositionName( String[] positionname )
  {
    this.positionname.clear();
    for( int i = 0; i < positionname.length; i++ )
    {
      this.positionname.add( positionname[i] );
    }
  }

  /**
   * minOccurs="0" maxOccurs="unbounded"
   * 
   * @return RoleCode-Array
   *  
   */
  public RoleCode[] getRoleCode()
  {
    return (RoleCode[])rolecode.toArray( new RoleCode[rolecode.size()] );
  }

  /**
   * @see getRoleCode
   */
  public void addRoleCode( RoleCode rolecode )
  {
    this.rolecode.add( rolecode );
  }

  /**
   * @see getRoleCode
   */
  public void setRoleCode( RoleCode[] rolecode )
  {
    this.rolecode.clear();
    for( int i = 0; i < rolecode.length; i++ )
    {
      this.rolecode.add( rolecode[i] );
    }
  }

  /**
   * to String method
   */
  public String toString()
  {
    String ret = null;
    ret = "contactinfo = " + contactinfo + "\n";
    ret += "individualname = " + individualname + "\n";
    ret += "organisationname = " + organisationname + "\n";
    ret += "positionname = " + positionname + "\n";
    ret += "rolecode = " + rolecode + "\n";
    return ret;
  }

}