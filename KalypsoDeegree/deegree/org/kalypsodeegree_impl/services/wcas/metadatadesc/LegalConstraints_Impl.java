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

import org.deegree.services.wcas.metadatadesc.LegalConstraints;
import org.deegree.services.wcas.metadatadesc.PropertyRightsCode;
import org.deegree.services.wcas.metadatadesc.UseConstraintsCode;

/**
 * LegalConstraints_Impl.java
 * 
 * Created on 16. September 2002, 10:19
 * <p>
 * ----------------------------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:schaefer@lat-lon.de">Axel Schaefer </a>
 * @version $Revision$ $Date$
 */
public class LegalConstraints_Impl implements LegalConstraints
{

  ArrayList otherconstraints = null;

  ArrayList propertyrightscode = null;

  ArrayList useconstraintscode = null;

  ArrayList uselimitation = null;

  /** Creates a new instance of LegalConstraints_Impl */
  public LegalConstraints_Impl( String[] otherconstraints, PropertyRightsCode[] propertyrightscode,
      UseConstraintsCode[] useconstraintscode, String[] uselimitation )
  {

    this.otherconstraints = new ArrayList();
    this.propertyrightscode = new ArrayList();
    this.useconstraintscode = new ArrayList();
    this.uselimitation = new ArrayList();

    setOtherConstraints( otherconstraints );
    setPropertyRightsCode( propertyrightscode );
    setUseConstraintsCode( useconstraintscode );
    setUseLimitation( uselimitation );
  }

  /**
   * minOccurs="0" maxOccurs="unbounded"
   * 
   * @param
   *  
   */
  public String[] getOtherConstraints()
  {
    return (String[])otherconstraints.toArray( new String[otherconstraints.size()] );
  }

  /**
   * @see getOtherConstraints
   */
  public void addOtherConstraints( String otherconstraints )
  {
    this.otherconstraints.add( otherconstraints );
  }

  /**
   * @see getOtherConstraints
   */
  public void setOtherConstraints( String[] otherconstraints )
  {
    this.otherconstraints.clear();
    for( int i = 0; i < otherconstraints.length; i++ )
    {
      this.otherconstraints.add( otherconstraints[i] );
    }
  }

  /**
   * minOccurs="0" maxOccurs="unbounded"
   * 
   * @param
   *  
   */
  public PropertyRightsCode[] getPropertyRightsCode()
  {
    return (PropertyRightsCode[])propertyrightscode
        .toArray( new PropertyRightsCode[propertyrightscode.size()] );
  }

  /**
   * @see getPropertyRightsCode
   */
  public void addPropertyRightsCode( PropertyRightsCode propertyrightscode )
  {
    this.propertyrightscode.add( propertyrightscode );
  }

  /**
   * @see getPropertyRightsCode
   */
  public void setPropertyRightsCode( PropertyRightsCode[] propertyrightscode )
  {
    this.propertyrightscode.clear();
    for( int i = 0; i < propertyrightscode.length; i++ )
    {
      this.propertyrightscode.add( propertyrightscode[i] );
    }
  }

  /**
   * minOccurs="0" maxOccurs="unbounded"
   * 
   * @param
   *  
   */
  public UseConstraintsCode[] getUseConstraintsCode()
  {
    return (UseConstraintsCode[])useconstraintscode
        .toArray( new UseConstraintsCode[useconstraintscode.size()] );
  }

  /**
   * @see getUseConstraintsCode
   */
  public void addUseConstraintsCode( UseConstraintsCode useconstraintscode )
  {
    this.useconstraintscode.add( useconstraintscode );
  }

  /**
   * @see getUseConstraintsCode
   */
  public void setUseConstraintsCode( UseConstraintsCode[] useconstraintscode )
  {
    this.useconstraintscode.clear();
    for( int i = 0; i < useconstraintscode.length; i++ )
    {
      this.useconstraintscode.add( useconstraintscode[i] );
    }
  }

  /**
   * minOccurs="0" maxOccurs="unbounded"
   * 
   * @param Stringarray
   *  
   */
  public String[] getUseLimitation()
  {
    return (String[])uselimitation.toArray( new String[uselimitation.size()] );
  }

  /**
   * @see getUseLimitation
   */
  public void addUseLimitation( String uselimitation )
  {
    this.uselimitation.add( uselimitation );
  }

  /**
   * @see getUseLimitation
   */
  public void setUseLimitation( String[] uselimitation )
  {
    this.uselimitation.clear();
    for( int i = 0; i < uselimitation.length; i++ )
    {
      this.uselimitation.add( uselimitation[i] );
    }
  }

  /**
   * to String method
   */
  public String toString()
  {
    String ret = null;
    ret = "otherconstraints = " + otherconstraints + "\n";
    ret += "propertyrightscode = " + propertyrightscode + "\n";
    ret += "useconstraintscode = " + useconstraintscode + "\n";
    ret += "uselimitation = " + uselimitation + "\n";
    return ret;
  }

}