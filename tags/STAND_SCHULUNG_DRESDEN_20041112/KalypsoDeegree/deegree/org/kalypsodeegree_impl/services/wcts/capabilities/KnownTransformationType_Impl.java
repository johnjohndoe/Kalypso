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
package org.deegree_impl.services.wcts.capabilities;

import org.deegree.services.wcts.capabilities.KnownTransformationType;

/**
 * The transformation types and reference systems known to the CTS are defined
 * by the two elements < KnownTransformationType > and <
 * KnownCoordinateReferenceSystem >.
 * <p>
 * ----------------------------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:k.lupp@web.de">Katharina Lupp </a>
 * @version 2002-07-10
 */
public class KnownTransformationType_Impl implements KnownTransformationType
{
  private String authority = null;

  private String code = null;

  /**
   * constructor initializing the class with the <KnownTransformationType>
   */
  KnownTransformationType_Impl( String authority, String code )
  {
    setAuthority( authority );
    setCode( code );
  }

  /**
   * gets the name for the knownCoordinateReferenceSystem
   */
  public String getAuthority()
  {
    return authority;
  }

  /**
   * sets the name for the knownCoordinateReferenceSystem
   */
  public void setAuthority( String authority )
  {
    this.authority = authority;
  }

  /**
   * gets the absract
   */
  public String getCode()
  {
    return code;
  }

  /**
   * sets the absract
   */
  public void setCode( String code )
  {
    this.code = code;
  }

  /**
   * 
   * 
   * @return
   */
  public String toString()
  {
    String ret = null;
    ret = "authority = " + authority + "\n";
    ret += ( "code = " + code + "\n" );
    return ret;
  }
}