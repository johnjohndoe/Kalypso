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
package org.deegree_impl.services.wcts.protocol;

import org.deegree.services.wcts.protocol.TransformationSequence;

/**
 * Is the CTS able to execute user-defined transformations, a succession of
 * transformation-steps can be defined by the &lt;TransformationSequence&gt;
 * element. Each transformation-step has to be described by its name and the
 * parameters used.
 * <p>
 * ----------------------------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @version 2002-07-26
 *  
 */
public class TransformationSequence_Impl implements TransformationSequence
{
  private String code = null;

  private String codespace = null;

  /**
   * constructor constructing with the code and the codespace
   */
  TransformationSequence_Impl( String code, String codespace )
  {
    setCode( code );
    setCodeSpace( codespace );
  }

  /**
   * gets the Code-element
   */
  public String getCode()
  {
    return code;
  }

  /**
   * @see #getCode()
   */
  public void setCode( String code )
  {
    this.code = code;
  }

  /**
   * gets the CodeSpace-element
   */
  public String getCodeSpace()
  {
    return codespace;
  }

  /**
   * @see #getCodeSpace()
   */
  public void setCodeSpace( String codespace )
  {
    this.codespace = codespace;
  }
}