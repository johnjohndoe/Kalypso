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

import org.deegree.services.wcas.metadatadesc.Keywords;
import org.deegree.services.wcas.metadatadesc.TypeCode;

/**
 * Keywords_Impl.java
 * 
 * Created on 16. September 2002, 10:10
 * <p>
 * ----------------------------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:schaefer@lat-lon.de">Axel Schaefer </a>
 * @version $Revision$ $Date$
 */
public class Keywords_Impl implements Keywords
{

  private ArrayList keywords = null;

  private String thesaurusname = null;

  private TypeCode typecode = null;

  /** Creates a new instance of Keywords_Impl */
  public Keywords_Impl( String[] keywords, String thesaurusname, TypeCode typecode )
  {

    this.keywords = new ArrayList();

    setKeywords( keywords );
    setThesaurusName( thesaurusname );
    setTypeCode( typecode );
  }

  /**
   * minOccurs="0" maxOccurs="unbounded"
   * 
   * @return
   */
  public String[] getKeywords()
  {
    return (String[])keywords.toArray( new String[keywords.size()] );
  }

  /**
   * @see getKeywords
   */
  public void addKeyword( String keyword )
  {
    this.keywords.add( keyword );
  }

  /**
   * @see getKeywords
   */
  public void setKeywords( String[] keywords )
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
   */
  public String getThesaurusName()
  {
    return thesaurusname;
  }

  /**
   * @see getThesaurusName
   */
  public void setThesaurusName( String thesaurusname )
  {
    this.thesaurusname = thesaurusname;
  }

  /**
   * minOccurs="0"
   *  
   */
  public TypeCode getTypeCode()
  {
    return typecode;
  }

  /**
   * @see getTypeCode
   */
  public void setTypeCode( TypeCode typecode )
  {
    this.typecode = typecode;
  }

  /**
   * to String method
   */
  public String toString()
  {
    String ret = null;
    ret = "keywords = " + keywords + "\n";
    ret += "thesaurusname = " + thesaurusname + "\n";
    ret += "typecode = " + typecode + "\n";
    return ret;
  }
}