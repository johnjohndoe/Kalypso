// $Header:
// /var/lib/cvs/backupdeegree/deegree/org/deegree_impl/services/wcas/capabilities/TaxonomyType_Impl.java,v
// 1.1.1.1 2004/05/11 16:43:26 doemming Exp $
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
package org.deegree_impl.services.wcas.capabilities;

import java.net.URL;

import org.deegree.services.wcas.capabilities.TaxonomyType;

/**
 * 
 * @version $Revision$
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @author last edited by: $Author$
 * 
 * @version 1.0. $Revision$, $Date$
 * 
 * @since 1.1
 */
final class TaxonomyType_Impl implements TaxonomyType
{

  private String abstract_ = null;

  private String languageCode = null;

  private String name = null;

  private String title = null;

  private URL taxonomyURL = null;

  /**
   * @param name
   * @param title
   * @param abstract_
   * @param languageCode
   * @param taxonomyURL
   */
  TaxonomyType_Impl( String name, String title, String abstract_, String languageCode,
      URL taxonomyURL )
  {
    setAbstract( abstract_ );
    setLanguageCode( languageCode );
    setName( name );
    setTitle( title );
    setTaxonomyURL( taxonomyURL );
  }

  /**
   * returns an abstract text describing the taxonomy
   *  
   */
  public String getAbstract()
  {
    return abstract_;
  }

  /**
   * @see TaxonomyType_Impl#getAbstract()
   */
  public void setAbstract( String abstract_ )
  {
    this.abstract_ = abstract_;
  }

  /**
   * returns a language code identifying the language the taxonomy is build at
   *  
   */
  public String getLanguageCode()
  {
    return languageCode;
  }

  /**
   * @see TaxonomyType_Impl#getLanguageCode()
   */
  public void setLanguageCode( String languageCode )
  {
    this.languageCode = languageCode;
  }

  /**
   * returns the name of the taxonomy
   *  
   */
  public String getName()
  {
    return name;
  }

  /**
   * @see TaxonomyType_Impl#getName()
   */
  public void setName( String name )
  {
    this.name = name;
  }

  /**
   * returns the <tt>URL<tt> a client can access the taxonomy as an XML
   * document.
   *
   */
  public URL getTaxonomyURL()
  {
    return taxonomyURL;
  }

  /**
   * @see TaxonomyType_Impl#getTaxonomyURL()
   */
  public void setTaxonomyURL( URL taxonomyURL )
  {
    this.taxonomyURL = taxonomyURL;
  }

  /**
   * returns the title of the taxonomy
   *  
   */
  public String getTitle()
  {
    return title;
  }

  /**
   * @see TaxonomyType_Impl#getTitle()
   */
  public void setTitle( String title )
  {
    this.title = title;
  }

}