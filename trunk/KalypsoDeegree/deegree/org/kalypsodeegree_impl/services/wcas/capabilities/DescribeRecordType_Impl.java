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

import java.util.ArrayList;

import org.deegree.services.capabilities.DCPType;
import org.deegree.services.wcas.capabilities.DescribeRecordType;

/**
 * The %lt;DescribeFeatureType&gt; tag isused to indicate what schema
 * description languages can be used to describe the schema of a feature type
 * when a client requests such a description. XMLSCHEMA is the only mandatory
 * language that must be available. The SCHEMALANGUAGES entity can be redefined
 * to include vendor specific languages.
 * 
 * <p>
 * -----------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:uzs6tr@uni-bonn.de">Axel Schaefer </a>
 * @version $Revision$ $Date$
 */

public class DescribeRecordType_Impl extends RequestType_Impl implements DescribeRecordType
{

  private ArrayList schemaDescriptionLanguages = null;

  DescribeRecordType_Impl( DCPType[] dcpTypes, String[] schemaDescriptionLanguages )
  {
    super( dcpTypes );
    this.schemaDescriptionLanguages = new ArrayList();
    setSchemaDescriptionLanguages( schemaDescriptionLanguages );
  }

  /**
   * This entity can be redefined to include vendor specific languages.
   * XMLSCHEMA is the only mandatory language that must be available.
   */
  public String[] getSchemaDescriptionLanguages()
  {
    String[] tmp = new String[schemaDescriptionLanguages.size()];
    return (String[])schemaDescriptionLanguages.toArray( tmp );
  }

  /**
   * @see DescribeRecordType_Impl#getSchemaDescriptionLanguages()
   */
  public void setSchemaDescriptionLanguages( String[] schemaDescriptionLanguages )
  {
    this.schemaDescriptionLanguages.clear();
    if( schemaDescriptionLanguages != null )
    {
      for( int i = 0; i < schemaDescriptionLanguages.length; i++ )
      {
        addSchemaDescriptionLanguages( schemaDescriptionLanguages[i] );
      }
    }
  }

  /**
   * @see DescribeRecordType_Impl#getSchemaDescriptionLanguages()
   */
  public void addSchemaDescriptionLanguages( String schemaDescriptionLanguage )
  {
    schemaDescriptionLanguages.add( schemaDescriptionLanguage );
  }

}

/*
 * Changes to this class. What the people haven been up to:
 * 
 * $Log$
 * Revision 1.3  2004/10/07 14:09:04  doemming
 * *** empty log message ***
 *
 * Revision 1.1  2004/09/02 23:57:01  doemming
 * *** empty log message ***
 * Revision 1.3 2004/08/31 12:53:32
 * doemming *** empty log message *** Revision 1.3 2004/03/16 08:07:25 poth no
 * message
 * 
 * Revision 1.2 2004/01/08 09:50:23 poth no message
 * 
 * Revision 1.1.1.1 2002/09/25 16:01:30 poth no message
 * 
 * Revision 1.1 2002/08/19 15:58:38 ap no message
 * 
 * Revision 1.4 2002/04/26 09:02:34 ap no message
 * 
 * Revision 1.2 2002/04/25 16:16:36 ap no message
 * 
 * Revision 1.1 2002/04/04 16:17:15 ap no message
 *  
 */
