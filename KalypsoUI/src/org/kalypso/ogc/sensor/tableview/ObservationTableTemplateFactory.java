/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and
 
 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

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

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de
  
---------------------------------------------------------------------------------------------------*/
package org.kalypso.ogc.sensor.tableview;

import java.io.InputStream;

import javax.xml.bind.JAXBException;

import org.apache.commons.io.IOUtils;
import org.kalypso.template.obstableview.ObjectFactory;
import org.kalypso.template.obstableview.ObstableviewType;

/**
 * ObservationTableTemplateFactory
 * 
 * @author schlienger
 */
public class ObservationTableTemplateFactory
{
  private final static ObjectFactory OF = new ObjectFactory();
  
  
  private ObservationTableTemplateFactory( )
  {
    // not instanciation
  }

  /**
   * Loads the xml template from the given stream. Closes the stream.
   * 
   * @param ins
   * @return table view template
   * @throws JAXBException
   */
  public static ObstableviewType loadTableTemplateXML( final InputStream ins ) throws JAXBException
  {
    try
    {
      final ObstableviewType baseTemplate = (ObstableviewType) OF
          .createUnmarshaller().unmarshal( ins );
      
      return baseTemplate;
    }
    finally
    {
      IOUtils.closeQuietly( ins );
    }
  }
}