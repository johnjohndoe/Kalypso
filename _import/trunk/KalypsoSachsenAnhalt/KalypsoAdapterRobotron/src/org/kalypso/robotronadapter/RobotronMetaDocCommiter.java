/*--------------- Kalypso-Header ------------------------------------------

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

--------------------------------------------------------------------------*/

package org.kalypso.robotronadapter;

import java.io.File;
import java.util.Map;
import java.util.Properties;

import javax.xml.rpc.ParameterMode;

import org.apache.axis.Constants;
import org.apache.axis.client.Call;
import org.apache.axis.client.Service;
import org.kalypso.metadoc.IMetaDocCommiter;
import org.kalypso.metadoc.impl.MetaDocException;

/**
 * TODO: insert type comment here
 *
 * @author schlienger
 */
public class RobotronMetaDocCommiter implements IMetaDocCommiter
{
  /**
   * @see org.kalypso.metadoc.IMetaDocCommiter#prepareMetainf(java.util.Properties, java.util.Map)
   */
  public void prepareMetainf( final Properties serviceProps, final Map metadata ) throws MetaDocException
  {
    // TODO Auto-generated method stub
  }

  /**
   * @see org.kalypso.metadoc.IMetaDocCommiter#commitDocument(java.util.Properties, java.util.Map, java.io.File)
   */
  public void commitDocument( final Properties serviceProps, final Map metadata, final File doc ) throws MetaDocException
  {
    //Options options = new Options(args);

    String endpoint = "http://localhost:8080/eXForms/KalypsoConnectorWS.jws";
    
    try
    {
      Service service = new Service();
      Call call = (Call)service.createCall();

      call.setTargetEndpointAddress( new java.net.URL( endpoint ) );
      call.setOperationName( "commitDocument" );
      call.addParameter( "docnames", Constants.SOAP_ARRAY, ParameterMode.IN );
      call.addParameter( "metadoc", Constants.XSD_STRING, ParameterMode.IN );

      call.setReturnType( Constants.XSD_STRING );

      String[] docs = new String[4];
      docs[0] = "/files/01";
      docs[1] = "/files/04";
      docs[2] = "/files/03";
      docs[3] = "/files/02";

      String ret = (String)call.invoke( new Object[]
      { docs, "<meta></meta>" } );
      
      System.out.println("Got result : " + ret);
    }
    catch( Exception e )
    {
      // TODO: handle exception
    }
  }
}
