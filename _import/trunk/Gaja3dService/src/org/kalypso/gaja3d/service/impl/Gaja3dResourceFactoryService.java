/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
 *  21073 Hamburg, Germany
 *  http://www.tuhh.de/wb
 * 
 *  and
 * 
 *  Bjoernsen Consulting Engineers (BCE)
 *  Maria Trost 3
 *  56070 Koblenz, Germany
 *  http://www.bjoernsen.de
 * 
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 * 
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 * 
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 * 
 *  Contact:
 * 
 *  E-Mail:
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 * 
 *  ---------------------------------------------------------------------------*/
package org.kalypso.gaja3d.service.impl;

import java.net.URL;
import java.rmi.RemoteException;

import org.apache.axis.AxisFault;
import org.apache.axis.MessageContext;
import org.apache.axis.message.addressing.EndpointReferenceType;
import org.globus.wsrf.ResourceContext;
import org.globus.wsrf.ResourceKey;
import org.globus.wsrf.container.ServiceHost;
import org.globus.wsrf.utils.AddressingUtils;
import org.kalypso.gaja3d.service.factory.stubs.CreateResource;
import org.kalypso.gaja3d.service.factory.stubs.CreateResourceResponse;

public class Gaja3dResourceFactoryService
{

  /* Implementation of createResource Operation */
  public CreateResourceResponse createResource( @SuppressWarnings("unused") CreateResource request ) throws RemoteException
  {
    ResourceKey key = null;

    /* First, we create a new Gaja3dResource through the Gaja3dResourceHome */
    try
    {
      final ResourceContext ctx = ResourceContext.getResourceContext();
      final Gaja3dResourceHome home = (Gaja3dResourceHome) ctx.getResourceHome();
      key = home.create();
    }
    catch( final Throwable e )
    {
      throw new AxisFault( "Could not create resource key.", e );
    }

    EndpointReferenceType epr = null;

    /*
     * We construct the instance's endpoint reference. The instance's service path can be found in the WSDD file as a
     * parameter.
     */
    try
    {
      final URL baseURL = ServiceHost.getBaseURL();
      final String instanceService = (String) MessageContext.getCurrentContext().getService().getOption( "instance" );
      final String instanceURI = baseURL.toString() + instanceService;
      // The endpoint reference includes the instance's URI and the
      // resource key
      epr = AddressingUtils.createEndpointReference( instanceURI, key );
    }
    catch( final Exception e )
    {
      throw new AxisFault( "Could not create EPR.", e );
    }

    /* Finally, return the endpoint reference in a CreateResourceResponse */
    final CreateResourceResponse response = new CreateResourceResponse();
    response.setEndpointReference( epr );
    return response;
  }
}