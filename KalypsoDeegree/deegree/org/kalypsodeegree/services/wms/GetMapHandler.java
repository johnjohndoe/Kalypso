package org.deegree.services.wms;

import org.deegree.services.OGCWebServiceResponse;
import org.deegree.services.WebServiceException;

public interface GetMapHandler
{

  public OGCWebServiceResponse performGetMap() throws WebServiceException;

}