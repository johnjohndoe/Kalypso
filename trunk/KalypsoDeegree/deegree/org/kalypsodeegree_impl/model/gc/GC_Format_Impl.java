/*
 * OpenGIS® Grid Coverage Implementation Specification
 * 
 * This Java profile is derived from OpenGIS's specification available on their
 * public web site:
 * 
 * http://www.opengis.org/techno/implementation.htm
 * 
 * You can redistribute it, but should not modify it unless for greater OpenGIS
 * compliance.
 */
package org.deegree_impl.model.gc;

import java.io.Serializable;
import java.rmi.RemoteException;

import org.opengis.gc.GC_Format;
import org.opengis.gc.GC_ParameterInfo;

/**
 * This interface is a discovery mechanism to determine the formats supported by
 * a {@link GC_GridCoverageExchange}implementation. A
 * <code>GC_GridCoverageExchange</code> implementation can support a number of
 * file format or resources.
 * 
 * @author Andreas Poth
 * @version 1.00
 * @since 1.00
 */
class GC_Format_Impl implements GC_Format, Serializable
{
  private String description = null;

  private String docURL = null;

  private String name = null;

  private int numParameters = -1;

  private GC_ParameterInfo parameterInfo = null;

  private String vendor = null;

  private String version = null;

  GC_Format_Impl( String description, String docURL, String name, int numParameters,
      GC_ParameterInfo parameterInfo, String vendor, String version )
  {
    this.description = description;
    this.docURL = docURL;
    this.name = name;
    this.numParameters = numParameters;
    this.parameterInfo = parameterInfo;
    this.vendor = vendor;
    this.version = version;
  }

  /**
   * Description of the file format. If no description, the value will be a null
   * or empty string.
   * 
   * @return the description of the file format.
   * @throws RemoteException
   *           if a remote method call failed.
   *  
   */
  public String getDescription() throws RemoteException
  {
    return description;
  }

  /**
   * Documentation URL for the format.
   * 
   * @return the documentation URL for the format.
   * @throws RemoteException
   *           if a remote method call failed.
   *  
   */
  public String getDocURL() throws RemoteException
  {
    return docURL;
  }

  /**
   * Name of the file format. This name is used as the name of the file in the
   * {@link GC_GridCoverageExchange#exportTo exportTo}operation.
   * 
   * @return the name of the file format.
   * @throws RemoteException
   *           if a remote method call failed.
   *  
   */
  public String getName() throws RemoteException
  {
    return name;
  }

  /**
   * Number of optional parameters for the
   * {@link GC_GridCoverageExchange#exportTo exportTo}operation.
   * 
   * @return the number of optional parameters for the exportTo operation.
   * @throws RemoteException
   *           if a remote method call failed.
   *  
   */
  public int getNumParameters() throws RemoteException
  {
    return numParameters;
  }

  /**
   * Retrieve the parameter information for a given index.
   * 
   * @param index
   *          Index to the parameter.
   * @return the parameter information for the given index.
   * @throws RemoteException
   *           if a remote method call failed.
   *  
   */
  public GC_ParameterInfo getParameterInfo( int index ) throws RemoteException
  {
    return parameterInfo;
  }

  /**
   * Vendor or agency for the format.
   * 
   * @return the vendor or agency for the format.
   * @throws RemoteException
   *           if a remote method call failed.
   *  
   */
  public String getVendor() throws RemoteException
  {
    return vendor;
  }

  /**
   * Version number of the format.
   * 
   * @return the version number of the format.
   * @throws RemoteException
   *           if a remote method call failed.
   *  
   */
  public String getVersion() throws RemoteException
  {
    return version;
  }

}