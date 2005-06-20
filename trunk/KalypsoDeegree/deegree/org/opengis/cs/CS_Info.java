/*
 * OpenGIS� Coordinate Transformation Services Implementation Specification Copyright (2001) OpenGIS consortium
 * 
 * THIS COPYRIGHT NOTICE IS A TEMPORARY PATCH. Version 1.00 of official OpenGIS's interface files doesn't contain a
 * copyright notice yet. This file is a slightly modified version of official OpenGIS's interface. Changes have been
 * done in order to fix RMI problems and are documented on the SEAGIS web site (seagis.sourceforge.net). THIS FILE WILL
 * LIKELY BE REPLACED BY NEXT VERSION OF OPENGIS SPECIFICATIONS.
 */
package org.opengis.cs;

// Various JDK's classes
import java.rmi.Remote;
import java.rmi.RemoteException;

/**
 * A base interface for metadata applicable to coordinate system objects. The metadata items 'Abbreviation', 'Alias',
 * 'Authority', 'AuthorityCode', 'Name' and 'Remarks' were specified in the Simple Features interfaces, so they have
 * been kept here.
 * 
 * This specification does not dictate what the contents of these items should be. However, the following guidelines are
 * suggested:
 * <ul>
 * <li>When {@link CS_CoordinateSystemAuthorityFactory}is used to create an object, the 'Authority' and
 * 'AuthorityCode' values should be set to the authority name of the factory object, and the authority code supplied by
 * the client, respectively. The other values may or may not be set. (If the authority is EPSG, the implementer may
 * consider using the corresponding metadata values in the EPSG tables.)</li>
 * <li>When {@link CS_CoordinateSystemFactory}creates an object, the 'Name' should be set to the value supplied by the
 * client. All of the other metadata items should be left empty.</li>
 * </ul>
 * 
 * @version 1.01
 * @since 1.00
 * @author Martin Daly
 */
public interface CS_Info extends Remote
{
  /**
   * Gets the name.
   * 
   * @throws RemoteException
   *           if a remote method call failed.
   */
  String getName() throws RemoteException;

  /**
   * Gets the authority name. An Authority is an organization that maintains definitions of Authority Codes. For example
   * the European Petroleum Survey Group (EPSG) maintains a database of coordinate systems, and other spatial
   * referencing objects, where each object has a code number ID. For example, the EPSG code for a WGS84 Lat/Lon
   * coordinate system is '4326'.
   * 
   * @throws RemoteException
   *           if a remote method call failed.
   */
  String getAuthority() throws RemoteException;

  /**
   * Gets the authority-specific identification code. The AuthorityCode is a compact string defined by an Authority to
   * reference a particular spatial reference object. For example, the European Survey Group (EPSG) authority uses 32
   * bit integers to reference coordinate systems, so all their code strings will consist of a few digits. The EPSG code
   * for WGS84 Lat/Lon is '4326'.
   * 
   * An empty string is used for no code.
   * 
   * @throws RemoteException
   *           if a remote method call failed.
   */
  String getAuthorityCode() throws RemoteException;

  /**
   * Gets the alias.
   * 
   * @throws RemoteException
   *           if a remote method call failed.
   */
  String getAlias() throws RemoteException;

  /**
   * Gets the abbreviation.
   * 
   * @throws RemoteException
   *           if a remote method call failed.
   */
  String getAbbreviation() throws RemoteException;

  /**
   * Gets the provider-supplied remarks.
   * 
   * @throws RemoteException
   *           if a remote method call failed.
   */
  String getRemarks() throws RemoteException;

  /**
   * Gets a Well-Known text representation of this object.
   * 
   * @throws RemoteException
   *           if a remote method call failed.
   */
  String getWKT() throws RemoteException;

  /**
   * Gets an XML representation of this object.
   * 
   * @throws RemoteException
   *           if a remote method call failed.
   */
  String getXML() throws RemoteException;
}