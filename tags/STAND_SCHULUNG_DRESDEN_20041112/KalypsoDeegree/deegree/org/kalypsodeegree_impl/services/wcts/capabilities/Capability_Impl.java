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
package org.deegree_impl.services.wcts.capabilities;

import java.util.ArrayList;

import org.deegree.services.wcts.capabilities.Capability;
import org.deegree.services.wcts.capabilities.KnownCoordinateReferenceSystem;
import org.deegree.services.wcts.capabilities.KnownTransformationType;
import org.deegree.services.wcts.capabilities.WCTS_Request;

/**
 * This section of the Capabilities-document describes the operations made
 * available by the CTS. Apart from the operations defined as mandatory
 * (GetCapabilities, IsTransformable, Transform), the optional operation
 * DescribeTransformation can be described within the element &lt;Request&gt;.
 * 
 * <p>
 * ----------------------------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:k.lupp@web.de">Katharina Lupp </a>
 * @version 2002-07-10
 */
public class Capability_Impl implements Capability
{
  private ArrayList knownCoordinateReferenceSystems = null;

  private ArrayList knownTransformationTypes = null;

  private ArrayList vendorSpecificCapabilities = null;

  private WCTS_Request request = null;

  private boolean userDefinedCoordinateSystems = false;

  private boolean userDefinedTransformations = false;

  /**
   * constructor initializing the class with the <Capability>
   */
  Capability_Impl( boolean userDefinedCoordinateSystems, boolean userDefinedTransformations,
      WCTS_Request request, KnownTransformationType[] knownTransformationTypes,
      KnownCoordinateReferenceSystem[] knownCoordinateReferenceSystems,
      String[] vendorSpecificCapabilities )
  {
    this.knownTransformationTypes = new ArrayList();
    this.knownCoordinateReferenceSystems = new ArrayList();
    this.vendorSpecificCapabilities = new ArrayList();

    setUserDefinedCoordinateSystems( userDefinedCoordinateSystems );
    setUserDefinedTransformations( userDefinedTransformations );
    setRequest( request );
    setKnownTransformationTypes( knownTransformationTypes );
    setKnownCoordinateReferenceSystems( knownCoordinateReferenceSystems );
    setVendorSpecificCapabilities( vendorSpecificCapabilities );
  }

  /**
   * <p>
   * In many cases, it is possible to perform a transformation of coordinates
   * from one reference system into another in different (mathematical) kinds.
   * Under normal conditions the CTS selects a suitable procedure on the basis
   * the source and the target reference system and accomplishes the
   * transformation. For different reasons it can make sense for a user to
   * specify the transformation steps themselves.
   * 
   * <p>
   * default="false"
   */
  public boolean getUserDefinedCoordinateSystems()
  {
    return userDefinedCoordinateSystems;
  }

  /**
   * @see Capability_Impl#getUserDefinedCoordinateSystems()
   */
  public void setUserDefinedCoordinateSystems( boolean userDefinedCoordinateSystems )
  {
    this.userDefinedCoordinateSystems = userDefinedCoordinateSystems;
  }

  /**
   * <p>
   * In many cases, it is possible to perform a transformation of coordinates
   * from one reference system into another in different (mathematical) kinds.
   * Under normal conditions the CTS selects a suitable procedure on the basis
   * the source and the target reference system and accomplishes the
   * transformation. For different reasons it can make sense for a user to
   * specify the transformation steps themselves.
   * 
   * <p>
   * default="false"
   */
  public boolean getUserDefinedTransformations()
  {
    return userDefinedTransformations;
  }

  /**
   * @see Capability_Impl#getUserDefinedTransformations()
   */
  public void setUserDefinedTransformations( boolean userDefinedTransformations )
  {
    this.userDefinedTransformations = userDefinedTransformations;
  }

  /**
   * A Coordinate transformation service (CTS) acts as Web service, which knows
   * the four different Requests
   */
  public WCTS_Request getRequest()
  {
    return request;
  }

  /**
   * @see Capability_Impl#getRequest()
   */
  public void setRequest( WCTS_Request request )
  {
    this.request = request;
  }

  /**
   * <p>
   * Each CTS must know at least one transformation-type. These concern
   * fundamental methods for the transformation of coordinates out of a
   * reference system into another. It <b>doesn't </b> concern concrete
   * transformations between two determined references system.
   * <p>
   * Examples of kinds of transformation are: Longitude rotation, Abridged
   * Molodenski, Geocentric_to_Ellipsoid etc...
   */
  public KnownTransformationType[] getKnownTransformationTypes()
  {
    return (KnownTransformationType[])knownTransformationTypes
        .toArray( new KnownTransformationType[knownTransformationTypes.size()] );
  }

  /**
   * adds a &lt;knownTransformationType&gt;
   */
  public void addKnownTransformationType( KnownTransformationType knownTransformationTypes )
  {
    this.knownTransformationTypes.add( knownTransformationTypes );
  }

  /**
   * sets the &lt;knownTransformationTypes&gt;
   */
  public void setKnownTransformationTypes( KnownTransformationType[] knownTransformationTypes )
  {
    this.knownTransformationTypes.clear();

    if( knownTransformationTypes != null )
    {
      for( int i = 0; i < knownTransformationTypes.length; i++ )
      {
        this.knownTransformationTypes.add( knownTransformationTypes[i] );
      }
    }
  }

  /**
   * Each CTS must know at least <b>two </b> spatial reference-System, so that
   * at least one transformation is possible.
   */
  public KnownCoordinateReferenceSystem[] getKnownCoordinateReferenceSystems()
  {
    return (KnownCoordinateReferenceSystem[])knownCoordinateReferenceSystems
        .toArray( new KnownCoordinateReferenceSystem[knownCoordinateReferenceSystems.size()] );
  }

  /**
   * adds the &lt;KnownCoordinateReferenceSystem&gt;
   */
  public void addKnownCoordinateReferenceSystem(
      KnownCoordinateReferenceSystem knownCoordinateReferenceSystem )
  {
    this.knownCoordinateReferenceSystems.add( knownCoordinateReferenceSystem );
  }

  /**
   * sets the &lt;KnownCoordinateReferenceSystem&gt;
   */
  public void setKnownCoordinateReferenceSystems(
      KnownCoordinateReferenceSystem[] knownCoordinateReferenceSystems )
  {
    this.knownCoordinateReferenceSystems.clear();

    if( knownCoordinateReferenceSystems != null )
    {
      for( int i = 0; i < knownCoordinateReferenceSystems.length; i++ )
      {
        this.knownCoordinateReferenceSystems.add( knownCoordinateReferenceSystems[i] );
      }
    }
  }

  /**
   * With the &lt;VendorSpecificCapabilities&gt; element, it is possible to
   * define additional Capabilities.
   */
  public String[] getVendorSpecificCapabilities()
  {
    return (String[])vendorSpecificCapabilities.toArray( new String[vendorSpecificCapabilities
        .size()] );
  }

  /**
   * adds the &lt;VendorSpecificCapabilities&gt;
   */
  public void addVendorSpecificCapability( String vendorSpecificCapability )
  {
    this.vendorSpecificCapabilities.add( vendorSpecificCapability );
  }

  /**
   * sets the &lt;VendorSpecificCapabilities&gt;
   */
  public void setVendorSpecificCapabilities( String[] vendorSpecificCapabilities )
  {
    this.vendorSpecificCapabilities.clear();

    if( vendorSpecificCapabilities != null )
    {
      for( int i = 0; i < vendorSpecificCapabilities.length; i++ )
      {
        this.vendorSpecificCapabilities.add( vendorSpecificCapabilities[i] );
      }
    }
  }

  /**
   * 
   * 
   * @return
   */
  public String toString()
  {
    String ret = null;
    ret = "userDefinedCoordinateSystems = " + userDefinedCoordinateSystems + "\n";
    ret += ( "userDefinedTransformations = " + userDefinedTransformations + "\n" );
    ret += ( "request = " + request + "\n" );
    ret += ( "knownTransformationTypes = " + knownTransformationTypes + "\n" );
    ret += ( "knownCoordinateReferenceSystems = " + knownCoordinateReferenceSystems + "\n" );
    ret += ( "vendorSpecificCapabilities = " + vendorSpecificCapabilities + "\n" );
    return ret;
  }
}