/*--------------- Kalypso-Deegree-Header ------------------------------------------------------------

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
 
 
 history:
 
 Files in this package are originally taken from deegree and modified here
 to fit in kalypso. As goals of kalypso differ from that one in deegree
 interface-compatibility to deegree is wanted but not retained always. 
 
 If you intend to use this software in other ways than in kalypso 
 (e.g. OGC-web services), you should consider the latest version of deegree,
 see http://www.deegree.org .

 all modifications are licensed as deegree, 
 original copyright:
 
 Copyright (C) 2001 by:
 EXSE, Department of Geography, University of Bonn
 http://www.giub.uni-bonn.de/exse/
 lat/lon GmbH
 http://www.lat-lon.de
 
 ---------------------------------------------------------------------------------------------------*/
package org.kalypsodeegree_impl.filterencoding;

import org.kalypsodeegree.filterencoding.FilterConstructionException;
import org.kalypsodeegree.filterencoding.FilterEvaluationException;
import org.kalypsodeegree.filterencoding.Operation;
import org.kalypsodeegree.gml.GMLBox;
import org.kalypsodeegree.gml.GMLException;
import org.kalypsodeegree.gml.GMLGeometry;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.xml.ElementList;
import org.kalypsodeegree.xml.XMLTools;
import org.kalypsodeegree_impl.gml.GMLFactory;
import org.kalypsodeegree_impl.gml.GMLGeometry_Impl;
import org.kalypsodeegree_impl.model.geometry.GMLAdapter;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;
import org.w3c.dom.Element;

import com.vividsolutions.jts.geom.Geometry;

/**
 * Encapsulates the information of a spatial_ops entity (as defined in the Filter DTD).
 * <p>
 * 
 * @author <a href="mailto:mschneider@lat-lon.de">Markus Schneider </a>
 * @author <a href="mailto:luigimarinucci@yahoo.com">Luigi Marinucci <a>
 * @version $Id$
 */
public class SpatialOperation extends AbstractOperation
{

  private GMLGeometry m_gmlGeometry;

  private GM_Object geometryLiteral;

  private PropertyName m_propertyName;

  // calvin added on 10/21/2003
  private double m_distance = -1;

  /**
   * Constructs a new SpatialOperation.
   * 
   * @see OperationDefines
   */
  public SpatialOperation( int operatorId, PropertyName propertyName, GMLGeometry gmlGeometry )
  {
    super( operatorId );
    m_propertyName = propertyName;
    m_gmlGeometry = gmlGeometry;
  }

  /**
   * Constructs a new SpatialOperation.
   * 
   * @see OperationDefines Calvin added on 10/21/2003
   */
  public SpatialOperation( int operatorId, PropertyName propertyName, GMLGeometry gmlGeometry, double d )
  {
    super( operatorId );
    this.m_propertyName = propertyName;
    this.m_gmlGeometry = gmlGeometry;
    this.m_distance = d;
  }

  /**
   * returns the distance for geo spatial comparsions such as DWithin or Beyond
   * 
   * @return the distance for geo spatial comparsions such as DWithin or Beyond
   */
  public double getDistance( )
  {
    return m_distance;
  }

  /**
   * Given a DOM-fragment, a corresponding Operation-object is built. This method recursively calls other buildFromDOM () -
   * methods to validate the structure of the DOM-fragment.
   * 
   * @throws FilterConstructionException
   *           if the structure of the DOM-fragment is invalid
   */
  public static Operation buildFromDOM( Element element ) throws FilterConstructionException
  {

    // check if root element's name is a spatial operator
    String name = element.getLocalName();
    int operatorId = OperationDefines.getIdByName( name );

    // every spatial operation has exactly 2 elements
    ElementList children = XMLTools.getChildElements( element );

    if( (children.getLength() != 2) && (operatorId != OperationDefines.DWITHIN) )
    {
      throw new FilterConstructionException( "'" + name + "' requires exactly 2 elements!" );
    }

    // first element must be a PropertyName-Element
    Element child1 = children.item( 0 );
    Element child2 = children.item( 1 );

    if( !child1.getLocalName().toLowerCase().equals( "propertyname" ) )
    {
      throw new FilterConstructionException( "First element of every '" + name + "'-operation must be a " + "'PropertyName'-element!" );
    }

    PropertyName propertyName = (PropertyName) PropertyName.buildFromDOM( child1 );
    final GMLGeometry gmlGeometry = GMLFactory.createGMLGeometry( child2 );

    if( gmlGeometry == null )
    {
      throw new FilterConstructionException( "Unable to parse GMLGeometry definition in '" + name + "'-operation!" );
    }

    // calvin added on 10/21/2003
    double dist = 0;

    if( operatorId == OperationDefines.DWITHIN )
    {
      if( children.getLength() != 3 )
      {
        throw new FilterConstructionException( "'" + name + "' requires exactly 3 elements!" );
      }

      Element child3 = children.item( 2 );

      if( !child3.getLocalName().toLowerCase().equals( "distance" ) )
      {
        throw new FilterConstructionException( "Name of element does not equal 'Distance'!" );
      }

      try
      { // assume the unit can be only metre
        dist = Double.parseDouble( XMLTools.getValue( child3 ) );

        if( dist < 0 )
        {
          throw new FilterConstructionException( "value of  Distance can't be negative:" + XMLTools.getValue( element ) );
        }
      }
      catch( Exception e )
      {
        throw new FilterConstructionException( "value of  Distance is error:" + XMLTools.getValue( element ) );
      }
    }

    switch( operatorId )
    {
      case OperationDefines.CROSSES:
      case OperationDefines.BEYOND:
        throw new FilterConstructionException( "Spatial operator '" + name + "' not implemented!" );
      case OperationDefines.EQUALS:
      case OperationDefines.OVERLAPS:
      case OperationDefines.TOUCHES:
      case OperationDefines.DISJOINT:
      case OperationDefines.INTERSECTS:
      case OperationDefines.WITHIN:
      case OperationDefines.CONTAINS:
      // calvin added on 10/21/2003
      case OperationDefines.DWITHIN:
        // every GMLGeometry is allowed as Literal-argument here
        break;
      case OperationDefines.BBOX:
      {
        if( !(gmlGeometry instanceof GMLBox) )
        {
          throw new FilterConstructionException( "'" + name + "' can only be used with a 'Box'-geometry!" );
        }

        break;
      }
      default:
        throw new FilterConstructionException( "'" + name + "' is not a spatial operator!" );
    }

    return new SpatialOperation( operatorId, propertyName, gmlGeometry, dist );
  }

  /**
   * Returns the geometry property used in the operation and one concrete feature.
   * <p>
   * 
   * @param feature
   * @return the property as a <tt>GM_Object</tt> -object.
   * @throws FilterEvaluationException
   *           if the PropertyName does not denote a GM_Object
   */
  public GM_Object getGeometryProperty( Feature feature ) throws FilterEvaluationException
  {
    Object o = feature.getProperty( m_propertyName.getValue() );

    if( o != null && !(o instanceof GM_Object) )
    {
      throw new FilterEvaluationException( "Specified PropertyName: '" + m_propertyName.getValue() + "' does not denote a geometry object!" );
    }

    return (GM_Object) o;
  }

  /**
   * Returns the geometry literal used in the operation.
   * <p>
   * 
   * @return the literal as a <tt>GM_Object</tt> -object.
   * @throws FilterEvaluationException
   *           if the Literal can not be converted to a GM_Object
   */
  public GM_Object getGeometryLiteral( ) throws FilterEvaluationException
  {
    if( geometryLiteral == null )
    {
      try
      {
        geometryLiteral = GMLAdapter.wrap( m_gmlGeometry );
      }
      catch( GM_Exception e )
      {
        throw new FilterEvaluationException( "Construction of GM_Object from " + "SpatialOperation literal failed: '" + e.getMessage() + "'!" );
      }
    }

    return geometryLiteral;
  }

  /**
   * Returns the geometry literal used in the operation.
   * 
   * @return the literal as a <tt>GMLGeometry</tt> -object.
   */
  public GMLGeometry getGeometry( )
  {
    return m_gmlGeometry;
  }

  /**
   * Returns the (bounding) box of a BBOX operation.
   * 
   * @deprecated replaced by {@link #getGeometry()}
   */
  @Deprecated
  public GM_Envelope getBoundingBox( )
  {
    GM_Envelope box = GMLAdapter.createGM_Envelope( (GMLBox) m_gmlGeometry );

    return box;
  }

  /**
   * returns the name of the (spatial) property that shall be use for geo spatial comparsions
   */
  public PropertyName getPropertyName( )
  {
    return m_propertyName;
  }

  /** Produces an indented XML representation of this object. */
  public StringBuffer toXML( )
  {
    StringBuffer sb = new StringBuffer( 2000 );
    sb.append( "<ogc:" ).append( getOperatorName() );
    sb.append( " xmlns:gml='http://www.opengis.net/gml' " ).append( ">" );
    sb.append( m_propertyName.toXML() );

    Element element = ((GMLGeometry_Impl) m_gmlGeometry).getAsElement();
    XMLTools.appendNode( element, "", sb );
    sb.append( "</ogc:" ).append( getOperatorName() ).append( ">" );

    return sb;
  }

  /**
   * Calculates the <tt>SpatialOperation</tt>'s logical value based on the property values of the given
   * <tt>Feature</tt>.
   * <p>
   * TODO: Implement operations: CROSSES, BEYOND, OVERLAPS AND TOUCHES.
   * <p>
   * 
   * @param feature
   *          that determines the property values
   * @return true, if the <tt>SpatialOperation</tt> evaluates to true, else false
   * @throws FilterEvaluationException
   *           if the evaluation fails
   */
  public boolean evaluate( Feature feature ) throws FilterEvaluationException
  {
    boolean value = false;

    GM_Object geom = getGeometryProperty( feature );
    if( geom == null )
      return false;

    switch( m_operatorId )
    {
      case OperationDefines.EQUALS:
        value = getGeometryProperty( feature ).equals( getGeometryLiteral() );
      case OperationDefines.DISJOINT:
      {
        try
        {
          Geometry geomNotToIntersectWith = JTSAdapter.export( getGeometryLiteral() );
          Geometry geomTestNotToIntersect = JTSAdapter.export( getGeometryProperty( feature ) );
          value = !geomNotToIntersectWith.intersects( geomTestNotToIntersect );
        }
        catch( GM_Exception e )
        {
          e.printStackTrace();
        }
        break;
      }
      case OperationDefines.WITHIN:
      {
        value = getGeometryLiteral().contains( getGeometryProperty( feature ) );
        break;
      }
      case OperationDefines.CONTAINS:
      {
        try
        {
          Geometry geomWhoContains = JTSAdapter.export( getGeometryLiteral() );
          Geometry geomIsContained = JTSAdapter.export( getGeometryProperty( feature ) );
          value = geomWhoContains.contains( geomIsContained );
        }
        catch( GM_Exception e )
        {
          e.printStackTrace();
        }
        break;
      }
      case OperationDefines.INTERSECTS:
      {
        try
        {
          Geometry geomWhoIntersects = JTSAdapter.export( getGeometryLiteral() );
          Geometry geomIsIntersecting = JTSAdapter.export( getGeometryProperty( feature ) );
          value = geomWhoIntersects.intersects( geomIsIntersecting );
        }
        catch( GM_Exception e )
        {
          e.printStackTrace();
        }
      }
      case OperationDefines.BBOX:
      {
        value = getGeometryProperty( feature ).intersects( getGeometryLiteral() );
        break;
      }
      // calvin added on 10/21/2003
      case OperationDefines.DWITHIN:
      {
        value = getGeometryProperty( feature ).isWithinDistance( getGeometryLiteral(), m_distance );
        break;
      }
      case OperationDefines.CROSSES:
        try
        {
          Geometry geomToCross = JTSAdapter.export( getGeometryLiteral() );
          Geometry geomThatCrosses = JTSAdapter.export( getGeometryProperty( feature ) );
          value = geomToCross.crosses( geomThatCrosses );
        }
        catch( GM_Exception e )
        {
          e.printStackTrace();
        }
      case OperationDefines.BEYOND:
      case OperationDefines.OVERLAPS:
        try
        {
          Geometry geomToOverlap = JTSAdapter.export( getGeometryLiteral() );
          Geometry geomOverlapping = JTSAdapter.export( getGeometryProperty( feature ) );
          value = geomToOverlap.overlaps( geomOverlapping );
        }
        catch( GM_Exception e )
        {
          e.printStackTrace();
        }
      case OperationDefines.TOUCHES:
        try
        {
          Geometry geomWhoTouches = JTSAdapter.export( getGeometryLiteral() );
          Geometry geomIsTouched = JTSAdapter.export( getGeometryProperty( feature ) );
          value = geomWhoTouches.touches( geomIsTouched );
        }
        catch( GM_Exception e )
        {
          e.printStackTrace();
        }
        throw new FilterEvaluationException( "Evaluation for spatial " + "operation '" + OperationDefines.getNameById( m_operatorId ) + "' is not implemented yet!" );
      default:
        throw new FilterEvaluationException( "Encountered unexpected " + "operatorId: " + m_operatorId + " in SpatialOperation.evaluate ()!" );
    }

    return value;
  }

  // CK: I added the set methods to change the filter element when it exists as an Object only. These changes must be
  // reflected when saving the filter somewhere this means the filter has to be exported as a XML in the source
  // Document.
  // (August 2005)

  public void setProperty( PropertyName name )
  {
    m_propertyName = name;
  }

  public void setGeometry( GMLGeometry geom ) throws GM_Exception
  {
    m_gmlGeometry = geom;
    geometryLiteral = GMLAdapter.wrap( geom );
  }

  public void setGeometry( GM_Object geom ) throws GMLException
  {
    geometryLiteral = geom;
    m_gmlGeometry = GMLFactory.createGMLGeometry( null, geom );

  }

  public void setOperatorId( int opearationId )
  {
    m_operatorId = opearationId;

  }

  public void setDistacnce( double distance )
  {
    m_distance = distance;
  }
}