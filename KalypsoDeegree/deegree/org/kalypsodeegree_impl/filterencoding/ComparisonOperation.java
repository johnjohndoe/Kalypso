/** This file is part of kalypso/deegree.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 * history:
 * 
 * Files in this package are originally taken from deegree and modified here
 * to fit in kalypso. As goals of kalypso differ from that one in deegree
 * interface-compatibility to deegree is wanted but not retained always. 
 * 
 * If you intend to use this software in other ways than in kalypso 
 * (e.g. OGC-web services), you should consider the latest version of deegree,
 * see http://www.deegree.org .
 *
 * all modifications are licensed as deegree, 
 * original copyright:
 *
 * Copyright (C) 2001 by:
 * EXSE, Department of Geography, University of Bonn
 * http://www.giub.uni-bonn.de/exse/
 * lat/lon GmbH
 * http://www.lat-lon.de
 */
package org.kalypsodeegree_impl.filterencoding;

import org.kalypsodeegree.filterencoding.FilterConstructionException;
import org.kalypsodeegree.filterencoding.Operation;
import org.w3c.dom.Element;

/**
 * Encapsulates the information of a comparison_ops entity (as defined in the Filter DTD).
 * 
 * @author Markus Schneider
 * @version 06.08.2002
 */
public abstract class ComparisonOperation extends AbstractOperation
{

  ComparisonOperation( final int operationId )
  {
    super( operationId );
  }

  /**
   * Given a DOM-fragment, a corresponding Operation-object is built. This method recursively calls other buildFromDOM () -
   * methods to validate the structure of the DOM-fragment.
   * 
   * @throws FilterConstructionException
   *             if the structure of the DOM-fragment is invalid
   */
  public static Operation buildFromDOM( final Element element ) throws FilterConstructionException
  {

    // check if root element's name is a known operator
    final String name = element.getLocalName();
    final int operatorId = OperationDefines.getIdByName( name );
    ComparisonOperation operation = null;

    switch( operatorId )
    {
      case OperationDefines.PROPERTYISEQUALTO:
      case OperationDefines.PROPERTYISNOTEQUALTO:
      case OperationDefines.PROPERTYISLESSTHAN:
      case OperationDefines.PROPERTYISGREATERTHAN:
      case OperationDefines.PROPERTYISLESSTHANOREQUALTO:
      case OperationDefines.PROPERTYISGREATERTHANOREQUALTO:
      {
        operation = (ComparisonOperation) PropertyIsCOMPOperation.buildFromDOM( element );
        break;
      }
      case OperationDefines.PROPERTYISLIKE:
      {
        operation = (ComparisonOperation) PropertyIsLikeOperation.buildFromDOM( element );
        break;
      }
      case OperationDefines.PROPERTYISNULL:
      {
        operation = (ComparisonOperation) PropertyIsNullOperation.buildFromDOM( element );
        break;
      }
      case OperationDefines.PROPERTYISBETWEEN:
      {
        operation = (ComparisonOperation) PropertyIsBetweenOperation.buildFromDOM( element );
        break;
      }
      default:
      {
        throw new FilterConstructionException( "'" + name + "' is not a comparison operator!" );
      }
    }
    return operation;
  }
}