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
package org.kalypso.model.flood.ui.map;

import java.util.List;

import org.kalypso.model.flood.binding.IFloodPolygon;
import org.kalypso.model.flood.i18n.Messages;
import org.kalypsodeegree.filterencoding.Expression;
import org.kalypsodeegree.filterencoding.FilterEvaluationException;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree_impl.filterencoding.AbstractFunctionExpression;

/**
 * @author Gernot Belger
 * @author Thomas Jung
 */
public class EventFilterExpression extends AbstractFunctionExpression
{
  /**
   * @see org.kalypsodeegree.filterencoding.IFunctionExpression#evaluate(org.kalypsodeegree.model.feature.Feature,
   *      java.util.List)
   */
  @Override
  public Object evaluate( final Feature feature, final List<Expression> args ) throws FilterEvaluationException
  {
    if( args.size() != 1 )
    {
      System.out.println( Messages.getString("org.kalypso.model.flood.ui.map.EventFilterExpression.0") + getName() ); //$NON-NLS-1$
      return false;
    }

    final IFloodPolygon polygon = (IFloodPolygon) feature.getAdapter( IFloodPolygon.class );

    final Expression eventIdExpr = args.get( 0 );

    final String eventId = (String) eventIdExpr.evaluate( feature );

    return Boolean.toString( polygon.appliesToEvent( eventId ) );
  }

}
