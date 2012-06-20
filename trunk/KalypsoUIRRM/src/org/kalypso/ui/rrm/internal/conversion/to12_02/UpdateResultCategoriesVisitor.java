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
package org.kalypso.ui.rrm.internal.conversion.to12_02;

import org.apache.commons.lang3.StringUtils;
import org.kalypso.model.hydrology.binding.model.nodes.Node;
import org.kalypso.ogc.sensor.util.ZmlLink;
import org.kalypso.zml.obslink.TimeseriesLinkType;
import org.kalypsodeegree.model.feature.IFeatureBindingCollectionVisitor;

/**
 * Sets empty result categories by existing qberechnet properties.
 * 
 * @author Dirk Kuch
 */
public class UpdateResultCategoriesVisitor implements IFeatureBindingCollectionVisitor<Node>
{
  @Override
  public void visit( final Node node )
  {
    /* Result category exists? So leave existing result category. */
    if( StringUtils.isNotBlank( node.getResultCategory() ) )
      return;

    final String href = getHref( node );
    if( StringUtils.isBlank( href ) )
      return;

    final String category = doCategorize( href );
    node.setProperty( Node.PROPERTY_RESULT_CATEGORY, category );
  }

  private String doCategorize( final String href )
  {
    return "Pegel"; //$NON-NLS-1$

    // final CharMatcher matcher = new CharMatcher()
    // {
    // @Override
    // public boolean matches( final char c )
    // {
    // switch( c )
    // {
    //          case '/': //$NON-NLS-1$
    //          case '\\': //$NON-NLS-1$
    // return true;
    // }
    //
    // return false;
    // }
    // };
    //
    // /* keep existing manually set zml file name (notation = ${FILE_NAME} */
    // final Iterable<String> splitted = Splitter.on( matcher ).trimResults().omitEmptyStrings().split( href );
    // String name = Iterables.getLast( splitted );
    // if( name.contains( "." ) )
    // name = name.substring( 0, name.indexOf( '.' ) );
    //
    // return String.format( "Pegel/${%s}", name ); //$NON-NLS-1$

  }

  private String getHref( final Node node )
  {
    final ZmlLink link = node.getResultLink();
    if( link == null )
      return null;

    final TimeseriesLinkType timeseriesLink = link.getTimeseriesLink();
    if( timeseriesLink == null )
      return null;

    return timeseriesLink.getHref();
  }
}