/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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
package org.kalypso.model.rcm.internal.binding;

import java.util.Date;

import javax.xml.namespace.QName;

import org.kalypso.commons.tokenreplace.IStringResolver;
import org.kalypso.contribs.java.util.DateUtilities;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.model.rcm.binding.IDateRange;
import org.kalypso.model.rcm.internal.UrlCatalogRcm;
import org.kalypsodeegree_impl.model.feature.Feature_Impl;

/**
 * @author Gernot Belger
 */
public class DateRange extends Feature_Impl implements IDateRange
{
  private static final QName PROPERTY_FROM = new QName( UrlCatalogRcm.NS_RCM, "from" ); //$NON-NLS-1$

  private static final QName PROPERTY_TO = new QName( UrlCatalogRcm.NS_RCM, "to" ); //$NON-NLS-1$

  public DateRange( final Object parent, final IRelationType parentRelation, final IFeatureType ft, final String id, final Object[] propValues )
  {
    super( parent, parentRelation, ft, id, propValues );
  }

  @Override
  public String getFrom( )
  {
    return getProperty( PROPERTY_FROM, String.class );
  }

  @Override
  public void setFrom( final String from )
  {
    setProperty( PROPERTY_FROM, from );
  }

  @Override
  public void setFrom( final Date from )
  {
    final String xmlDate = DateUtilities.printDateTime( from, KalypsoCorePlugin.getDefault().getTimeZone() );
    setProperty( PROPERTY_FROM, xmlDate );
  }

  @Override
  public String getTo( )
  {
    return getProperty( PROPERTY_TO, String.class );
  }

  @Override
  public void setTo( final String to )
  {
    setProperty( PROPERTY_TO, to );
  }

  @Override
  public void setTo( final Date to )
  {
    final String xmlDate = DateUtilities.printDateTime( to, KalypsoCorePlugin.getDefault().getTimeZone() );
    setProperty( PROPERTY_TO, xmlDate );
  }

  @Override
  public org.kalypso.ogc.sensor.DateRange asDateRange( final IStringResolver variables )
  {
    final Date from = getFrom( variables );
    final Date to = getTo( variables );
    return new org.kalypso.ogc.sensor.DateRange( from, to );
  }

  private Date getFrom( final IStringResolver variables )
  {
    return asDate( PROPERTY_FROM, variables );
  }

  private Date getTo( final IStringResolver variables )
  {
    return asDate( PROPERTY_TO, variables );
  }

  private Date asDate( final QName property, final IStringResolver variables )
  {
    final Object value = getProperty( property );
    if( value == null )
      return null;

    final Date date = DateUtilities.toDate( value );
    if( date != null )
      return date;

    if( variables == null )
      return DateUtilities.parseDateTime( value.toString() );

    final String replace = variables.resolve( value.toString() );
    return DateUtilities.parseDateTime( replace );
  }
}