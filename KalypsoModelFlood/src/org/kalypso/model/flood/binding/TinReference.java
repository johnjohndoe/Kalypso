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
package org.kalypso.model.flood.binding;

import java.math.BigDecimal;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.Date;

import javax.xml.datatype.XMLGregorianCalendar;

import org.eclipse.core.runtime.IStatus;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.java.util.DateUtilities;
import org.kalypso.model.flood.KalypsoModelFloodPlugin;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree.model.geometry.GM_Triangle;
import org.kalypsodeegree.model.geometry.GM_TriangulatedSurface;
import org.kalypsodeegree_impl.gml.binding.commons.AbstractFeatureBinder;
import org.kalypsodeegree_impl.model.feature.gmlxpath.GMLXPath;

/**
 * @author Gernot Belger
 */
public class TinReference extends AbstractFeatureBinder implements ITinReference
{
  public TinReference( final Feature featureToBind )
  {
    super( featureToBind, QNAME );
  }

  /**
   * @see org.kalypso.model.flood.binding.ITinReference#getValue(org.kalypsodeegree.model.geometry.GM_Position)
   */
  @Override
  public double getValue( final GM_Position pos )
  {
    final GM_TriangulatedSurface tin = getTin();

    return tin.getValue( pos );
  }

  /**
   * @see org.kalypso.model.flood.binding.ITinReference#getTin()
   */
  @Override
  public GM_TriangulatedSurface getTin( )
  {
    return (GM_TriangulatedSurface) getFeature().getProperty( QNAME_PROP_TIN );
  }

  /**
   * @see org.kalypso.model.flood.binding.ITinReference#getMax()
   */
  @Override
  public BigDecimal getMax( )
  {
    return getProperty( QNAME_PROP_MAX, BigDecimal.class );
  }

  /**
   * @see org.kalypso.model.flood.binding.ITinReference#getMin()
   */
  @Override
  public BigDecimal getMin( )
  {
    return getProperty( QNAME_PROP_MIN, BigDecimal.class );
  }

  @Override
  public void setSourceFeaturePath( final GMLXPath path )
  {
    setProperty( QNAME_PROP_SOURCE_PATH, path == null ? null : path.toString() );
  }

  /**
   * @see org.kalypso.model.flood.binding.ITinReference#getSourceFeaturePath()
   */
  @Override
  public GMLXPath getSourceFeaturePath( )
  {
    final String path = getProperty( QNAME_PROP_SOURCE_PATH, String.class );

    if( path == null )
      return null;

    final GMLWorkspace workspace = getFeature().getWorkspace();

    return new GMLXPath( path, workspace.getNamespaceContext() );
  }

  /**
   * @see org.kalypso.model.flood.binding.ITinReference#getSourceLocation()
   */
  @Override
  public URL getSourceLocation( )
  {
    final String uri = getProperty( QNAME_PROP_SOURCE_LOCATION, String.class );
    if( uri == null )
      return null;

    try
    {
      final GMLWorkspace workspace = getFeature().getWorkspace();
      return new URL( workspace.getContext(), uri );
    }
    catch( final MalformedURLException e )
    {
      e.printStackTrace();

      final IStatus status = StatusUtilities.statusFromThrowable( e );
      KalypsoModelFloodPlugin.getDefault().getLog().log( status );

      return null;
    }
  }

  /**
   * @see org.kalypso.model.flood.binding.ITinReference#getUpdateDate()
   */
  @Override
  public Date getUpdateDate( )
  {
    final XMLGregorianCalendar date = getProperty( QNAME_PROP_SOURCE_DATE, XMLGregorianCalendar.class );
    return DateUtilities.toDate( date );
  }

  /**
   * @see org.kalypso.model.flood.binding.ITinReference#setMax(java.math.BigDecimal)
   */
  @Override
  public void setMax( final BigDecimal max )
  {
    setProperty( QNAME_PROP_MAX, max );
  }

  /**
   * @see org.kalypso.model.flood.binding.ITinReference#setMin(java.math.BigDecimal)
   */
  @Override
  public void setMin( final BigDecimal min )
  {
    setProperty( QNAME_PROP_MIN, min );
  }

  /**
   * @see org.kalypso.model.flood.binding.ITinReference#setSourceLocation(java.net.URL)
   */
  @Override
  public void setSourceLocation( final URL location )
  {
    setProperty( QNAME_PROP_SOURCE_LOCATION, location.toExternalForm() );
  }

  /**
   * @see org.kalypso.model.flood.binding.ITinReference#setTin(org.kalypsodeegree.model.geometry.GM_TriangulatedSurface)
   */
  @Override
  public void setTin( final GM_TriangulatedSurface surface )
  {
    setProperty( QNAME_PROP_TIN, surface );
  }

  /**
   * @see org.kalypso.model.flood.binding.ITinReference#setUpdateDate(java.util.Date)
   */
  @Override
  public void setUpdateDate( final Date date )
  {
    setProperty( QNAME_PROP_SOURCE_DATE, DateUtilities.toXMLGregorianCalendar( date ) );
  }

  /**
   * @see org.kalypso.model.flood.binding.ITinReference#getRunoffEvent()
   */
  @Override
  public IRunoffEvent getRunoffEvent( )
  {
    final Feature parent = getFeature().getParent();
    if( parent == null )
      return null;

    return (IRunoffEvent) parent.getAdapter( IRunoffEvent.class );
  }

  /**
   * @see org.kalypso.model.flood.binding.ITinReference#getSourceType()
   */
  @Override
  public SOURCETYPE getSourceType( )
  {
    final String value = (String) getFeature().getProperty( QNAME_PROP_SOURCE_TYPE );
    return SOURCETYPE.valueOf( value );
  }

  /**
   * @see org.kalypso.model.flood.binding.ITinReference#setSourceType(org.kalypso.model.flood.binding.ITinReference.SOURCETYPE)
   */
  @Override
  public void setSourceType( final SOURCETYPE type )
  {
    getFeature().setProperty( QNAME_PROP_SOURCE_TYPE, type.name() );
  }

  /**
   * @see org.kalypso.model.flood.binding.ITinReference#getTraingle(org.kalypsodeegree.model.geometry.GM_Position)
   */
  @Override
  public GM_Triangle getTraingle( final GM_Position pos )
  {
    final GM_TriangulatedSurface tin = getTin();

    return tin.getTriangle( pos );

  }

}
